-- ok-civic-pulse :: DuckDB schema
-- Okanagan civic discourse monitor — Castanet phpBB forums, municipal
-- sphere-of-influence issue tracking ahead of the 2026 BC general local
-- election (voting day 2026-10-17).
--
-- DESIGN NOTE (copyright / privacy). Personal research project.
-- Castanet's ToU prohibits redistribution of content, so `posts.body_local`
-- holds comment text for LOCAL analysis only. The DuckDB file (db/) is
-- git-ignored. Everything exported or published (output/snapshots/, reports)
-- is de-texted: issue codes, counts, network structure. No verbatim comment
-- text and no handles leave this machine.
--
-- Fixes two defects carried over from drought-sna:
--   1. `articles` was never written -> article_id always NULL.
--   2. thread titles were discovered, used for filtering, then discarded.
-- Here `threads` is a first-class table and every post resolves to a thread.

----------------------------------------------------------------------
-- SOURCES : every corpus this project ingests, meta-tagged at the row level.
--
-- Sources are not interchangeable. A phpBB forum yields stable pseudonymous
-- IDs and resolvable quote-reply edges; a Reddit subreddit yields a different
-- demographic and threaded replies; a council correspondence package yields
-- text that is in-scope by construction. Recording which source a row came
-- from is what makes it possible to ask, later, WHICH SOURCES ARE ACTUALLY
-- WORTH INGESTING — see source_daily below.
----------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS sources (
    source_id     VARCHAR PRIMARY KEY,  -- 'castanet_forums', 'reddit_kelowna', ...
    name          VARCHAR,
    platform      VARCHAR,              -- 'phpbb' | 'reddit' | 'escribe' | 'engagementhq'
    url           VARCHAR,
    region        VARCHAR,
    access        VARCHAR,              -- 'open' | 'oauth_free' | 'licensed'
    cost_note     VARCHAR,              -- what it costs, in plain language
    robots_note   VARCHAR,              -- the posture we verified, and when
    has_reply_edges BOOLEAN,            -- can this source support network analysis at all?
    active        BOOLEAN DEFAULT TRUE,
    added_at      TIMESTAMP DEFAULT now()
);

----------------------------------------------------------------------
-- SOURCE_DAILY : the value of each source over time.
--
-- Volume is the least interesting column here. The ones that decide whether a
-- source earns its keep are `pct_local` (how much of what it produces is
-- actually within local-government scope), `novel_issues` (issues this source
-- surfaced before any other did), and `hhi` (whether its apparent activity is
-- a handful of voices).
----------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS source_daily (
    day            DATE,
    source_id      VARCHAR,
    n_posts        INTEGER,
    n_actors       INTEGER,
    n_coded        INTEGER,
    n_local        INTEGER,
    pct_local      DOUBLE,
    hhi            DOUBLE,
    mean_confidence DOUBLE,
    n_issues       INTEGER,
    novel_issues   INTEGER,
    PRIMARY KEY (day, source_id)
);

----------------------------------------------------------------------
-- FORUMS : the crawl surface for phpBB sources. Registry, not a scrape artifact.
----------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS forums (
    forum_id     INTEGER PRIMARY KEY,   -- phpBB f= id
    name         VARCHAR,
    kind         VARCHAR,               -- 'news_comments' | 'discussion'
    region       VARCHAR,               -- 'central_ok' | 'north_ok' | 'south_ok' | 'bc' | 'general'
    priority     INTEGER DEFAULT 5,     -- 1 = crawl first; used to stage the backfill
    active       BOOLEAN DEFAULT TRUE,  -- include in daily incremental
    n_topics     INTEGER,               -- from the index page, for crawl budgeting
    n_posts      INTEGER,
    last_indexed TIMESTAMP
);

----------------------------------------------------------------------
-- THREADS : one phpBB topic. For f=134 a topic == one news article's
-- comment section, and `title` IS the headline.
----------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS threads (
    t_id           BIGINT PRIMARY KEY,   -- phpBB topic id
    source_id      VARCHAR DEFAULT 'castanet_forums',
    forum_id       INTEGER,
    title          VARCHAR,             -- headline for news comments
    article_id     BIGINT,              -- resolved Castanet news id, when linked
    listing_replies INTEGER,            -- reply count shown on the forum listing
    first_post_at  TIMESTAMP,
    last_post_at   TIMESTAMP,           -- drives the daily lookback
    posts_captured INTEGER DEFAULT 0,
    is_water       BOOLEAN,             -- legacy drought-sna sieve, kept for continuity
    first_seen     TIMESTAMP DEFAULT now(),
    last_scraped   TIMESTAMP
);

----------------------------------------------------------------------
-- ARTICLES : news items behind f=134 threads. Populated by resolving the
-- article link off the thread's first post / the article page itself.
----------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS articles (
    article_id       BIGINT PRIMARY KEY,
    section          VARCHAR,           -- kelowna / vernon / penticton / bc / ...
    headline         VARCHAR,
    url              VARCHAR,
    published_at     TIMESTAMP,
    comment_thread_t BIGINT,
    first_seen       TIMESTAMP DEFAULT now()
);

----------------------------------------------------------------------
-- ACTORS : persistent pseudonymous commenters = stable network nodes.
-- Handles are quasi-identifiers. Actor-level detail is INTERNAL ONLY and
-- never appears in an exported snapshot or a rendered report.
----------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS actors (
    actor_key              VARCHAR PRIMARY KEY,  -- '<source>:<native id>'
    source_id              VARCHAR DEFAULT 'castanet_forums',
    user_id                BIGINT,               -- native numeric id (phpBB u=)
    handle                 VARCHAR,
    join_date              DATE,
    total_posts_at_capture INTEGER,
    is_staff               BOOLEAN DEFAULT FALSE,
    first_seen_in_corpus   TIMESTAMP DEFAULT now(),
    last_seen_in_corpus    TIMESTAMP
);

----------------------------------------------------------------------
-- POSTS : the unit of analysis. body_local is LOCAL-ONLY text.
----------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS posts (
    post_id        BIGINT PRIMARY KEY,
    source_id      VARCHAR DEFAULT 'castanet_forums',  -- meta-tag: which corpus
    actor_key      VARCHAR,             -- '<source>:<native id>' - cross-source identity
    native_id      VARCHAR,             -- the source's own post id, as a string
    thread_t       BIGINT,
    forum_id       INTEGER,
    article_id     BIGINT,
    author_user_id BIGINT,
    posted_at      TIMESTAMP,           -- absolute UTC from <time datetime>
    seq_in_thread  INTEGER,
    quote_count    INTEGER,
    n_chars        INTEGER,
    body_local     VARCHAR,             -- NEVER exported
    scrape_batch   VARCHAR,
    last_seen      TIMESTAMP
);

----------------------------------------------------------------------
-- EDGES_REPLY : directed replier -> quoted author. phpBB threads are flat;
-- the blockquote cite is the only unambiguous reply signal, and it resolves
-- BOTH the quoted user (u=) and the exact quoted post (p=).
----------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS edges_reply (
    edge_id      VARCHAR PRIMARY KEY,
    source_id    VARCHAR DEFAULT 'castanet_forums',
    from_actor_key VARCHAR,
    to_actor_key   VARCHAR,
    from_user_id BIGINT,
    to_user_id   BIGINT,
    from_post_id BIGINT,
    to_post_id   BIGINT,
    thread_t     BIGINT,
    forum_id     INTEGER,
    posted_at    TIMESTAMP,
    edge_type    VARCHAR DEFAULT 'quote_reply',
    weight       DOUBLE  DEFAULT 1.0,
    evidence     VARCHAR              -- quoted snippet, LOCAL-ONLY
);

----------------------------------------------------------------------
-- POST_ISSUES : many-to-many. One comment can raise several issues.
-- `scope` is the core product judgement: is this something a council can
-- actually act on, or is it a provincial/federal grievance aimed at city hall?
----------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS post_issues (
    post_id      BIGINT,
    issue_code   VARCHAR,              -- see R/taxonomy.R
    scope        VARCHAR,              -- 'local' | 'shared' | 'provincial' | 'federal' | 'none'
    jurisdiction VARCHAR,              -- kelowna / west_kelowna / vernon / penticton / rdco / ...
    stance       VARCHAR,              -- 'support' | 'oppose' | 'mixed' | 'neutral'
    salience     DOUBLE,               -- 0-1, how central the issue is to the comment
    confidence   DOUBLE,
    coder_id     VARCHAR,              -- 'keyword-sieve' | 'claude-<model>' | human login
    coded_at     TIMESTAMP DEFAULT now(),
    PRIMARY KEY (post_id, issue_code, coder_id)
);

-- Polarity plus NRC emotion. Emotion matters more than polarity for civic
-- discourse: "angry about potholes" and "afraid of the wildfire interface" are
-- both negative and call for completely different responses.
CREATE TABLE IF NOT EXISTS post_sentiment (
    post_id   BIGINT PRIMARY KEY,
    sentiment DOUBLE,
    sd        DOUBLE,
    n_words   INTEGER,
    anger        DOUBLE,
    anticipation DOUBLE,
    disgust      DOUBLE,
    fear         DOUBLE,
    joy          DOUBLE,
    sadness      DOUBLE,
    surprise     DOUBLE,
    trust        DOUBLE,
    method    VARCHAR,
    scored_at TIMESTAMP DEFAULT now()
);

----------------------------------------------------------------------
-- CODEBOOK GOVERNANCE
--
-- The codebook is the project's main intellectual asset, so it is versioned
-- data (codebook/codebook.yml), not a literal buried in code. Every coded row
-- records the codebook version that produced it, so a definition change never
-- silently rewrites the meaning of history.
----------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS codebook_version (
    version     VARCHAR PRIMARY KEY,   -- content hash of the YAML
    label       VARCHAR,               -- human tag, e.g. 'v3 split housing'
    n_codes     INTEGER,
    adopted_at  TIMESTAMP DEFAULT now(),
    notes       VARCHAR
);

-- Reviewer notes and proposed changes. This is the comment/adjust surface:
-- observations accumulate against a code over time and become the evidence
-- for the next codebook revision.
CREATE TABLE IF NOT EXISTS codebook_review (
    review_id   VARCHAR PRIMARY KEY,
    issue_code  VARCHAR,
    version     VARCHAR,               -- codebook version being commented on
    kind        VARCHAR,               -- 'note' | 'split' | 'merge' | 'redefine' | 'new' | 'retire'
    note        VARCHAR,
    proposed    VARCHAR,               -- proposed new definition / seeds, if any
    status      VARCHAR DEFAULT 'open',-- 'open' | 'accepted' | 'rejected'
    author      VARCHAR,
    created_at  TIMESTAMP DEFAULT now()
);

----------------------------------------------------------------------
-- ISSUE_DAILY : the rollup the dashboard, timeline and PDF all read.
-- Rebuilt by R/metrics.R; safe to drop and recompute.
----------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS issue_daily (
    day             DATE,
    issue_code      VARCHAR,
    jurisdiction    VARCHAR,
    scope           VARCHAR,
    n_posts         INTEGER,
    n_actors        INTEGER,
    n_threads       INTEGER,
    hhi             DOUBLE,            -- posts-per-actor concentration: groundswell vs 3 loud voices
    mean_sentiment  DOUBLE,
    n_reply_edges   INTEGER,
    contested       DOUBLE,            -- share of reply edges crossing stance
    PRIMARY KEY (day, issue_code, jurisdiction)
);

----------------------------------------------------------------------
-- AUDIT_SAMPLE : which posts were drawn for the high-tier audit, and from
-- which stratum. Bulk coding runs on a cheap model; this is how its accuracy
-- gets MEASURED instead of assumed. Strata deliberately include posts the
-- gate SKIPPED, so the false-negative rate is estimated rather than hoped for.
----------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS audit_sample (
    post_id   BIGINT PRIMARY KEY,
    stratum   VARCHAR,            -- 'gated_in' | 'skipped_out_of_scope' | 'skipped_no_code'
    drawn_at  TIMESTAMP DEFAULT now(),
    sample_id VARCHAR             -- which audit round
);

----------------------------------------------------------------------
-- Crawl bookkeeping: resumable backfill + honest provenance.
----------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS crawl_cursor (
    forum_id   INTEGER PRIMARY KEY,
    next_start INTEGER DEFAULT 0,      -- phpBB ?start= offset to resume the listing walk
    oldest_seen TIMESTAMP,             -- oldest last_post_at reached so far
    complete   BOOLEAN DEFAULT FALSE,  -- reached the configured cutoff
    updated_at TIMESTAMP
);

CREATE TABLE IF NOT EXISTS scrape_log (
    batch_id      VARCHAR PRIMARY KEY,
    run_at        TIMESTAMP,
    mode          VARCHAR,             -- 'index' | 'discover' | 'backfill' | 'daily'
    forums_seen   INTEGER,
    threads_seen  INTEGER,
    posts_new     INTEGER,
    posts_updated INTEGER,
    errors        INTEGER,
    notes         VARCHAR
);
