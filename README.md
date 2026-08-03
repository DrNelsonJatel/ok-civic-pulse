# ok-civic-pulse

Personal research project: monitoring Okanagan civic discourse on Castanet's
phpBB forums, and separating the issues a **local government can actually act
on** from the provincial and federal grievances aimed at city hall — ahead of
the BC general local election on **2026-10-17**.

## Why the scope question is the product

Castanet's comment sections are dominated by national and world news. Of
everything residents are angry about, only a fraction sits inside a council's
authority. `R/taxonomy.R` encodes a codebook of BC local-government
competencies (Community Charter / Local Government Act) alongside explicit
out-of-scope categories, and every comment is coded for both the issue and the
level of government that owns it.

`ballot` is a distinct scope from `local`: school trustees and regional
district electoral area directors are not municipal, but their seats *are* on
the same ballot, so they belong in an election product.

## Crawl surface (live index, 2026-08-03)

| Forum | Topics | Posts |
|---|---:|---:|
| f=134 Castanet News Comments | 55,090 | 331,316 |
| f=23 Central Okanagan | 9,210 | 416,051 |
| f=26 B.C. | 3,450 | 177,269 |
| f=31 Social Concerns | 3,842 | 174,851 |
| f=40 Traffic | 1,377 | 47,170 |
| f=110 South Okanagan | 878 | 31,732 |
| f=104 North Okanagan | 689 | 17,286 |
| f=136 Fire Watch | 40 | 386 |

**~1.2M posts across the active set.** A full archive crawl at a polite 2 s/request
would take roughly four days, so this is **forward-only daily ingest plus a
bounded historical window**, staged by forum priority and resumable via
`crawl_cursor`.

## Legal / ethics posture

Personal research, not a commercial product.

- **robots.txt** allows `viewtopic.php` / `viewforum.php` for a generic
  crawler; only `/posting.php` is disallowed. Named AI/SEO bots are blocked, so
  this crawler uses an honest identifying UA and a ≥2 s delay.
- **ToU** has no anti-scraping clause but **prohibits redistribution**, so
  comment text lives only in the git-ignored DuckDB. Everything exported or
  reported is de-texted and aggregate: issue codes, counts, network structure.
  No verbatim comment text and no handles leave this machine.
- Pseudonymous handles are quasi-identifiers. Actor-level detail stays internal
  to the analysis; deliverables are issue-level and network-level.
- If this ever becomes a paid product, the posture has to be revisited before
  anything ships — BC PIPA and, for candidate clients, LECFA contribution rules
  both apply to commercial use in a way they don't here.

## Layout

```
codebook/codebook.yml    THE CODEBOOK — versioned data, content-hashed
sql/schema.sql           DuckDB schema
R/db.R                   connect / init / COALESCE-guarded upsert
R/fetch.R                polite HTTP (honest UA, 2s delay, retry)
R/forums.R               forum registry + resumable topic-listing walk
R/scrape.R               phpBB parser, article resolution, megathread resume
R/persist.R              parsed thread -> DuckDB
R/codebook.R             load / validate / version / health metrics / review notes
R/taxonomy.R             runtime view of the codebook + keyword sieve
R/sieve_run.R            recall filter: which comments deserve a model call
R/classify.R             two-tier Claude classification (bulk + Batch API)
R/audit.R                stratified audit of the cheap tier; Cohen's kappa
R/sentiment.R            sentimentr polarity + NRC emotion (length-normalised)
R/metrics.R              issue_daily rollup, momentum, HHI concentration
R/topics.R               seeded LDA / keyATM from codebook seeds; residual topics
R/trends.R               Kleinberg bursts, changepoints, concentration-guarded momentum
R/scaling.R              LSX domain scaling (replaces failed lexicon sentiment)
R/sna.R                  reply + co-participation networks, small-world, gated ERGM
R/py_viz.R               optional weekly BERTopic / scattertext via reticulate
R/serve.R                de-texted read copy for the dashboard
R/election.R             2026 BC local election calendar
R/theme_okcp.R           plot theme, >=12pt enforced
dashboard/index.qmd      Quarto static dashboard
inst/dev/00_init_db.R    schema + forum registry
inst/dev/01_backfill.R   staged, resumable, budgeted backfill
inst/dev/02_daily.R      daily incremental (pure R, no Python)
inst/dev/03_weekly.R     weekly enrichment (topics, LSX, ERGM, Python visuals)
inst/dev/04_migrate_sources.R  one-off: source meta-tagging migration
inst/dev/05_reddit_check.R     Reddit connection check (PARKED - see below)
inst/dev/06_daily_report.R     render the branded daily brief
inst/dev/07_qaqc.R             QAQC suite; gates CI
```

## Reddit is parked, deliberately

Reddit's Responsible Builder Policy requires research to go through the Reddit
for Researchers programme, requires explicit prior approval, and **prohibits
inferring political affiliation about users** — which is what actor-level
stance modelling and ERGMs on Reddit accounts amount to. The collector in
`R/reddit.R` is correct and tested but is not called, and both Reddit sources
are `active = FALSE` with access `blocked_by_policy`. Cost was never the
blocker.

## The codebook is versioned data

`codebook/codebook.yml` is the project's main asset, not a literal in code.
The version is a **content hash of the semantic fields** (codes, scopes,
definitions, seeds), so editing a label doesn't mint a version but changing
what a code *means* does. Every coded row records the version that produced it,
so a revision never silently rewrites the meaning of already-coded history.

Revision is evidence-driven. `codebook_health()` flags each code as over-used,
under-used, seeds-over-fire, seeds-too-narrow, or low-confidence; unseeded
("residual") topics from the topic model are written straight into
`codebook_review` as candidate new codes. `codebook_note()` records proposals
to split / merge / redefine / retire.

Seed regexes are validated with **the same ICU engine the sieve runs on** —
validating against base R's TRE engine rejects the lookahead/lookbehind several
seeds depend on.

## Daily vs weekly

The **daily** job is pure R + DuckDB: crawl, sieve, classify, metrics, PDF. It
has no Python dependency and nothing in it can be broken by a torch wheel.

The **weekly** job adds topic models, LSX scaling, the ERGM, and the optional
Python visuals. Every step is individually wrapped — a failure is logged and
skipped, never fatal.

## Design decisions worth knowing

**Two networks, not one.** Quote-replies give unambiguous directed edges but are
sparse (~0.3 per post in the comparable drought-sna corpus). A co-participation
projection runs alongside so connectivity claims don't rest on quoting alone.

**ERGM is weekly, not daily, and gated.** Fitting takes minutes to hours and
MCMC degeneracy is common. `fit_ergm()` returns `ok = FALSE` with a reason
rather than printing coefficients from a degenerate model.

**The keyword sieve is not a finding.** Sieve rows are written under
`coder_id = 'keyword-sieve'` purely to decide which comments are worth a model
call. `governance_process` alone fires on any comment containing "council" or
"election", so every metric filters `coder_id <> 'keyword-sieve'`.

**Concentration is reported next to volume.** `hhi` on posts-per-actor
distinguishes a real groundswell from three people posting forty times.

**Megathread resume.** Discussion-forum threads run to thousands of posts.
`parse_thread(start_at=)` resumes from `posts_captured` minus an overlap, so a
daily run costs a few pages instead of re-walking the whole thread.

**DuckDB is single-writer.** The ingest holds an exclusive lock — even a
read-only connection fails while it runs. The dashboard therefore reads a
separate exported copy, never the live ingest file.

## After cloning — do this first

`core.hooksPath` is local config and is NOT cloned, so the disclosure guard
must be enabled by hand:

```bash
git config core.hooksPath .githooks
```

The pre-commit hook blocks any commit containing a `.duckdb` file, a batch
metadata RDS (those embed `body_local`), `output/py_local/`, `.Renviron`, or a
pasted API key. A text leak is the one mistake here that cannot be undone once
pushed, so it fails closed.

## QAQC

```bash
Rscript inst/dev/07_qaqc.R   # exits non-zero on any failure; gates CI
```

Disclosure checks run first and include a real leak test: comment fragments and
non-staff handles are searched for inside the rendered `docs/index.html`. Data
checks report as **skipped** (never as passing) when the uncommitted DuckDB
files are absent, which is the normal state in CI.

## Run

```bash
Rscript inst/dev/00_init_db.R    # schema + forum registry (live sizes)
OKCP_CUTOFF=2026-06-01 OKCP_THREADS=200 Rscript inst/dev/01_backfill.R
```

Re-run the backfill as many times as you like — `crawl_cursor` and
`posts_captured` make it resumable.

Environment: `OKCP_DB`, `OKCP_UA`, `OKCP_DELAY`, `OKCP_CUTOFF`, `OKCP_PAGES`,
`OKCP_THREADS`, `OKCP_THREAD_PAGES`, `OKCP_OVERLAP`, `OKCP_MODEL`,
`OKCP_EFFORT`, `ANTHROPIC_API_KEY`.
