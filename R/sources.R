# sources.R — source registry and per-source value accounting.
#
# The point of meta-tagging every row is to be able to answer, months from now,
# "which of these corpora is actually worth the effort?" Volume is the least
# useful answer. A source earns its place on:
#
#   pct_local     what share of what it produces is inside local-government
#                 scope. Castanet's news forum is dominated by world news; a
#                 council correspondence package is in-scope by construction.
#   novel_issues  issues this source surfaced on a day BEFORE any other source
#                 did. A source that only ever echoes another adds nothing.
#   hhi           whether its apparent activity is a handful of voices.
#   confidence    how confidently the classifier can code its text at all.
#
# Registry entries record the access posture VERIFIED AT A DATE, because these
# change and an unrecorded assumption becomes a legal problem later.
suppressMessages({library(dplyr); library(DBI)})

SOURCE_REGISTRY <- tibble::tribble(
  ~source_id, ~name, ~platform, ~url, ~region, ~access, ~cost_note, ~robots_note, ~has_reply_edges, ~active,

  "castanet_forums", "Castanet News Comments + discussion forums", "phpbb",
  "https://forums.castanet.net", "okanagan", "open",
  "free",
  "verified 2026-08-03: viewtopic/viewforum allowed for generic UA; named AI bots blocked; /posting.php disallowed",
  TRUE, TRUE,

  # CORRECTION 2026-08-03: forums.castanetkamloops.net does NOT resolve. There
  # is no separate Kamloops forum - Kamloops discussion already lives inside
  # forums.castanet.net as f=125 (93 topics / 939 posts). So this is not a new
  # source at all, just a forum to activate. The earlier "doubles your market"
  # claim was wrong.
  "castanet_kamloops", "Castanet Kamloops (= forums.castanet.net f=125)", "phpbb",
  "https://forums.castanet.net/viewforum.php?f=125", "kamloops", "open",
  "free - already reachable via the existing crawler, just activate f=125",
  "verified 2026-08-03: no separate host; f=125 is inside the main forum",
  TRUE, FALSE,

  # PARKED - see the header of R/reddit.R. Reddit's Responsible Builder Policy
  # requires research to go through the RFR programme, requires explicit prior
  # approval, and prohibits inferring political affiliation about users - which
  # is what actor-level stance modelling and ERGMs on Reddit accounts would be.
  "reddit_kelowna", "r/kelowna", "reddit",
  "https://www.reddit.com/r/kelowna", "central_ok", "blocked_by_policy",
  "free tier exists but is NOT an available path: research must go via Reddit for Researchers (RFR)",
  paste("verified 2026-08-03: unauthenticated .json returns HTTP 403;",
        "Responsible Builder Policy bars research outside RFR and bars inferring",
        "political affiliation about users - PARKED pending RFR application"),
  TRUE, FALSE,

  "reddit_okanagan", "r/okanagan", "reddit",
  "https://www.reddit.com/r/okanagan", "okanagan", "blocked_by_policy",
  "same as reddit_kelowna - parked", "same as reddit_kelowna", TRUE, FALSE,

  "kelowna_escribe", "City of Kelowna council agendas, minutes, correspondence", "escribe",
  "https://kelownapublishing.escribemeetings.com", "kelowna", "open",
  "free",
  "verified 2026-08-03: robots.txt blocks only PetalBot - fully open",
  FALSE, TRUE,

  # Platform is SOCIAL PINPOINT, not Granicus EngagementHQ - the moderation
  # page names "Social Pinpoint" as operator and widgets load from
  # mysocialpinpoint.ca. Comments are NOT server-rendered; they arrive by XHR,
  # so this needs endpoint discovery or a headless browser, not a scraper.
  "kelowna_getinvolved", "Get Involved Kelowna (public engagement)", "socialpinpoint",
  "https://getinvolved.kelowna.ca", "kelowna", "open",
  "free - but comments are JS-loaded; needs XHR endpoint mapping",
  paste("verified 2026-08-03: User-agent:* Allow:/ with Crawl-delay 1;",
        "ClaudeBot/GPTBot/CCBot/Google-Extended/Bytespider blocked;",
        "Content-Signal ai-train=no, use=reference - analysis is permitted, training is not"),
  FALSE, TRUE,

  "westkelowna_ourwk", "Engage West Kelowna", "engagementhq",
  "https://www.ourwk.ca", "west_kelowna", "open",
  "free", "not yet verified - VERIFY before first crawl", FALSE, FALSE
)

register_sources <- function(con, registry = SOURCE_REGISTRY) {
  db_upsert(con, "sources", as.data.frame(mutate(registry, added_at = Sys.time())), "source_id")
  message("sources registered: ", nrow(registry),
          " (", sum(registry$active), " active)")
  invisible(nrow(registry))
}

# ---- per-source value, per day ---------------------------------------------
rebuild_source_daily <- function(con) {
  base <- dbGetQuery(con, "
    SELECT p.source_id, CAST(p.posted_at AS DATE) AS day, p.post_id, p.actor_key
      FROM posts p WHERE p.posted_at IS NOT NULL AND p.source_id IS NOT NULL")
  if (!nrow(base)) { message("source_daily: no posts"); return(invisible(0L)) }

  codes <- dbGetQuery(con, "
    SELECT post_id, issue_code, scope, confidence FROM post_issues
     WHERE coder_id <> 'keyword-sieve' AND issue_code <> 'none'")

  # First appearance of each issue, by source. An issue is 'novel' to the
  # source that surfaced it earliest — that is the column that tells you
  # whether a source leads or merely echoes.
  first_seen <- base |> inner_join(codes, by = "post_id") |>
    group_by(issue_code) |>
    slice_min(day, n = 1, with_ties = TRUE) |>
    distinct(issue_code, day, source_id) |>
    mutate(is_novel = TRUE)

  per_actor <- base |> count(source_id, day, actor_key, name = "n")
  conc <- per_actor |> group_by(source_id, day) |>
    summarise(hhi = hhi(n), n_actors = n(), .groups = "drop")

  coded <- base |> inner_join(codes, by = "post_id") |>
    group_by(source_id, day) |>
    summarise(n_coded = n_distinct(post_id),
              n_local = n_distinct(post_id[scope %in% c("local","ballot","shared")]),
              mean_confidence = mean(confidence, na.rm = TRUE),
              n_issues = n_distinct(issue_code), .groups = "drop")

  novel <- first_seen |> count(source_id, day, name = "novel_issues")

  out <- base |> group_by(source_id, day) |>
    summarise(n_posts = n_distinct(post_id), .groups = "drop") |>
    left_join(conc,  by = c("source_id","day")) |>
    left_join(coded, by = c("source_id","day")) |>
    left_join(novel, by = c("source_id","day")) |>
    mutate(across(c(n_coded, n_local, n_issues, novel_issues), ~ coalesce(.x, 0L)),
           pct_local = ifelse(n_coded > 0, n_local / n_coded, NA_real_)) |>
    select(day, source_id, n_posts, n_actors, n_coded, n_local, pct_local,
           hhi, mean_confidence, n_issues, novel_issues)

  dbExecute(con, "DELETE FROM source_daily")
  db_upsert(con, "source_daily", as.data.frame(out), c("day","source_id"))
  message(sprintf("source_daily: %d rows across %d source(s)",
                  nrow(out), n_distinct(out$source_id)))
  invisible(nrow(out))
}

# The scorecard: is this source earning its keep?
source_scorecard <- function(con, since = NULL) {
  d <- dbGetQuery(con, "SELECT * FROM source_daily")
  if (!nrow(d)) return(tibble::tibble())
  if (!is.null(since)) d <- filter(d, as.Date(day) >= as.Date(since))
  s <- dbGetQuery(con, "SELECT source_id, name, platform, access, cost_note, has_reply_edges FROM sources")
  d |> group_by(source_id) |>
    summarise(days_active = n_distinct(day),
              posts = sum(n_posts), actors = sum(n_actors),
              coded = sum(n_coded), local = sum(n_local),
              pct_local = ifelse(sum(n_coded) > 0, sum(n_local)/sum(n_coded), NA_real_),
              mean_hhi = mean(hhi, na.rm = TRUE),
              mean_conf = weighted.mean(mean_confidence, pmax(n_coded,1), na.rm = TRUE),
              novel_issues = sum(novel_issues), .groups = "drop") |>
    left_join(s, by = "source_id") |>
    # Locally-actionable comments per active day is the honest headline: it
    # rewards a source for producing IN-SCOPE material, not merely volume.
    mutate(local_per_day = local / pmax(days_active, 1)) |>
    arrange(desc(local_per_day))
}
