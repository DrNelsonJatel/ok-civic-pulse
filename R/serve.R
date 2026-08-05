# serve.R — export a de-texted read copy for the dashboard.
#
# DuckDB allows a single writer and no concurrent readers across processes: a
# read_only connection fails with a lock error while the ingest is running.
# The app therefore never touches db/civic_pulse.duckdb. This writes a separate
# serve copy that is also DE-TEXTED — no body_local, no evidence, no handles —
# so the file the dashboard reads could not leak comment text even by accident.
suppressMessages({library(DBI); library(dplyr)})

SERVE_PATH <- Sys.getenv("OKCP_SERVE_DB", "db/serve.duckdb")

export_serve_db <- function(con, path = SERVE_PATH) {
  tmp <- paste0(path, ".tmp")
  if (file.exists(tmp)) unlink(tmp)
  out <- dbConnect(duckdb::duckdb(), dbdir = tmp)
  on.exit(dbDisconnect(out, shutdown = TRUE), add = TRUE)

  put <- function(name, sql) {
    df <- dbGetQuery(con, sql)
    dbWriteTable(out, name, df, overwrite = TRUE)
    nrow(df)
  }

  n <- list(
    issue_daily = put("issue_daily", "SELECT * FROM issue_daily"),
    # Actor ids are kept as opaque integers for network structure; handles are
    # dropped entirely so nothing in the serve file identifies a person.
    nodes = put("nodes", "
      SELECT a.user_id, a.join_date, a.is_staff,
             count(p.post_id) AS n_posts,
             count(DISTINCT p.thread_t) AS n_threads
        FROM actors a LEFT JOIN posts p ON p.author_user_id = a.user_id
       GROUP BY 1,2,3"),
    edges = put("edges", "
      SELECT from_user_id, to_user_id, thread_t, forum_id, posted_at
        FROM edges_reply WHERE from_user_id IS NOT NULL AND to_user_id IS NOT NULL"),
    # Thread titles ARE headlines, which are Castanet's editorial content, so
    # the serve copy carries only the id and metadata. The dashboard shows
    # issue codes, never headlines.
    # source_id is required for the cross-source gap analysis; actor_key is the
    # de-identified cross-source identity (never the handle or the name).
    posts_meta = put("posts_meta", "
      SELECT post_id, source_id, actor_key, thread_t, forum_id, author_user_id,
             posted_at, n_chars, quote_count
        FROM posts"),
    post_issues = put("post_issues", "
      SELECT post_id, issue_code, scope, jurisdiction, stance, salience, confidence, coder_id
        FROM post_issues WHERE coder_id <> 'keyword-sieve'"),
    forums = put("forums", "SELECT forum_id, name, kind, region FROM forums"),
    # Sentiment scores are numbers derived from text, not text — safe to ship.
    sentiment = put("sentiment", "
      SELECT post_id, sentiment, n_words, anger, fear, trust, sadness, disgust,
             joy, anticipation, surprise, method
        FROM post_sentiment"),
    # Source accounting: which corpora are actually earning their keep.
    source_daily = put("source_daily", "SELECT * FROM source_daily"),
    sources = put("sources", "
      SELECT source_id, name, platform, url, region, access, cost_note,
             robots_note, has_reply_edges, active FROM sources"),
    scrape_log = put("scrape_log", "SELECT * FROM scrape_log")
  )

  # Weekly analyses are far too slow to recompute at render time (the ERGM
  # takes minutes). The weekly job writes them to output/weekly/; ship the
  # results into the serve copy so the dashboard just reads a table.
  #
  # These MUST be written through `out`, the connection to the temp file.
  # Writing them to `path` — the previous serve copy — put them in the file
  # that the unlink()/rename() below then deletes, so ergm_fit, net_timeline
  # and actor_turnover never reached the shipped database. The failure was
  # silent and worse than a crash: wk() returns the row count of what it wrote,
  # so the run logged "ergm=2, timeline=9, turnover=4" every week while the
  # dashboard's ERGM and longitudinal panels rendered "No ERGM fit yet".
  wk <- function(name, file, build) {
    f <- file.path("output/weekly", file)
    if (!file.exists(f)) return(0L)
    df <- tryCatch(build(readRDS(f)), error = function(e) NULL)
    if (is.null(df) || !nrow(df)) return(0L)
    dbWriteTable(out, name, as.data.frame(df), overwrite = TRUE)
    nrow(df)
  }
  n$ergm <- wk("ergm_fit", "ergm.rds", function(x) {
    if (!isTRUE(x$ok)) return(data.frame(term = NA_character_, estimate = NA_real_,
                                         std_error = NA_real_, p_value = NA_real_,
                                         spec = "suppressed", reason = x$reason,
                                         n = NA_integer_, m = NA_integer_))
    cf <- x$coefs
    data.frame(term = rownames(cf), estimate = cf[[1]], std_error = cf[[2]],
               p_value = cf[[ncol(cf)]], spec = x$spec, reason = NA_character_,
               n = x$n, m = x$m)
  })
  n$timeline <- wk("net_timeline", "net_timeline.rds", function(x) x)
  n$turnover <- wk("actor_turnover", "actor_turnover.rds", function(x) x)

  dbDisconnect(out, shutdown = TRUE)
  on.exit()
  if (file.exists(path)) unlink(path)
  file.rename(tmp, path)

  # Read the shipped file back and confirm every table we counted actually
  # arrived. The counts in `n` are what we INTENDED to write; only this check
  # proves what the dashboard will find. Cheap, and it is exactly the assertion
  # whose absence let the weekly tables vanish unnoticed.
  chk <- dbConnect(duckdb::duckdb(), dbdir = path, read_only = TRUE)
  have <- dbListTables(chk)
  dbDisconnect(chk, shutdown = TRUE)
  want <- names(n)[unlist(n) > 0]
  want <- ifelse(want == "ergm", "ergm_fit",
          ifelse(want == "timeline", "net_timeline",
          ifelse(want == "turnover", "actor_turnover", want)))
  missing <- setdiff(want, have)
  if (length(missing))
    stop("serve db is missing table(s) that were reported as written: ",
         paste(missing, collapse = ", "),
         "\n  The export counted rows it never shipped — do not trust the dashboard.",
         call. = FALSE)

  message(sprintf("serve db written: %s (%s)", path,
                  paste(sprintf("%s=%d", names(n), unlist(n)), collapse = ", ")))
  invisible(path)
}
