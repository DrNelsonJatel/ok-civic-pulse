#!/usr/bin/env Rscript
# Daily incremental: re-scrape recently-active threads, sieve, classify, and
# rebuild the metrics rollup. Cheap by design — thanks to the resume offset it
# reads a few pages per active thread rather than re-walking whole megathreads.
source("R/db.R"); source("R/fetch.R"); source("R/forums.R"); source("R/scrape.R")
source("R/persist.R"); source("R/taxonomy.R"); source("R/sieve_run.R")
source("R/classify.R"); source("R/metrics.R")
suppressMessages({library(dplyr); library(DBI)})

LOOKBACK <- as.integer(Sys.getenv("OKCP_LOOKBACK_DAYS", "3"))
OVERLAP  <- as.integer(Sys.getenv("OKCP_OVERLAP", "15"))
PAGES_PER_THREAD <- as.integer(Sys.getenv("OKCP_THREAD_PAGES", "40"))
DO_CLASSIFY <- !identical(Sys.getenv("OKCP_SKIP_CLASSIFY"), "1")
cutoff <- Sys.Date() - LOOKBACK
batch  <- paste0("daily-", format(Sys.time(), "%Y%m%d-%H%M%S"))

con <- db_connect(); db_init(con)
fx  <- dbGetQuery(con, "SELECT forum_id, name FROM forums WHERE active ORDER BY priority, forum_id")

np <- ne <- err <- 0L; nthreads <- 0L
for (i in seq_len(nrow(fx))) {
  fid <- fx$forum_id[i]
  # Daily runs walk the listing from the top — recently-active topics bubble to
  # page 1, so the crawl_cursor (a backfill concept) is deliberately not used.
  w <- tryCatch(walk_listing(fid, cutoff = cutoff, start = 0L, max_pages = 5L),
                error = function(e) { message("  listing ERR f=", fid, ": ", conditionMessage(e)); NULL })
  if (is.null(w) || !nrow(w$threads)) { if (is.null(w)) err <- err + 1L; next }

  db_upsert(con, "threads", w$threads |>
              transmute(t_id, forum_id = as.integer(fid), title,
                        listing_replies, last_post_at) |> as.data.frame(), "t_id")

  todo <- dbGetQuery(con, sprintf("
    SELECT t_id, title, posts_captured, article_id
      FROM threads
     WHERE forum_id = %d AND last_post_at >= DATE '%s'
       AND (last_scraped IS NULL
            OR COALESCE(listing_replies, 0) + 1 > COALESCE(posts_captured, 0))
     ORDER BY last_post_at DESC", fid, format(cutoff)))
  message(sprintf("f=%d %s: %d active thread(s)", fid, fx$name[i], nrow(todo)))

  for (j in seq_len(nrow(todo))) {
    tryCatch({
      resume <- max(0L, as.integer(coalesce(todo$posts_captured[j], 0L)) - OVERLAP)
      p <- parse_thread(todo$t_id[j], forum_id = fid, article_id = todo$article_id[j],
                        max_pages = PAGES_PER_THREAD, start_at = resume)
      s <- persist_thread(con, p, todo$t_id[j], fid, batch, title = todo$title[j])
      np <- np + s$posts; ne <- ne + s$edges; nthreads <- nthreads + 1L
    }, error = function(e) {
      err <<- err + 1L
      message("  ERR t=", sprintf("%.0f", todo$t_id[j]), ": ", conditionMessage(e)) })
  }
}

sieve_new_posts(con)
if (DO_CLASSIFY) classify_new(con) else message("classify: skipped (OKCP_SKIP_CLASSIFY=1)")
rebuild_issue_daily(con)

db_log(con, batch, "daily", nrow(fx), nthreads, np, 0L, err,
       sprintf("lookback %dd", LOOKBACK))
cat(sprintf("DONE daily: %d threads, %d posts, %d edges, %d errors\n",
            nthreads, np, ne, err))
dbDisconnect(con, shutdown = TRUE)
