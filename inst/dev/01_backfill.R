#!/usr/bin/env Rscript
# Staged, resumable, budgeted backfill.
#
# A full archive crawl of the active surface (~1.2M posts) would take ~4 days at
# a polite 2 s/request, so this never attempts one. It walks forums in priority
# order under an explicit page and thread budget, records where it stopped in
# `crawl_cursor`, and can be re-run any number of times to go deeper.
#
#   OKCP_CUTOFF      earliest last_post_at to bother with    (default 2026-06-01)
#   OKCP_PAGES       listing pages per forum per run         (default 4)
#   OKCP_THREADS     threads parsed per run (global budget)  (default 60)
#   OKCP_THREAD_PAGES post pages per thread per run          (default 40)
#   OKCP_OVERLAP     posts to re-read before the resume point (default 15)
source("R/db.R"); source("R/fetch.R"); source("R/forums.R")
source("R/scrape.R"); source("R/persist.R"); source("R/taxonomy.R"); source("R/sieve_run.R")
suppressMessages({library(dplyr); library(DBI)})

CUTOFF   <- as.Date(Sys.getenv("OKCP_CUTOFF", "2026-06-01"))
PAGES    <- as.integer(Sys.getenv("OKCP_PAGES", "4"))
BUDGET   <- as.integer(Sys.getenv("OKCP_THREADS", "60"))
# Caps how deep a single megathread can be walked in one run. A discussion
# thread with thousands of posts is then absorbed over several runs instead of
# starving every other thread in the queue.
PAGES_PER_THREAD <- as.integer(Sys.getenv("OKCP_THREAD_PAGES", "40"))
OVERLAP  <- as.integer(Sys.getenv("OKCP_OVERLAP", "15"))
batch    <- paste0("backfill-", format(Sys.time(), "%Y%m%d-%H%M%S"))

con <- db_connect(); db_init(con)
fx <- dbGetQuery(con, "SELECT forum_id, name FROM forums WHERE active ORDER BY priority, forum_id")

np <- ne <- err <- 0L; nthreads <- 0L; nforums <- 0L
for (i in seq_len(nrow(fx))) {
  if (nthreads >= BUDGET) break
  fid <- fx$forum_id[i]
  cur <- dbGetQuery(con, sprintf("SELECT * FROM crawl_cursor WHERE forum_id = %d", fid))
  if (nrow(cur) && isTRUE(cur$complete)) { message("f=", fid, " complete, skipping"); next }
  start <- if (nrow(cur)) as.integer(cur$next_start) else 0L

  message(sprintf("\n== f=%d %s :: listing from start=%d (budget %d pages)",
                  fid, fx$name[i], start, PAGES))
  w <- tryCatch(walk_listing(fid, cutoff = CUTOFF, start = start, max_pages = PAGES),
                error = function(e) { message("  listing ERR: ", conditionMessage(e)); NULL })
  if (is.null(w)) { err <- err + 1L; next }
  nforums <- nforums + 1L
  message(sprintf("   %d topics over %d pages; complete=%s", nrow(w$threads), w$pages, w$complete))

  if (nrow(w$threads)) {
    db_upsert(con, "threads", w$threads |>
                transmute(t_id, forum_id = as.integer(fid), title, listing_replies,
                          last_post_at) |> as.data.frame(), "t_id")

    # Parse threads we have not fully captured. listing_replies is the site's
    # own count, so a thread that has grown since last capture comes back.
    todo <- dbGetQuery(con, sprintf("
      SELECT t_id, title, listing_replies, posts_captured, article_id
        FROM threads
       WHERE forum_id = %d AND last_post_at >= DATE '%s'
         AND (last_scraped IS NULL
              OR COALESCE(listing_replies, 0) + 1 > COALESCE(posts_captured, 0))
       ORDER BY last_post_at DESC", fid, format(CUTOFF)))
    todo <- head(todo, max(0L, BUDGET - nthreads))
    message(sprintf("   parsing %d thread(s)", nrow(todo)))

    for (j in seq_len(nrow(todo))) {
      # Resume where the last capture stopped, backing off OVERLAP posts so an
      # edited or late-inserted post near the tail is still re-read. Without
      # this a 3,000-post megathread costs 200 page fetches on every run.
      resume <- max(0L, as.integer(coalesce(todo$posts_captured[j], 0L)) - OVERLAP)
      r <- tryCatch({
        p <- parse_thread(todo$t_id[j], forum_id = fid,
                          article_id = todo$article_id[j],
                          max_pages = PAGES_PER_THREAD, start_at = resume)
        s <- persist_thread(con, p, todo$t_id[j], fid, batch, title = todo$title[j])
        np <- np + s$posts; ne <- ne + s$edges
        TRUE
      }, error = function(e) {
        err <<- err + 1L
        message("   ERR t=", sprintf("%.0f", todo$t_id[j]), ": ", conditionMessage(e)); FALSE })
      nthreads <- nthreads + 1L
      if (nthreads >= BUDGET) { message("   thread budget reached"); break }
    }
  }

  db_upsert(con, "crawl_cursor", data.frame(
    forum_id = fid, next_start = as.integer(w$next_start),
    oldest_seen = if (is.null(w$oldest_seen)) NA else w$oldest_seen,
    complete = w$complete, updated_at = Sys.time()), "forum_id")
}

sieve_new_posts(con)
db_log(con, batch, "backfill", nforums, nthreads, np, 0L, err,
       sprintf("cutoff %s, pages %d, thread budget %d", CUTOFF, PAGES, BUDGET))

cat(sprintf("\nDONE backfill: %d forums, %d threads, %d posts, %d edges, %d errors\n",
            nforums, nthreads, np, ne, err))
print(dbGetQuery(con, "SELECT forum_id, next_start, complete FROM crawl_cursor ORDER BY forum_id"))
dbDisconnect(con, shutdown = TRUE)
