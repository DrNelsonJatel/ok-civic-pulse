# persist.R — write a parsed thread into DuckDB.
suppressMessages({library(dplyr); library(DBI)})

persist_thread <- function(con, parsed, t_id, forum_id, batch_id = "adhoc",
                           title = NA_character_) {
  np <- 0L; ne <- 0L
  art <- parsed$article_id

  if (nrow(parsed$actors)) {
    a <- parsed$actors |>
      transmute(user_id, handle, join_date, total_posts_at_capture, is_staff,
                last_seen_in_corpus = Sys.time()) |>
      filter(!is.na(user_id)) |> distinct(user_id, .keep_all = TRUE)
    db_upsert(con, "actors", a, "user_id")
  }

  # Posts are written BEFORE the threads row on purpose: posts_captured is read
  # back from the DB below. A resumed scrape only returns the new tail, so
  # storing nrow(parsed$posts) would reset the counter every run and the next
  # resume would restart from page 0 forever.
  if (nrow(parsed$posts)) {
    p <- parsed$posts |>
      transmute(post_id, thread_t, forum_id = as.integer(forum_id), article_id,
                author_user_id, posted_at, seq_in_thread, quote_count, n_chars,
                body_local, scrape_batch = batch_id, last_seen = Sys.time()) |>
      filter(!is.na(post_id))
    # scrape_batch/last_seen must be last-write-wins so provenance stays honest;
    # everything else is COALESCE-protected against a bad re-parse.
    db_upsert(con, "posts", p, "post_id",
              null_safe = setdiff(names(p), c("post_id", "scrape_batch", "last_seen")))
    np <- nrow(p)
  }

  # Cumulative truth for this thread, straight from the DB.
  agg <- dbGetQuery(con, sprintf(
    "SELECT count(*) n, min(posted_at) mn, max(posted_at) mx
       FROM posts WHERE thread_t = %.0f", t_id))

  # --- threads: the headline lives HERE. drought-sna discovered thread titles,
  # used them for its keyword sieve, then threw them away, leaving a corpus
  # that knew which thread a comment was in but not what it was about.
  db_upsert(con, "threads", tibble::tibble(
    t_id = t_id, forum_id = as.integer(forum_id), title = title,
    article_id = art,
    first_post_at = agg$mn[1], last_post_at = agg$mx[1],
    posts_captured = as.integer(agg$n[1]),
    last_scraped = Sys.time()
  ), "t_id",
  # posts_captured must be last-write-wins: it can legitimately stay flat, and
  # COALESCE-protection would be wrong if a thread were ever re-counted down.
  null_safe = c("title", "article_id", "first_post_at", "last_post_at"))

  # --- articles: minimal row now (id + thread + headline). A separate
  # enrichment pass fills section / url / published_at from the article page,
  # so the thread walk costs no extra fetches.
  if (!is.na(art)) {
    db_upsert(con, "articles", tibble::tibble(
      article_id = art, headline = title, comment_thread_t = t_id
    ), "article_id")
  }

  if (nrow(parsed$edges)) {
    e <- parsed$edges |>
      transmute(edge_id, from_user_id, to_user_id, from_post_id, to_post_id,
                thread_t, forum_id = as.integer(forum_id), posted_at,
                edge_type, weight, evidence) |>
      filter(!is.na(edge_id)) |> distinct(edge_id, .keep_all = TRUE)
    db_upsert(con, "edges_reply", e, "edge_id")
    ne <- nrow(e)
  }
  list(posts = np, edges = ne, article_id = art)
}
