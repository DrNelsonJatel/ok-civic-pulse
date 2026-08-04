# metrics.R — daily issue rollup.
#
# Reads MODEL/HUMAN codes only. The keyword sieve (coder_id 'keyword-sieve') is
# a recall filter, not a finding: `governance_process` alone fires on any
# comment containing "council" or "election", so including sieve rows would
# massively overstate local-scope volume.
#
# CODER PRECEDENCE. A corpus can carry codes from several coders at once — a
# model upgrade mid-run leaves some posts on the old model, and adjudicated
# posts carry a human code alongside the model's. Averaging across coders
# silently blends classifiers of different quality and makes a figure that
# corresponds to no actual measurement. So exactly ONE code set is used per
# post, by this precedence:
#
#   human:*  >  claude:claude-opus-5  >  claude:claude-sonnet-5  >  everything else
#
# This is what made the Haiku->Opus 5 transition safe: partially re-coded
# corpora report on the best available coder per post rather than a mixture.
suppressMessages({library(dplyr); library(DBI)})

CODER_PRECEDENCE <- c("human:", "claude:claude-opus-5", "claude:claude-sonnet-5",
                      "claude:claude-haiku-4-5")

# SQL fragment selecting the winning coder for each post.
preferred_codes_sql <- function(alias = "i") {
  ranks <- paste(vapply(seq_along(CODER_PRECEDENCE), function(k)
    sprintf("WHEN coder_id LIKE '%s%%' THEN %d", CODER_PRECEDENCE[k], k),
    character(1)), collapse = " ")
  sprintf("
    SELECT * FROM (
      SELECT *, dense_rank() OVER (
                  PARTITION BY post_id
                  ORDER BY CASE %s ELSE 99 END) AS coder_rank
        FROM post_issues WHERE coder_id <> 'keyword-sieve')
     WHERE coder_rank = 1", ranks)
}

# Which coder actually supplied each post's codes, for provenance reporting.
coder_mix <- function(con) {
  dbGetQuery(con, sprintf("
    SELECT coder_id, count(DISTINCT post_id) AS posts
      FROM (%s) GROUP BY 1 ORDER BY 2 DESC", preferred_codes_sql()))
}

# Herfindahl index over posts-per-actor. This is the metric that separates a
# genuine groundswell from three people posting forty times each: 1/n_actors
# means everyone contributed equally, 1.0 means a single voice.
hhi <- function(counts) {
  if (!length(counts)) return(NA_real_)
  s <- sum(counts); if (s == 0) return(NA_real_)
  sum((counts / s)^2)
}

rebuild_issue_daily <- function(con) {
  coded <- dbGetQuery(con, sprintf("
    SELECT i.post_id, i.issue_code, i.scope, i.jurisdiction, i.stance,
           i.salience, i.confidence,
           p.author_user_id, p.thread_t, CAST(p.posted_at AS DATE) AS day
      FROM (%s) i
      JOIN posts p ON p.post_id = i.post_id
     WHERE p.posted_at IS NOT NULL", preferred_codes_sql()))
  if (!nrow(coded)) { message("metrics: no model/human codes yet"); return(invisible(0L)) }

  # Reply edges attributed to an issue via the replying post's own codes.
  edges <- dbGetQuery(con, "
    SELECT e.edge_id, e.from_post_id, e.to_post_id,
           CAST(e.posted_at AS DATE) AS day, i.issue_code, i.jurisdiction
      FROM edges_reply e
      JOIN post_issues i ON i.post_id = e.from_post_id
     WHERE i.coder_id <> 'keyword-sieve' AND e.posted_at IS NOT NULL")

  # Contestedness: share of an issue's reply edges that cross a stance boundary
  # (support <-> oppose). Neutral/mixed edges don't count either way.
  stance <- coded |> select(post_id, issue_code, stance) |> distinct()
  contested <- edges |>
    left_join(stance, by = c("from_post_id" = "post_id", "issue_code")) |>
    rename(from_stance = stance) |>
    left_join(stance, by = c("to_post_id" = "post_id", "issue_code")) |>
    rename(to_stance = stance) |>
    filter(!is.na(from_stance), !is.na(to_stance)) |>
    mutate(crosses = (from_stance == "support" & to_stance == "oppose") |
                     (from_stance == "oppose"  & to_stance == "support")) |>
    group_by(day, issue_code, jurisdiction) |>
    summarise(n_edge_stanced = n(), n_cross = sum(crosses), .groups = "drop") |>
    mutate(contested = n_cross / pmax(1, n_edge_stanced))

  edge_n <- edges |> count(day, issue_code, jurisdiction, name = "n_reply_edges")

  sent <- dbGetQuery(con, "SELECT post_id, sentiment FROM post_sentiment")

  daily <- coded |>
    left_join(sent, by = "post_id") |>
    group_by(day, issue_code, jurisdiction, scope) |>
    summarise(
      n_posts   = n_distinct(post_id),
      n_actors  = n_distinct(author_user_id),
      n_threads = n_distinct(thread_t),
      hhi       = hhi(as.integer(table(author_user_id))),
      mean_sentiment = if (all(is.na(sentiment))) NA_real_ else mean(sentiment, na.rm = TRUE),
      .groups = "drop") |>
    left_join(edge_n,    by = c("day", "issue_code", "jurisdiction")) |>
    left_join(select(contested, day, issue_code, jurisdiction, contested),
              by = c("day", "issue_code", "jurisdiction")) |>
    mutate(n_reply_edges = coalesce(n_reply_edges, 0L),
           # jurisdiction is part of the primary key, so NA has to become a
           # real value or the upsert silently drops those rows.
           jurisdiction  = coalesce(jurisdiction, "unspecified"))

  dbExecute(con, "DELETE FROM issue_daily")
  db_upsert(con, "issue_daily", as.data.frame(daily),
            c("day", "issue_code", "jurisdiction"))
  message(sprintf("metrics: issue_daily rebuilt — %d rows, %s to %s",
                  nrow(daily), min(daily$day), max(daily$day)))
  invisible(nrow(daily))
}

# Velocity + breadth over a trailing window: the "what's rising" table the
# dashboard and the PDF both lead with.
issue_momentum <- function(con, window = 7L, jurisdictions = NULL) {
  d <- dbGetQuery(con, "SELECT * FROM issue_daily")
  if (!nrow(d)) return(tibble::tibble())
  if (!is.null(jurisdictions)) d <- filter(d, jurisdiction %in% jurisdictions)
  latest <- max(d$day)
  cur  <- filter(d, day >  latest - window)
  prev <- filter(d, day <= latest - window, day > latest - 2 * window)

  # Concentration MUST be computed over the whole window from post-level data.
  # Averaging the per-day HHI is badly wrong on sparse issues: a day with a
  # single comment has HHI = 1.0 by definition, so an issue with seven comments
  # spread over seven days averaged to 0.81 — reading as "one voice dominates"
  # when six different people were talking.
  win_hhi <- dbGetQuery(con, sprintf("
    SELECT i.issue_code, p.author_user_id, count(*) AS n
      FROM post_issues i JOIN posts p ON p.post_id = i.post_id
     WHERE i.coder_id <> 'keyword-sieve' AND i.issue_code <> 'none'
       AND p.author_user_id IS NOT NULL
       AND CAST(p.posted_at AS DATE) > DATE '%s'
     GROUP BY 1, 2", format(latest - window))) |>
    group_by(issue_code) |>
    summarise(hhi = hhi(n), n_actors_window = n(), .groups = "drop")

  agg <- function(x) x |> group_by(issue_code, scope) |>
    summarise(posts = sum(n_posts), actors = sum(n_actors),
              edges = sum(n_reply_edges),
              sentiment = weighted.mean(mean_sentiment, n_posts, na.rm = TRUE),
              contested = weighted.mean(contested, pmax(n_reply_edges, 1), na.rm = TRUE),
              .groups = "drop")

  a <- agg(cur) |> left_join(win_hhi, by = "issue_code")
  b <- agg(prev) |> select(issue_code, prev_posts = posts)
  a |> left_join(b, by = "issue_code") |>
    mutate(prev_posts = coalesce(prev_posts, 0L),
           # +Inf on a cold start is useless in a ranking; treat a zero base as
           # a new issue and cap the ratio.
           velocity = ifelse(prev_posts == 0, NA_real_, posts / prev_posts),
           is_new   = prev_posts == 0 & posts > 0) |>
    arrange(desc(posts))
}
