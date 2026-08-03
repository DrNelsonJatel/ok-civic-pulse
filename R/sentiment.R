# sentiment.R — polarity and emotion scoring.
#
# THREE measures, deliberately, because no single one is trustworthy on
# sarcastic forum text:
#
#  1. sentimentr polarity — valence-shifter aware, so it handles negation
#     ("not bad") and amplifiers ("really terrible") that bag-of-words
#     lexicons get backwards.
#  2. syuzhet NRC emotions — anger / fear / trust / sadness etc. For civic
#     discourse this is more informative than polarity: "angry about potholes"
#     and "afraid of the wildfire interface" are both negative, and a council
#     should respond to them completely differently.
#  3. The model's own `stance` field (support / oppose / mixed / neutral),
#     already in post_issues. This is the most reliable of the three because
#     it is judged against the POLICY rather than the mood of the sentence.
#
# The lexicon measures are kept mainly so stance can be CROSS-CHECKED cheaply:
# where lexicon polarity and model stance disagree systematically, that is a
# flag worth inspecting, not a number to publish.
suppressMessages({library(dplyr); library(DBI)})

EMOTIONS <- c("anger","anticipation","disgust","fear","joy","sadness","surprise","trust")

score_sentiment <- function(con, limit = Inf, chunk = 500L) {
  todo <- dbGetQuery(con, paste("
    SELECT post_id, body_local FROM posts
     WHERE body_local IS NOT NULL AND length(body_local) > 5
       AND post_id NOT IN (SELECT post_id FROM post_sentiment)",
    if (is.finite(limit)) paste("LIMIT", as.integer(limit)) else ""))
  if (!nrow(todo)) { message("sentiment: nothing new"); return(invisible(0L)) }
  message(sprintf("sentiment: scoring %d posts", nrow(todo)))

  suppressMessages({library(sentimentr); library(syuzhet)})
  out <- list()
  idx <- split(seq_len(nrow(todo)), ceiling(seq_len(nrow(todo)) / chunk))
  for (k in seq_along(idx)) {
    b <- todo[idx[[k]], , drop = FALSE]
    # sentiment_by returns one row per element_id in input order.
    pol <- sentimentr::sentiment_by(sentimentr::get_sentences(b$body_local))
    emo <- syuzhet::get_nrc_sentiment(b$body_local)
    out[[k]] <- tibble::tibble(
      post_id   = b$post_id,
      sentiment = pol$ave_sentiment[match(seq_len(nrow(b)), pol$element_id)],
      sd        = pol$sd[match(seq_len(nrow(b)), pol$element_id)],
      n_words   = pol$word_count[match(seq_len(nrow(b)), pol$element_id)],
      method    = "sentimentr+nrc", scored_at = Sys.time()
    ) |> bind_cols(as_tibble(emo[, EMOTIONS, drop = FALSE]))
    message(sprintf("  %d/%d chunks", k, length(idx)))
  }
  res <- bind_rows(out)
  db_upsert(con, "post_sentiment", as.data.frame(res), "post_id")
  message(sprintf("sentiment: %d posts scored", nrow(res)))
  invisible(nrow(res))
}

# Sentiment and emotion per issue, restricted to model codes. `scope_filter`
# is what powers the dashboard's local-government view.
issue_sentiment <- function(con, scope_filter = NULL, min_posts = 5L) {
  d <- dbGetQuery(con, "
    SELECT i.issue_code, i.scope, i.stance, s.sentiment, s.n_words,
           s.anger, s.fear, s.trust, s.sadness, s.disgust, s.joy
      FROM post_issues i
      JOIN post_sentiment s ON s.post_id = i.post_id
     WHERE i.coder_id <> 'keyword-sieve' AND i.issue_code <> 'none'")
  if (!nrow(d)) return(tibble::tibble())
  if (!is.null(scope_filter)) d <- filter(d, scope %in% scope_filter)
  # NRC returns RAW COUNTS, which scale with comment length. Reporting them
  # unnormalised is a length artifact, not an emotion result: on the pilot
  # corpus, "oppose" comments looked angrier than neutral ones (1.79 vs 1.11)
  # purely because they are 63% longer. Per-100-words reverses that comparison.
  # Always normalise.
  d <- mutate(d, across(all_of(c("anger","fear","trust","sadness","disgust","joy")),
                        ~ 100 * .x / pmax(n_words, 1)))
  d |> group_by(issue_code, scope) |>
    summarise(n = n(), mean_words = mean(n_words, na.rm = TRUE),
              mean_sentiment = mean(sentiment, na.rm = TRUE),
              anger = mean(anger, na.rm = TRUE), fear = mean(fear, na.rm = TRUE),
              trust = mean(trust, na.rm = TRUE), sadness = mean(sadness, na.rm = TRUE),
              pct_oppose  = mean(stance == "oppose",  na.rm = TRUE),
              pct_support = mean(stance == "support", na.rm = TRUE),
              .groups = "drop") |>
    filter(n >= min_posts) |> arrange(desc(n))
}

# Do the cheap lexicon and the model's stance agree? Reported as a diagnostic,
# not as a headline number.
stance_vs_lexicon <- function(con) {
  d <- dbGetQuery(con, "
    SELECT i.stance, s.sentiment FROM post_issues i
      JOIN post_sentiment s ON s.post_id = i.post_id
     WHERE i.coder_id <> 'keyword-sieve' AND i.stance IN ('support','oppose')")
  if (nrow(d) < 10) return(NULL)
  d |> group_by(stance) |>
    summarise(n = n(), mean_lexicon_sentiment = mean(sentiment, na.rm = TRUE),
              .groups = "drop")
}
