# trends.R — longitudinal signal: bursts, changepoints, momentum.
#
# The question this answers is the one a daily product lives or dies on:
# did an issue ACTUALLY spike, or is today just the high end of ordinary
# day-to-day variation? Reporting raw daily counts as "trending" is the single
# easiest way to manufacture false signal.
#
# Kleinberg's burst detection models an event stream as a hidden Markov process
# switching between a base rate and elevated states, so a burst is a state
# change rather than a threshold crossing. That makes it robust to the corpus
# growing over time — which matters here, because ingest volume is still
# ramping and any absolute threshold would fire constantly.
suppressMessages({library(dplyr); library(DBI)})

# Kleinberg bursts for one issue. Returns intervals with a burst level; level 1
# is baseline, level >= 2 is a genuine burst.
issue_bursts <- function(con, issue_code, min_events = 20L, s = 2, gamma = 1) {
  if (!requireNamespace("bursts", quietly = TRUE))
    return(list(ok = FALSE, reason = "bursts not installed"))
  ev <- dbGetQuery(con, sprintf("
    SELECT p.posted_at FROM post_issues i JOIN posts p ON p.post_id = i.post_id
     WHERE i.coder_id <> 'keyword-sieve' AND i.issue_code = '%s'
       AND p.posted_at IS NOT NULL ORDER BY p.posted_at", issue_code))
  if (nrow(ev) < min_events)
    return(list(ok = FALSE, reason = sprintf("only %d events for %s (need %d)",
                                             nrow(ev), issue_code, min_events)))
  # Kleinberg works on inter-arrival gaps, so identical timestamps break it.
  # Jitter within the second rather than dropping duplicate posts.
  t <- as.numeric(ev$posted_at)
  t <- t + seq_along(t) * 1e-6
  b <- tryCatch(bursts::kleinberg(t, s = s, gamma = gamma), error = function(e) e)
  if (inherits(b, "error")) return(list(ok = FALSE, reason = conditionMessage(b)))
  out <- as.data.frame(b) |>
    mutate(issue_code = issue_code,
           start = as.POSIXct(start, origin = "1970-01-01", tz = "UTC"),
           end   = as.POSIXct(end,   origin = "1970-01-01", tz = "UTC")) |>
    filter(level >= 2) |>
    arrange(desc(level), start)
  list(ok = TRUE, bursts = out, n_events = nrow(ev))
}

# Sweep every issue with enough events. This is what generates the daily
# report's "what actually spiked" line.
all_bursts <- function(con, min_events = 20L, scopes = NULL) {
  codes <- dbGetQuery(con, sprintf("
    SELECT issue_code, count(*) n FROM post_issues
     WHERE coder_id <> 'keyword-sieve' AND issue_code <> 'none' %s
     GROUP BY 1 HAVING count(*) >= %d ORDER BY n DESC",
    if (is.null(scopes)) "" else
      sprintf("AND scope IN (%s)", paste(sprintf("'%s'", scopes), collapse = ",")),
    min_events))
  if (!nrow(codes)) return(tibble::tibble())
  res <- lapply(codes$issue_code, function(cd) {
    r <- issue_bursts(con, cd, min_events = min_events)
    if (isTRUE(r$ok) && nrow(r$bursts)) r$bursts else NULL
  })
  bind_rows(res)
}

# Changepoints in an issue's daily volume — "when did this become salient",
# as distinct from "is it spiking right now".
issue_changepoints <- function(con, issue_code, min_days = 30L) {
  if (!requireNamespace("changepoint", quietly = TRUE))
    return(list(ok = FALSE, reason = "changepoint not installed"))
  d <- dbGetQuery(con, sprintf("
    SELECT CAST(p.posted_at AS DATE) day, count(*) n
      FROM post_issues i JOIN posts p ON p.post_id = i.post_id
     WHERE i.coder_id <> 'keyword-sieve' AND i.issue_code = '%s'
       AND p.posted_at IS NOT NULL GROUP BY 1 ORDER BY 1", issue_code))
  if (nrow(d) < min_days)
    return(list(ok = FALSE, reason = sprintf("only %d days of data for %s", nrow(d), issue_code)))
  # Fill gaps: a day with no comments is a zero, not a missing observation.
  full <- data.frame(day = seq(min(d$day), max(d$day), by = "day")) |>
    left_join(d, by = "day") |> mutate(n = coalesce(n, 0L))
  cp <- tryCatch(changepoint::cpt.meanvar(full$n, method = "PELT", penalty = "MBIC"),
                 error = function(e) e)
  if (inherits(cp, "error")) return(list(ok = FALSE, reason = conditionMessage(cp)))
  idx <- changepoint::cpts(cp)
  list(ok = TRUE, dates = full$day[idx], series = full,
       means = changepoint::param.est(cp)$mean)
}

# Week-over-week momentum with a concentration guard. Velocity alone is
# gameable by a single prolific commenter, so the HHI is reported alongside it
# and a spike driven by one voice is labelled as such rather than promoted.
momentum_guarded <- function(con, window = 7L, hhi_flag = 0.35) {
  m <- issue_momentum(con, window = window)
  if (!nrow(m)) return(m)
  m |> mutate(
    driver = case_when(
      is.na(hhi)        ~ "unknown",
      hhi >= hhi_flag   ~ "concentrated - few voices",
      hhi < 0.15        ~ "broad participation",
      TRUE              ~ "mixed"),
    trustworthy_spike = !is.na(velocity) & velocity > 1.5 &
                        !is.na(hhi) & hhi < hhi_flag)
}
