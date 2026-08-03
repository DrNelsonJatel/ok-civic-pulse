# forums.R — crawl surface: the forum registry and the topic-listing walk.
suppressMessages({library(rvest); library(stringr); library(dplyr); library(tibble)})

# ---- registry ---------------------------------------------------------------
# Which forums matter for municipal-election discourse, and in what order the
# backfill should eat them. Sizes are filled in live by index_forums(); these
# are the editorial decisions (region + priority + active).
#
# Crawl budget context (live index, 2026-08-03): f=134 alone is 55,090 topics /
# 331,316 posts ~= 22k page fetches ~= 12 h at 2 s. f=23 is 416k posts. A full
# archive crawl is not on the table — this is forward-only daily ingest plus a
# bounded historical window, staged by priority.
FORUM_REGISTRY <- tribble(
  ~forum_id, ~name,                          ~kind,            ~region,      ~priority, ~active,
  134L,      "Castanet News Comments",       "news_comments",  "general",    1L,        TRUE,
  23L,       "Central Okanagan",             "discussion",     "central_ok", 1L,        TRUE,
  104L,      "North Okanagan",               "discussion",     "north_ok",   2L,        TRUE,
  110L,      "South Okanagan",               "discussion",     "south_ok",   2L,        TRUE,
  31L,       "Social Concerns",              "discussion",     "general",    3L,        TRUE,
  40L,       "Trials & Tribulations of Traffic", "discussion",  "general",    3L,        TRUE,
  136L,      "Fire Watch",                   "discussion",     "general",    3L,        TRUE,
  26L,       "B.C.",                         "discussion",     "bc",         4L,        TRUE,
  # Kamloops discussion lives here, not on a separate castanetkamloops forum
  # (that host does not resolve). Small - 93 topics / 939 posts - but free.
  125L,      "Kamloops",                     "discussion",     "kamloops",   4L,        TRUE,
  # Registered but not crawled: off-topic or out of the municipal frame.
  27L,       "Canada",                       "discussion",     "national",   9L,        FALSE,
  28L,       "World",                        "discussion",     "world",      9L,        FALSE,
  95L,       "Health",                       "discussion",     "general",    9L,        FALSE,
  52L,       "Conspiracies and Weird Science","discussion",    "general",    9L,        FALSE,
  78L,       "Archives",                     "discussion",     "general",    9L,        FALSE
)

# ---- live index: topic/post counts per forum, for crawl budgeting ------------
index_forums <- function() {
  html <- fetch_html("https://forums.castanet.net/")
  rows <- html |> html_elements("li.row")
  out <- lapply(rows, function(r) {
    a <- r |> html_element("a.forumtitle")
    if (length(a) == 0 || is.na(a)) return(NULL)
    fid <- id_from(clean_url(coalesce(html_attr(a, "href"), "")), "f")
    if (is.na(fid)) return(NULL)
    nums <- r |> html_elements(".topics, .posts") |> html_text2() |>
      str_extract("[0-9,]+") |> str_remove_all(",") |> as.integer()
    tibble(forum_id = fid,
           name     = str_squish(html_text2(a)),
           n_topics = if (length(nums) >= 1) nums[1] else NA_integer_,
           n_posts  = if (length(nums) >= 2) nums[2] else NA_integer_)
  })
  bind_rows(out) |> filter(!is.na(forum_id)) |> distinct(forum_id, .keep_all = TRUE)
}

# ---- topic listing walk -----------------------------------------------------
# Walk viewforum.php?f=&start= newest-first until `cutoff`, `max_pages`, or the
# end of the forum.
#
# Two phpBB traps this guards against:
#   * pinned/global topics carry stale dates and repeat on EVERY page — they
#     must not drive the stop decision, or the walk halts on page 1.
#   * when `start` overshoots the end, phpBB clamps and re-serves the last page
#     forever. Stopping on "no new topic ids" is the only reliable terminator;
#     row-count heuristics are not, because page size varies by forum.
walk_listing <- function(forum_id, cutoff, start = 0L, max_pages = 500L) {
  out <- list(); page <- 0L; seen <- integer(0); oldest <- NULL; complete <- FALSE
  repeat {
    url  <- sprintf("https://forums.castanet.net/viewforum.php?f=%d&start=%d", forum_id, start)
    html <- tryCatch(fetch_html(url), error = function(e) NULL)
    if (is.null(html)) break
    rows <- html |> html_elements("li.row, .topiclist .row")
    if (!length(rows)) { complete <- TRUE; break }

    df <- bind_rows(lapply(rows, function(r) {
      a    <- r |> html_element("a.topictitle")
      if (length(a) == 0 || is.na(a)) return(NULL)
      href <- clean_url(coalesce(html_attr(a, "href"), ""))
      tm   <- r |> html_elements("time") |> html_attr("datetime")
      rep  <- r |> html_element(".posts") |> html_text2() |>
        str_extract("[0-9,]+") |> str_remove_all(",") |> as.integer()
      tibble(
        t_id  = as.numeric(id_from(href, "t")),
        title = str_squish(html_text2(a)),
        listing_replies = rep,
        last_post_at = suppressWarnings(lubridate::ymd_hms(tail(tm, 1), quiet = TRUE)),
        sticky = grepl("global|announce|sticky", coalesce(html_attr(r, "class"), ""),
                       ignore.case = TRUE)
      )
    }))
    df <- if (is.null(df) || !nrow(df)) tibble() else filter(df, !is.na(t_id), !sticky)

    fresh <- if (nrow(df)) df[!(df$t_id %in% seen), , drop = FALSE] else df
    if (!nrow(fresh)) { complete <- TRUE; break }   # clamped re-serve == end of forum
    seen <- c(seen, fresh$t_id)
    out[[length(out) + 1]] <- fresh
    page <- page + 1L

    # Advance the cursor BEFORE the budget check. If the page budget breaks the
    # loop first, next_start still points at the page just consumed, so a
    # resumed backfill re-crawls it forever and never advances.
    start <- start + nrow(fresh)

    pg_oldest <- suppressWarnings(min(fresh$last_post_at, na.rm = TRUE))
    if (is.finite(pg_oldest)) oldest <- pg_oldest
    if (is.finite(pg_oldest) && as.Date(pg_oldest) < cutoff) { complete <- TRUE; break }
    if (page >= max_pages) break                    # budget hit, NOT complete
  }
  res <- bind_rows(out)
  if (nrow(res)) res <- res |> distinct(t_id, .keep_all = TRUE) |> select(-sticky)
  list(threads = res, next_start = start, oldest_seen = oldest,
       complete = complete, pages = page)
}
