# scrape.R — phpBB thread/post parser + news-article resolution.
# DOM verified against forums.castanet.net (phpBB) for drought-sna, June 2026;
# re-verified 2026-08-03.
suppressMessages({library(rvest); library(xml2); library(stringr); library(dplyr); library(tibble)})

# ---- THREAD: walk ?start= pages, return posts + actors + reply edges ---------
#
# `start_at` resumes partway into a thread instead of walking from page 0.
# This is essential for the discussion forums: f=23 alone holds 416k posts
# across 9,210 topics, and its active threads are long-running megathreads.
# Re-walking one from the top on every daily run is O(thread size) per run and
# takes hours. The caller passes the count already captured, minus a small
# overlap so edited or newly-inserted posts near the tail are still picked up.
parse_thread <- function(t_id, forum_id = NA_integer_, article_id = NA_real_,
                         max_pages = 200L, start_at = 0L) {
  base <- sprintf("https://forums.castanet.net/viewtopic.php?t=%.0f", t_id)
  all_posts <- list(); all_edges <- list(); start <- as.integer(start_at)
  seq0 <- start; pages <- 0L
  seen <- numeric(0)
  art  <- article_id
  repeat {
    url  <- if (start == 0) base else sprintf("%s&start=%d", base, start)
    html <- tryCatch(fetch_html(url), error = function(e) NULL)
    if (is.null(html)) break
    pdiv <- html |> html_elements("div.post")
    if (!length(pdiv)) break

    # Resolve the underlying news article once, from the first page fetched.
    # Castanet links the article from the thread body; the id is the trailing
    # number in /news/<Section>/<id>/<slug>. drought-sna never did this, so
    # every post there carries a NULL article_id.
    if (start == start_at && is.na(art)) art <- article_id_from_page(html)

    parsed <- lapply(seq_along(pdiv), function(i)
      parse_post(pdiv[[i]], t_id, forum_id, art, seq0 + i))
    pids   <- vapply(parsed, function(p) p$post$post_id, numeric(1))
    fresh  <- !(pids %in% seen)
    # phpBB clamps `start` past the end and re-serves the last page: stop when
    # a page adds no new post ids. Do NOT stop on "fewer than 15 rows" — page
    # size is not constant across forums.
    if (!any(fresh)) break
    parsed <- parsed[fresh]
    seen   <- c(seen, pids[fresh])
    all_posts <- c(all_posts, lapply(parsed, `[[`, "post"))
    all_edges <- c(all_edges, lapply(parsed, `[[`, "edges"))
    seq0  <- seq0 + length(parsed)
    start <- start + length(pdiv)
    # Count pages fetched this call — not absolute offset, which would make
    # max_pages meaningless once start_at is non-zero.
    pages <- pages + 1L
    if (pages >= max_pages) break
  }
  posts <- bind_rows(all_posts)
  edges <- bind_rows(all_edges)
  actors <- if (nrow(posts)) {
    posts |> distinct(user_id = author_user_id, handle, join_date,
                      total_posts_at_capture, is_staff)
  } else tibble()
  list(posts = posts, actors = actors, edges = edges, article_id = art)
}

# The article id behind a news-comment thread, read off the first in-body link
# to /news/<Section>/<id>/.
article_id_from_page <- function(html) {
  hrefs <- html |> html_elements("div.content a, .postbody a") |> html_attr("href")
  hrefs <- hrefs[!is.na(hrefs)]
  m <- str_match(hrefs, "castanet\\.net/news/[^/]+/([0-9]+)/")[, 2]
  m <- m[!is.na(m)]
  if (length(m)) as.numeric(m[1]) else NA_real_
}

# ---- ARTICLE page: section / headline / publish time ------------------------
parse_article <- function(url) {
  html <- fetch_html(url)
  links <- html |> html_elements("a") |> html_attr("href")
  links <- links[!is.na(links)]
  tlink <- links[str_detect(links, "viewtopic\\.php\\?t=") & !str_detect(links, "posting")]
  pub <- html |> html_element("meta[property='article:published_time']") |> html_attr("content")
  tibble(
    article_id       = as.numeric(str_match(url, "/news/[^/]+/([0-9]+)/")[, 2]),
    section          = str_match(url, "/news/([^/]+)/")[, 2],
    headline         = html |> html_element("h1") |> html_text2() |> str_squish(),
    url              = url,
    published_at     = suppressWarnings(lubridate::ymd_hms(pub, quiet = TRUE)),
    comment_thread_t = if (length(tlink)) as.numeric(id_from(clean_url(tlink[1]), "t")) else NA_real_
  )
}

# ---- single post -> post row + outgoing quote-reply edges -------------------
parse_post <- function(node, t_id, forum_id, article_id, seq_n) {
  post_id <- node |> html_attr("id") |> str_remove("^p") |> as.numeric()
  prof    <- node |> html_element(".postprofile")
  prof_a  <- prof |> html_elements("a") |> html_attr("href")
  user_id <- if (length(prof_a)) as.numeric(id_from(clean_url(prof_a[1]), "u")) else NA_real_
  handle  <- prof |> html_element("a.username, a.username-coloured") |> html_text2()
  prof_tx <- prof |> html_text2() |> str_squish()
  npost   <- as.integer(str_match(prof_tx, "Posts:\\s*([0-9]+)")[, 2])
  jdate   <- str_match(prof_tx, "Joined:\\s*([A-Za-z]+ [0-9]+[a-z]+, [0-9]{4})")[, 2]
  jdate   <- suppressWarnings(lubridate::mdy(str_remove(jdate, "(st|nd|rd|th)")))
  # Absolute UTC from the datetime attribute — NOT the relative display text
  # ("3 hours ago"), which is unusable for temporal network analysis.
  posted  <- node |> html_element("time") |> html_attr("datetime")
  posted  <- suppressWarnings(lubridate::ymd_hms(posted, quiet = TRUE))
  staff   <- str_detect(coalesce(handle, ""), "(?i)castanet") ||
             str_detect(prof_tx, "(?i)administrator|moderator")

  body <- node |> html_element(".content") |> html_text2() |> str_squish()

  # Outgoing edges: each blockquote cite names BOTH the quoted user (u=) and
  # the exact quoted post (p=), so from->to is unambiguous. Quotes are the only
  # reply signal in a flat phpBB thread.
  cites <- node |> html_elements("blockquote cite")
  edges <- lapply(cites, function(c) {
    hrefs   <- c |> html_elements("a") |> html_attr("href")
    hrefs   <- hrefs[!is.na(hrefs)]
    to_user <- as.numeric(id_from(clean_url(hrefs[str_detect(hrefs, "memberlist")][1]), "u"))
    to_post <- as.numeric(id_from(clean_url(hrefs[str_detect(hrefs, "p=")][1]), "p"))
    tibble(
      from_user_id = user_id, to_user_id = to_user,
      from_post_id = post_id, to_post_id = to_post,
      thread_t = t_id, forum_id = forum_id, posted_at = posted,
      edge_type = "quote_reply", weight = 1.0,
      evidence = c |> html_text2() |> str_squish() |> str_trunc(160)
    )
  }) |> bind_rows()
  if (nrow(edges)) edges <- edges |> filter(!is.na(to_user_id) | !is.na(to_post_id))
  if (nrow(edges)) edges$edge_id <- with(edges, sprintf(
    "%.0f|%.0f|%s", from_post_id, coalesce(to_post_id, 0), edge_type))

  post <- tibble(
    post_id = post_id, thread_t = t_id, forum_id = forum_id, article_id = article_id,
    author_user_id = user_id, posted_at = posted, seq_in_thread = as.integer(seq_n),
    quote_count = nrow(edges), n_chars = nchar(coalesce(body, "")),
    body_local = body,
    handle = handle, join_date = jdate, total_posts_at_capture = npost, is_staff = staff
  )
  list(post = post, edges = edges)
}
