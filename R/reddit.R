# reddit.R — Reddit collector. *** PARKED — DO NOT RUN. ***
#
# ================== READ BEFORE ENABLING ANYTHING HERE ==================
#
# Reddit's Responsible Builder Policy (reviewed 2026-08-03) puts this project's
# analysis on the wrong side of three clauses. Cost was never the blocker.
#
# 1. RESEARCH MUST GO THROUGH THE RFR PROGRAM.
#    "Any research that uses Reddit data collected outside of the RFR Program
#     is in violation of this policy."
#    This IS a research project, so the ordinary script-app route is not an
#    available path for it — regardless of staying inside rate limits.
#
# 2. APPROVAL IS REQUIRED UP FRONT.
#    "You must request access and get explicit approval before accessing any
#     Reddit data through our API." Creating a script app is not that approval.
#    The no-approval clause also covers "commercial and NON-COMMERCIAL mining,
#    scraping" without express written approval.
#
# 3. THE PRIVACY CLAUSE HITS THE CORE METHOD — this is the decisive one.
#    "You are strictly prohibited from processing data to derive or infer
#     potentially sensitive characteristics about Reddit users (e.g. health,
#     POLITICAL AFFILIATION, sexual orientation)."
#    This pipeline codes stance toward policy, aggregates to actor level, builds
#    actor-to-actor reply networks and fits ERGMs on actor attributes. Applied
#    to Reddit users that is political-affiliation inference about identifiable
#    accounts. Coding a single COMMENT's stance might be defensible; the
#    actor-level modelling this project exists to do is not.
#
# CONSEQUENCE: reddit_kelowna / reddit_okanagan are set active = FALSE in
# SOURCE_REGISTRY and nothing calls ingest_subreddit(). The code is retained
# because it is correct and tested, and because a granted RFR application would
# make it usable — under RFR's own data-handling rules (no retention beyond
# immediate need, re-run queries against fresh exports to honour deletions),
# which this schema does NOT currently implement.
#
# To pursue it properly: apply to Reddit for Researchers, and revisit the
# actor-level analysis design before ingesting a single comment.
# ========================================================================
#
# Technical posture (verified 2026-08-03): unauthenticated requests to
# https://www.reddit.com/r/<sub>/about.json return HTTP 403 — OAuth is
# mandatory even to read. Free tier is 100 queries/minute, non-commercial;
# commercial starts ~US$12,000/month with no smaller plan.
#
# Setup, IF AND ONLY IF access is approved: create a "script" app at
# https://www.reddit.com/prefs/apps, then set
#   REDDIT_CLIENT_ID, REDDIT_CLIENT_SECRET, REDDIT_USERNAME, REDDIT_PASSWORD
suppressMessages({library(httr2); library(dplyr); library(DBI)})

REDDIT_UA <- Sys.getenv("REDDIT_UA",
  "macos:ok-civic-pulse:0.1 (personal research; contact njatel@limnology.ca)")
REDDIT_QPM <- as.numeric(Sys.getenv("REDDIT_QPM", "60"))   # under the 100 QPM cap

.reddit_token <- local({
  tok <- NULL; expires <- 0
  function() {
    if (!is.null(tok) && Sys.time() < expires) return(tok)
    id <- Sys.getenv("REDDIT_CLIENT_ID"); sec <- Sys.getenv("REDDIT_CLIENT_SECRET")
    usr <- Sys.getenv("REDDIT_USERNAME"); pwd <- Sys.getenv("REDDIT_PASSWORD")
    if (!nzchar(id) || !nzchar(sec) || !nzchar(usr) || !nzchar(pwd))
      stop("Reddit credentials missing. Set REDDIT_CLIENT_ID / _SECRET / _USERNAME / _PASSWORD.",
           call. = FALSE)
    r <- request("https://www.reddit.com/api/v1/access_token") |>
      req_auth_basic(id, sec) |>
      req_user_agent(REDDIT_UA) |>
      req_body_form(grant_type = "password", username = usr, password = pwd) |>
      req_retry(max_tries = 3) |> req_perform() |> resp_body_json()
    tok <<- r$access_token
    expires <<- Sys.time() + (r$expires_in %||% 3600) - 120
    tok
  }
})

reddit_get <- function(path, query = list()) {
  Sys.sleep(60 / REDDIT_QPM)   # self-throttle below the free-tier ceiling
  request(paste0("https://oauth.reddit.com", path)) |>
    req_auth_bearer_token(.reddit_token()) |>
    req_user_agent(REDDIT_UA) |>
    req_url_query(!!!query) |>
    req_retry(max_tries = 4, backoff = \(i) 2^i) |>
    req_timeout(30) |> req_perform() |> resp_body_json()
}

# Reddit ids are base36 strings ("1abcdef"). The schema keys on BIGINT, so
# convert for the key and keep the original in native_id. Seven base36 chars
# max out around 7.8e10, comfortably inside BIGINT.
b36 <- function(x) {
  chars <- strsplit(tolower(x), "")[[1]]
  digits <- match(chars, c(as.character(0:9), letters))
  sum((digits - 1) * 36^rev(seq_along(digits) - 1))
}

# Walk /r/<sub>/new back to `cutoff`. One call returns up to 100 posts.
reddit_listing <- function(subreddit, cutoff = Sys.Date() - 7, max_pages = 10L) {
  out <- list(); after <- NULL; page <- 0L
  repeat {
    q <- list(limit = 100); if (!is.null(after)) q$after <- after
    r <- reddit_get(sprintf("/r/%s/new", subreddit), q)
    ch <- r$data$children
    if (!length(ch)) break
    df <- bind_rows(lapply(ch, function(x) with(x$data, tibble::tibble(
      native_id = id, fullname = name, title = title,
      created_utc = created_utc, num_comments = num_comments %||% 0L,
      author = author %||% "[deleted]"))))
    out[[length(out) + 1]] <- df
    page <- page + 1L
    oldest <- as.POSIXct(min(df$created_utc), origin = "1970-01-01", tz = "UTC")
    after <- r$data$after
    if (is.null(after) || as.Date(oldest) < cutoff || page >= max_pages) break
  }
  res <- bind_rows(out)
  if (nrow(res)) res <- distinct(res, native_id, .keep_all = TRUE)
  res
}

# Flatten a comment tree. `parent_id` gives the reply target directly, so
# actor->actor edges are resolvable exactly as with phpBB quote-cites — which
# is why Reddit is one of the few sources that can support network analysis.
.flatten <- function(node, acc = new.env(parent = emptyenv())) {
  if (is.null(node$data$children)) return(invisible(NULL))
  for (ch in node$data$children) {
    if (!identical(ch$kind, "t1")) next
    d <- ch$data
    acc$rows[[length(acc$rows) + 1]] <- tibble::tibble(
      native_id = d$id, parent_fullname = d$parent_id %||% NA_character_,
      author = d$author %||% "[deleted]", body = d$body %||% "",
      created_utc = d$created_utc %||% NA_real_)
    if (!is.null(d$replies) && is.list(d$replies)) .flatten(d$replies, acc)
  }
  invisible(NULL)
}

reddit_comments <- function(post_native_id) {
  r <- reddit_get(sprintf("/comments/%s", post_native_id), list(limit = 500, depth = 10))
  acc <- new.env(parent = emptyenv()); acc$rows <- list()
  if (length(r) >= 2) .flatten(r[[2]], acc)
  if (!length(acc$rows)) return(tibble::tibble())
  bind_rows(acc$rows)
}

# Ingest one subreddit into the source-tagged schema.
ingest_subreddit <- function(con, subreddit, source_id = paste0("reddit_", tolower(subreddit)),
                             cutoff = Sys.Date() - 7, max_posts = 100L,
                             batch_id = paste0("reddit-", format(Sys.time(), "%Y%m%d-%H%M%S"))) {
  posts <- reddit_listing(subreddit, cutoff = cutoff)
  if (!nrow(posts)) { message("reddit/", subreddit, ": no posts"); return(invisible(0L)) }
  posts <- head(posts, max_posts)
  message(sprintf("reddit/%s: %d posts, fetching comments...", subreddit, nrow(posts)))

  np <- 0L; ne <- 0L
  for (i in seq_len(nrow(posts))) {
    cm <- tryCatch(reddit_comments(posts$native_id[i]), error = function(e) {
      message("  ERR ", posts$native_id[i], ": ", conditionMessage(e)); tibble::tibble() })
    if (!nrow(cm)) next
    cm <- filter(cm, author != "[deleted]", nzchar(body), body != "[removed]")
    if (!nrow(cm)) next

    actors <- tibble::tibble(
      actor_key = paste0("reddit:", cm$author), source_id = source_id,
      user_id = NA_real_, handle = cm$author, join_date = as.Date(NA),
      total_posts_at_capture = NA_integer_, is_staff = FALSE,
      last_seen_in_corpus = Sys.time()) |> distinct(actor_key, .keep_all = TRUE)
    db_upsert(con, "actors", as.data.frame(actors), "actor_key")

    prow <- tibble::tibble(
      post_id = vapply(cm$native_id, b36, numeric(1)),
      source_id = source_id, actor_key = paste0("reddit:", cm$author),
      native_id = cm$native_id,
      thread_t = b36(posts$native_id[i]), forum_id = NA_integer_, article_id = NA_real_,
      author_user_id = NA_real_,
      posted_at = as.POSIXct(cm$created_utc, origin = "1970-01-01", tz = "UTC"),
      seq_in_thread = seq_len(nrow(cm)), quote_count = 0L,
      n_chars = nchar(cm$body), body_local = cm$body,
      scrape_batch = batch_id, last_seen = Sys.time())
    db_upsert(con, "posts", as.data.frame(prow), "post_id",
              null_safe = setdiff(names(prow), c("post_id","scrape_batch","last_seen")))
    np <- np + nrow(prow)

    # Edges: parent_id "t1_xxx" is a comment (a real actor-to-actor reply);
    # "t3_xxx" is the submission itself, which is a top-level comment, not a
    # reply to a person — those are correctly excluded.
    lookup <- setNames(paste0("reddit:", cm$author), paste0("t1_", cm$native_id))
    ed <- cm |> filter(startsWith(coalesce(parent_fullname, ""), "t1_")) |>
      mutate(to_actor_key = unname(lookup[parent_fullname])) |>
      filter(!is.na(to_actor_key)) |>
      transmute(edge_id = paste0("reddit|", native_id, "|", parent_fullname),
                source_id = source_id,
                from_actor_key = paste0("reddit:", author), to_actor_key,
                from_user_id = NA_real_, to_user_id = NA_real_,
                from_post_id = vapply(native_id, b36, numeric(1)),
                to_post_id = vapply(sub("^t1_", "", parent_fullname), b36, numeric(1)),
                thread_t = b36(posts$native_id[i]), forum_id = NA_integer_,
                posted_at = as.POSIXct(created_utc, origin = "1970-01-01", tz = "UTC"),
                edge_type = "reddit_reply", weight = 1, evidence = NA_character_) |>
      filter(from_actor_key != to_actor_key)   # drop self-replies, as with phpBB
    if (nrow(ed)) { db_upsert(con, "edges_reply", as.data.frame(ed), "edge_id"); ne <- ne + nrow(ed) }
  }
  message(sprintf("reddit/%s: %d comments, %d reply edges", subreddit, np, ne))
  invisible(np)
}

`%||%` <- function(a, b) if (is.null(a)) b else a
