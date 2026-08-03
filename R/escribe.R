# escribe.R — City of Kelowna council agendas via eScribe.
#
# WHY THIS SOURCE MATTERS MORE THAN COMMENT SECTIONS: an agenda item is what
# council is actually deciding on. Every item is in-scope by construction, so
# this corpus has none of the world-news noise that dominates Castanet. Running
# the same codebook over both produces the comparison that makes the project
# publishable: what residents complain about vs what council is actually
# working on, and where those two diverge.
#
# ACCESS (verified 2026-08-03): robots.txt on kelownapublishing.escribemeetings.com
# blocks only PetalBot — this host is fully open. It is a public-record
# publishing system for a municipality, not a copyrighted content business, so
# the redistribution constraint that binds Castanet does not apply here.
#
# ENDPOINTS (reverse-engineered 2026-08-03, all confirmed working):
#   POST /MeetingsCalendarView.aspx/GetCalendarMeetings
#        body {"calendarStartDate":"YYYY-MM-DD","calendarEndDate":"YYYY-MM-DD"}
#        -> {"d":[ {ID (guid), MeetingName, StartDate, HasAgenda,
#                   AllowPublicComments, MeetingDocumentLink[...] }, ... ]}
#   GET  /Meeting.aspx?Id=<guid>&Agenda=Agenda&lang=English   -> HTML agenda
#   GET  /FileStream.ashx?DocumentId=<int>                    -> PDF
#
# The calendar page itself is ASP.NET WebForms behind __doPostBack and is
# useless to scrape; the FullCalendar JSON endpoint above is the real index.
suppressMessages({library(httr2); library(jsonlite); library(rvest);
                  library(stringr); library(dplyr); library(DBI)})

ESCRIBE_BASE  <- Sys.getenv("OKCP_ESCRIBE_BASE", "https://kelownapublishing.escribemeetings.com")
ESCRIBE_SOURCE <- "kelowna_escribe"
ESCRIBE_DELAY <- as.numeric(Sys.getenv("OKCP_ESCRIBE_DELAY", "2.0"))

escribe_req <- function(path) {
  Sys.sleep(ESCRIBE_DELAY)
  request(paste0(ESCRIBE_BASE, path)) |>
    req_user_agent(UA) |>
    req_retry(max_tries = 3, backoff = \(i) 2^i) |>
    req_timeout(60)
}

# ---- the meeting index ------------------------------------------------------
escribe_meetings <- function(from = Sys.Date() - 90, to = Sys.Date() + 30) {
  r <- escribe_req("/MeetingsCalendarView.aspx/GetCalendarMeetings") |>
    req_headers(`Content-Type` = "application/json") |>
    req_body_raw(sprintf('{"calendarStartDate":"%s","calendarEndDate":"%s"}',
                         format(as.Date(from)), format(as.Date(to)))) |>
    req_perform() |> resp_body_json()
  rows <- r$d
  if (!length(rows)) return(tibble::tibble())
  bind_rows(lapply(rows, function(x) tibble::tibble(
    meeting_id   = x$ID %||% NA_character_,
    meeting_name = x$MeetingName %||% NA_character_,
    meeting_type = x$MeetingType %||% NA_character_,
    # StartDate comes back as "2026/06/23 16:00:00"
    starts_at    = suppressWarnings(as.POSIXct(x$StartDate, format = "%Y/%m/%d %H:%M:%S", tz = "UTC")),
    has_agenda   = isTRUE(x$HasAgenda),
    public_comments = isTRUE(x$AllowPublicComments),
    # Typed document ids. The TYPE matters: 'PostMinutes' is the one that
    # carries public-hearing testimony, 'Agenda' is only the item list.
    doc_ids = paste(na.omit(vapply(x$MeetingDocumentLink %||% list(), function(d) {
      u <- d$Url %||% ""
      m <- str_match(u, "DocumentId=(\\d+)")[, 2]
      if (is.na(m)) NA_character_ else m
    }, character(1))), collapse = ","),
    minutes_doc_id = {
      dl <- x$MeetingDocumentLink %||% list()
      hit <- Filter(function(d) identical(d$Type, "PostMinutes") &&
                                identical(d$Format, ".pdf"), dl)
      if (length(hit)) str_match(hit[[1]]$Url %||% "", "DocumentId=(\\d+)")[, 2] else NA_character_
    }
  ))) |> filter(!is.na(meeting_id)) |> arrange(desc(starts_at))
}

# ---- agenda items -----------------------------------------------------------
#
# One agenda item = one unit of analysis. There is no author, so these rows
# carry no actor and contribute no reply edges — which is exactly why the
# source registry records has_reply_edges = FALSE for this source.
escribe_agenda_items <- function(meeting_id) {
  html <- escribe_req(sprintf("/Meeting.aspx?Id=%s&Agenda=Agenda&lang=English", meeting_id)) |>
    req_perform() |> resp_body_html()
  items <- html |> html_elements("div.AgendaItem")
  if (!length(items)) return(tibble::tibble())
  bind_rows(lapply(seq_along(items), function(i) {
    it   <- items[[i]]
    ttl  <- it |> html_element(".AgendaItemTitle") |> html_text2() |> str_squish()
    desc <- it |> html_element(".AgendaItemDescription") |> html_text2() |> str_squish()
    cat_ <- it |> html_element(".AgendaItemCategory") |> html_text2() |> str_squish()
    atts <- it |> html_elements("a") |> html_attr("href")
    atts <- na.omit(str_match(atts, "DocumentId=(\\d+)")[, 2])
    tibble::tibble(
      meeting_id = meeting_id, seq = i,
      category = cat_, title = ttl,
      description = if (is.na(desc)) "" else desc,
      n_attachments = length(atts),
      attachment_ids = paste(atts, collapse = ","))
  })) |> filter(!is.na(title), nzchar(title))
}

# Deterministic synthetic post_id. eScribe has no integer item id, so hash the
# (meeting, sequence) pair — stable across re-runs so upserts are idempotent
# rather than duplicating the agenda on every crawl.
#
# strtoi() CANNOT be used here: it returns a 32-bit integer and silently yields
# NA above 2^31, which is every 12-hex-digit hash. Accumulate into a double
# instead — doubles represent integers exactly up to 2^53, comfortably above
# the 2^48 ceiling of 12 hex digits, so the ids stay collision-resistant and
# exact.
.hex2num <- function(h) {
  d <- match(strsplit(tolower(h), "")[[1]], c(0:9, letters[1:6])) - 1
  Reduce(function(acc, x) acc * 16 + x, d, 0)
}

.escribe_pid <- function(meeting_id, seq) {
  vapply(seq_along(meeting_id), function(i)
    .hex2num(substr(digest::digest(paste(meeting_id[i], seq[i])), 1, 12)),
    numeric(1))
}

# ---- ingest -----------------------------------------------------------------
ingest_escribe <- function(con, from = Sys.Date() - 90, to = Sys.Date() + 30,
                           max_meetings = 40L,
                           batch_id = paste0("escribe-", format(Sys.time(), "%Y%m%d-%H%M%S"))) {
  suppressMessages(library(digest))
  mt <- escribe_meetings(from, to)
  if (!nrow(mt)) { message("escribe: no meetings in range"); return(invisible(0L)) }
  mt <- mt |> filter(has_agenda) |> head(max_meetings)
  message(sprintf("escribe: %d meeting(s) with agendas, %s to %s",
                  nrow(mt), format(as.Date(from)), format(as.Date(to))))

  # The meeting itself is stored as a "thread" so agenda items hang off it the
  # same way comments hang off a forum topic — one shape for every source.
  db_upsert(con, "threads", mt |>
    transmute(t_id = .escribe_pid(meeting_id, 0L),
              source_id = ESCRIBE_SOURCE, forum_id = NA_integer_,
              title = paste(meeting_name, format(as.Date(starts_at))),
              article_id = NA_real_, listing_replies = NA_integer_,
              first_post_at = starts_at, last_post_at = starts_at,
              last_scraped = Sys.time()) |> as.data.frame(), "t_id")

  n_items <- 0L
  for (i in seq_len(nrow(mt))) {
    it <- tryCatch(escribe_agenda_items(mt$meeting_id[i]), error = function(e) {
      message("  ERR ", mt$meeting_id[i], ": ", conditionMessage(e)); tibble::tibble() })
    if (!nrow(it)) next
    body <- str_squish(paste(it$category, it$title, it$description))
    prow <- tibble::tibble(
      post_id = .escribe_pid(it$meeting_id, it$seq),
      source_id = ESCRIBE_SOURCE,
      # No author: an agenda item is an institutional artifact, not a person.
      actor_key = NA_character_,
      native_id = paste0(it$meeting_id, "#", it$seq),
      thread_t = .escribe_pid(mt$meeting_id[i], 0L),
      forum_id = NA_integer_, article_id = NA_real_, author_user_id = NA_real_,
      posted_at = mt$starts_at[i], seq_in_thread = it$seq,
      quote_count = it$n_attachments, n_chars = nchar(body),
      body_local = body, scrape_batch = batch_id, last_seen = Sys.time())
    db_upsert(con, "posts", as.data.frame(prow), "post_id",
              null_safe = setdiff(names(prow), c("post_id","scrape_batch","last_seen")))
    n_items <- n_items + nrow(prow)
    message(sprintf("  %s %s: %d items",
                    format(as.Date(mt$starts_at[i])), mt$meeting_name[i], nrow(prow)))
  }
  message(sprintf("escribe: %d agenda items ingested", n_items))
  invisible(n_items)
}

`%||%` <- function(a, b) if (is.null(a)) b else a
