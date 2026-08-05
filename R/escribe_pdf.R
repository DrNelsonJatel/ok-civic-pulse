# escribe_pdf.R — public-hearing testimony from council minutes PDFs.
#
# This is the highest-signal corpus in the project. Everything else is people
# talking ABOUT decisions; minutes record the testimony that fed INTO them —
# who appeared at a public hearing, on which item, and which side they took.
#
# The format is stable and machine-readable (verified against Kelowna minutes,
# 2026-08-03):
#
#     City Clerk invited anyone participating online or in the gallery ...
#     Gallery:
#     Steve Crossford on behalf of Trafalgar Sq. resident
#      - Raised concerns about future impact of property and neighbourhood needs.
#      - Opposed to this application.
#
#     Marty Laznicka, Devonian Ave
#     - Spoke to existing traffic concerns in area.
#     - Opposed to this application.
#
# A speaker line is a non-bullet line inside a Gallery block; the bullets that
# follow belong to it. "No one from the Gallery or Online came forward." marks
# an item with no public testimony — which is itself a finding worth recording.
#
# ---------------------------------------------------------------------------
# PRIVACY — this source is different from every other one.
#
# Castanet handles are pseudonyms. These are REAL NAMES of identifiable
# residents, in a public record. Analysing them is legitimate; republishing
# them is not, and they deserve at least the care given to handles:
#
#   * The name goes in `body_local` and `actors.handle` — both local-only.
#   * `actor_key` is a HASH of the normalised name, so repeat speakers can be
#     linked across hearings (analytically valuable: who shows up repeatedly?)
#     without the serve copy or any export ever carrying the name itself.
#   * R/serve.R already drops `handle` and `body_local`. Do not add them back.
# ---------------------------------------------------------------------------
suppressMessages({library(pdftools); library(stringr); library(dplyr);
                  library(DBI); library(digest)})

# Lines that are page furniture rather than content.
.is_furniture <- function(x) {
  str_detect(x, "^\\s*$") |
  str_detect(x, "^\\s*Page \\d+") |
  str_detect(x, "^\\s*[-_]{5,}\\s*$") |
  str_detect(x, regex("^\\s*(city of kelowna|minutes|regular meeting|public hearing)\\s*$", ignore_case = TRUE))
}

# Bullet glyphs, INCLUDING the Private Use Area block U+F000-U+F0FF.
#
# Kelowna's minutes are not consistent about this and the difference is
# invisible: some meetings bullet with an ASCII "-" (U+002D), others with
# U+F02D. The latter is Word writing a Symbol-font bullet, which maps ASCII
# punctuation into the PUA at 0xF000 + codepoint. pdftools passes it through
# and a terminal renders it as nothing, so the line merely looks indented.
#
# The cost of missing it was total, not partial: statements attached to no
# speaker, n_statements stayed 0, and the `filter(n_statements > 0)` at the end
# of parse_minutes_speakers() then dropped every correctly-identified speaker.
# The 2025-11-18 public hearing parsed as zero speakers while holding eight.
BULLET_RX <- "^\\s*[-•–\\*\u{F000}-\u{F0FF}]\\s*"
.is_bullet <- function(x) str_detect(x, BULLET_RX)

# A speaker line: not a bullet, reasonably short, and looks like a name —
# either "Name, Address" or "Name on behalf of X". Deliberately conservative;
# a false negative loses one speaker, a false positive invents a person.
.is_speaker <- function(x) {
  x <- str_squish(x)
  nchar(x) >= 4 & nchar(x) <= 90 &
    !.is_bullet(x) &
    str_detect(x, "^[A-Z][A-Za-z.'’-]+(\\s+[A-Z][A-Za-z.'’-]+)+") &
    (str_detect(x, ",") | str_detect(x, regex("on behalf of", ignore_case = TRUE))) &
    !str_detect(x, regex("^(staff|council|mayor|councillor|city clerk|gallery)\\b", ignore_case = TRUE))
}

# Kelowna's clerks write both "opposed to THIS application" and "opposed to
# THE application" — the original patterns only matched the former, so three of
# seven explicit stances in the sample scored NA. Determiner and the optional
# verb "s" are both optional now. These are a HINT only: the model-assigned
# stance in post_issues remains the analytic field.
STANCE_PAT <- c(
  oppose  = paste0("opposed to (this|the) application|in opposition|",
                   "against (this|the) application|do(es)? not support"),
  support = paste0("in (favour|support)\\b|supports? (this|the) application|",
                   "supportive of"))

# Leading-whitespace width of a raw (un-squished) line.
.indent <- function(x) nchar(str_extract(x, "^[ \t]*"))

parse_minutes_speakers <- function(txt) {
  lines <- unlist(str_split(txt, "\n"))
  lines <- lines[!.is_furniture(lines)]
  out <- list(); cur <- NULL; in_gallery <- FALSE; item <- NA_character_
  gal_indent <- 0L

  for (ln in lines) {
    s <- str_squish(ln)
    # Indentation has to be read off the RAW line: str_squish() strips it, and
    # it is the only thing distinguishing a speaker from a wrapped statement.
    ind <- .indent(ln)
    # Track the agenda item we are under, e.g. "5.1  START TIME 4:00 PM - ..."
    if (str_detect(s, "^\\d+(\\.\\d+)+\\s+\\S")) item <- str_trunc(s, 160)

    if (str_detect(s, regex("^gallery:?$", ignore_case = TRUE))) {
      in_gallery <- TRUE; gal_indent <- ind; next }
    # A Gallery block ends when Council/Staff/the applicant resume, or the item
    # terminates.
    #
    # "Applicant in Response" MUST be here. Without it the applicant's bullets
    # fall through to the `.is_bullet` branch and are appended to whichever
    # resident spoke last — putting the developer's own words in a neighbour's
    # mouth, with their real name attached. A misattribution is worse than a
    # miss, so this list errs toward closing the block.
    if (in_gallery && str_detect(s, regex(paste0("^(staff|council|mayor|councillor|city clerk|",
                                                 "moved by|carried|termination|applicant|",
                                                 "petitioner|proponent|developer|consultant|",
                                                 "there were no further comments)\\b"),
                                          ignore_case = TRUE))) {
      if (!is.null(cur)) { out[[length(out) + 1]] <- cur; cur <- NULL }
      in_gallery <- FALSE; next
    }
    if (!in_gallery) next

    # A speaker sits at the same indent as the "Gallery:" header; a wrapped
    # continuation of a statement is indented past it. Without this test,
    # "...within the Heritage Conservation Area, causing it to resemble other
    # neighbourhoods" — the second line of somebody else's bullet — satisfies
    # every other speaker rule (capitalised words, a comma) and is recorded as
    # a person named "Heritage Conservation Area", truncating the real
    # speaker's testimony at that point. Measured relative to the header rather
    # than against 0 so an indented Gallery block still parses.
    if (ind <= gal_indent + 1L && .is_speaker(s)) {
      if (!is.null(cur)) out[[length(out) + 1]] <- cur
      nm  <- str_squish(str_split(s, ",")[[1]][1])
      nm  <- str_squish(str_replace(nm, regex("on behalf of.*", ignore_case = TRUE), ""))
      aff <- str_squish(str_replace(s, stringr::fixed(nm), ""))
      cur <- list(agenda_item = item, speaker = nm,
                  affiliation = str_replace(aff, "^,\\s*", ""), statements = character())
    } else if (.is_bullet(s) && !is.null(cur)) {
      cur$statements <- c(cur$statements, str_squish(str_remove(s, BULLET_RX)))
    }
  }
  if (!is.null(cur)) out[[length(out) + 1]] <- cur
  if (!length(out)) return(tibble::tibble())

  bind_rows(lapply(out, function(x) {
    body <- paste(x$statements, collapse = " ")
    tibble::tibble(agenda_item = x$agenda_item %||% NA_character_,
                   speaker = x$speaker, affiliation = x$affiliation,
                   n_statements = length(x$statements), body = body,
                   stance_hint = dplyr::case_when(
                     str_detect(body, regex(STANCE_PAT[["oppose"]],  ignore_case = TRUE)) ~ "oppose",
                     str_detect(body, regex(STANCE_PAT[["support"]], ignore_case = TRUE)) ~ "support",
                     TRUE ~ NA_character_))
  })) |> filter(n_statements > 0)
}

# Items where the clerk invited comment and nobody spoke. Recording these
# prevents a silent denominator error: without them you cannot tell an item
# with no testimony from an item that was never opened to the public.
count_silent_items <- function(txt) {
  length(str_extract_all(txt, regex("No one from the Gallery or Online came forward",
                                    ignore_case = TRUE))[[1]])
}

# PDF fetches MUST go through escribe_req(), not download.file().
#
# download.file() has no retry and no backoff: a single 429 makes it write a
# zero-length file and raise, which ingest_escribe_minutes() catches and logs
# as "ERR doc NNNN" before moving on. The meeting is then silently absent from
# the testimony corpus with no way to tell it apart from a meeting that simply
# had no speakers. Every minutes PDF lost in the first backfill was lost this
# way. escribe_req() carries the same 429 handling as the JSON and HTML calls.
fetch_minutes_text <- function(doc_id) {
  f <- tempfile(fileext = ".pdf")
  on.exit(unlink(f), add = TRUE)
  resp <- escribe_req(sprintf("/FileStream.ashx?DocumentId=%s", doc_id)) |>
    req_perform()
  writeBin(resp_body_raw(resp), f)
  if (file.size(f) < 1000)
    stop("minutes PDF for doc ", doc_id, " is ", file.size(f), " bytes — not a PDF")
  paste(pdftools::pdf_text(f), collapse = "\n")
}

# ---- ingest -----------------------------------------------------------------
ingest_escribe_minutes <- function(con, from = Sys.Date() - 120, to = Sys.Date(),
                                   max_meetings = 25L,
                                   batch_id = paste0("escmin-", format(Sys.time(), "%Y%m%d-%H%M%S"))) {
  mt <- escribe_meetings(from, to) |> filter(!is.na(minutes_doc_id)) |> head(max_meetings)
  if (!nrow(mt)) { message("escribe minutes: none in range"); return(invisible(0L)) }
  message(sprintf("escribe minutes: %d meeting(s) with published minutes", nrow(mt)))

  n_sp <- 0L; n_silent <- 0L
  for (i in seq_len(nrow(mt))) {
    txt <- tryCatch(fetch_minutes_text(mt$minutes_doc_id[i]), error = function(e) {
      message("  ERR doc ", mt$minutes_doc_id[i], ": ", conditionMessage(e)); NULL })
    if (is.null(txt)) next
    n_silent <- n_silent + count_silent_items(txt)
    sp <- parse_minutes_speakers(txt)
    if (!nrow(sp)) {
      message(sprintf("  %s %s: no gallery speakers",
                      format(as.Date(mt$starts_at[i])), mt$meeting_name[i]))
      next
    }

    # actor_key hashes the name; the name itself stays in local-only columns.
    keys <- paste0("escribe:", vapply(tolower(str_squish(sp$speaker)),
                                      function(z) substr(digest::digest(z), 1, 12), character(1)))
    db_upsert(con, "actors", data.frame(
      actor_key = keys, source_id = ESCRIBE_SOURCE, user_id = NA_real_,
      handle = sp$speaker, join_date = as.Date(NA),
      total_posts_at_capture = NA_integer_, is_staff = FALSE,
      last_seen_in_corpus = Sys.time(), stringsAsFactors = FALSE) |>
        distinct(actor_key, .keep_all = TRUE), "actor_key")

    body <- str_squish(paste(sp$affiliation, sp$body))
    prow <- tibble::tibble(
      post_id = vapply(seq_len(nrow(sp)), function(j)
        .hex2num(substr(digest::digest(paste(mt$minutes_doc_id[i], j, sp$speaker[j])), 1, 12)),
        numeric(1)),
      source_id = ESCRIBE_SOURCE, actor_key = keys,
      native_id = paste0("min", mt$minutes_doc_id[i], "#", seq_len(nrow(sp))),
      thread_t = .escribe_pid(mt$meeting_id[i], 0L),
      forum_id = NA_integer_, article_id = NA_real_, author_user_id = NA_real_,
      posted_at = mt$starts_at[i], seq_in_thread = seq_len(nrow(sp)),
      quote_count = sp$n_statements, n_chars = nchar(body),
      body_local = body, scrape_batch = batch_id, last_seen = Sys.time())
    db_upsert(con, "posts", as.data.frame(prow), "post_id",
              null_safe = setdiff(names(prow), c("post_id","scrape_batch","last_seen")))
    n_sp <- n_sp + nrow(prow)
    message(sprintf("  %s %s: %d speaker(s), %d opposed / %d in favour",
                    format(as.Date(mt$starts_at[i])), mt$meeting_name[i], nrow(sp),
                    sum(sp$stance_hint == "oppose", na.rm = TRUE),
                    sum(sp$stance_hint == "support", na.rm = TRUE)))
  }
  message(sprintf("escribe minutes: %d testimony records; %d items drew no public speaker",
                  n_sp, n_silent))
  invisible(n_sp)
}

`%||%` <- function(a, b) if (is.null(a)) b else a
