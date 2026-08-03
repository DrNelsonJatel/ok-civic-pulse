# codebook.R — the codebook as versioned, reviewable data.
#
# The codebook is this project's main intellectual asset, so it lives in
# codebook/codebook.yml rather than as a literal buried in R code. Three things
# follow from that:
#
#  1. VERSIONING. The version is a content hash of the normalised codebook, so
#     it cannot drift from what is actually in the file. Every coded row
#     records the version that produced it, so revising a definition never
#     silently rewrites the meaning of already-coded history — you can always
#     ask "what did `housing` mean when this comment was coded?"
#
#  2. HEALTH METRICS. Revision is driven by evidence, not intuition: over-use,
#     under-use, low model confidence (an ambiguous definition), bulk-vs-audit
#     disagreement (an unclear boundary), and coverage gaps where the sieve
#     fired but the model returned 'none'.
#
#  3. REVIEW. Notes and proposals accumulate against a code in codebook_review.
#     Git holds the YAML's history; DuckDB holds the running commentary.
suppressMessages({library(yaml); library(digest); library(dplyr); library(DBI); library(stringr)})

# Same regex constructor the sieve uses, so validation and execution agree.
if (!exists(".rx")) .rx <- function(x) stringr::regex(x, ignore_case = TRUE)

CODEBOOK_PATH <- Sys.getenv("OKCP_CODEBOOK", "codebook/codebook.yml")

# ---- load / validate --------------------------------------------------------
codebook_load <- function(path = CODEBOOK_PATH) {
  if (!file.exists(path)) stop("codebook not found: ", path, call. = FALSE)
  cb <- yaml::read_yaml(path)
  codebook_validate(cb)
  cb$version <- codebook_hash(cb)
  cb
}

# The hash covers only the SEMANTIC content — codes, scopes, definitions,
# seeds, jurisdictions. Editing the label or a comment does not mint a new
# version; changing what a code MEANS does.
codebook_hash <- function(cb) {
  sem <- list(
    issues = lapply(cb$issues[order(vapply(cb$issues, `[[`, "", "code"))], function(x)
      list(code = x$code, scope = x$scope, definition = x$definition, seeds = x$seeds)),
    jurisdictions = lapply(cb$jurisdictions[order(vapply(cb$jurisdictions, `[[`, "", "code"))],
      function(x) list(code = x$code, seeds = x$seeds)),
    weak_gate = sort(unlist(cb$weak_gate_codes %||% character()))
  )
  substr(digest::digest(sem, algo = "sha256"), 1, 12)
}

VALID_SCOPES <- c("local","ballot","shared","provincial","federal")

codebook_validate <- function(cb) {
  if (!length(cb$issues)) stop("codebook has no issues", call. = FALSE)
  codes <- vapply(cb$issues, function(x) x$code %||% "", "")
  if (any(!nzchar(codes))) stop("every issue needs a code", call. = FALSE)
  if (anyDuplicated(codes)) stop("duplicate issue codes: ",
    paste(codes[duplicated(codes)], collapse = ", "), call. = FALSE)
  if ("none" %in% codes) stop("'none' is reserved and must not be a codebook entry", call. = FALSE)
  for (x in cb$issues) {
    if (!x$scope %in% VALID_SCOPES)
      stop("issue ", x$code, " has invalid scope '", x$scope, "'", call. = FALSE)
    if (!nzchar(x$definition %||% ""))
      stop("issue ", x$code, " has no definition — the model reads this", call. = FALSE)
    # A seed regex that fails to compile would silently sieve nothing, so it is
    # checked here — but with the SAME engine the sieve uses. stringr wraps ICU,
    # which supports the lookahead/lookbehind several seeds rely on
    # ("(?! ?Act)", "(?<!West )"); base grepl's TRE engine does not and would
    # reject perfectly good patterns. Validating against a different engine
    # than you run against is worse than not validating.
    tryCatch(stringr::str_detect("probe", .rx(x$seeds)),
             error = function(e) stop("issue ", x$code, " seed regex does not compile: ",
                                      conditionMessage(e), call. = FALSE))
  }
  for (j in cb$jurisdictions)
    tryCatch(stringr::str_detect("probe", .rx(j$seeds)), error = function(e)
      stop("jurisdiction ", j$code, " seed regex does not compile: ",
           conditionMessage(e), call. = FALSE))
  invisible(TRUE)
}

# Flatten to the tibbles the rest of the pipeline already expects, so adopting
# the YAML is a drop-in replacement for the hard-coded tribbles.
codebook_issues <- function(cb) {
  dplyr::bind_rows(lapply(cb$issues, function(x) tibble::tibble(
    code = x$code, label = x$label %||% x$code, scope = x$scope,
    definition = x$definition, seeds = x$seeds)))
}
codebook_jurisdictions <- function(cb) {
  dplyr::bind_rows(lapply(cb$jurisdictions, function(x) tibble::tibble(
    code = x$code, label = x$label %||% x$code, seeds = x$seeds)))
}

# ---- register a version in the DB ------------------------------------------
codebook_register <- function(con, cb, label = NULL, notes = NULL) {
  db_upsert(con, "codebook_version", data.frame(
    version = cb$version, label = label %||% (cb$label %||% NA_character_),
    n_codes = length(cb$issues), adopted_at = Sys.time(),
    notes = notes %||% NA_character_, stringsAsFactors = FALSE), "version")
  invisible(cb$version)
}

# ---- health metrics: the evidence for the next revision ---------------------
codebook_health <- function(con, cb = NULL, bulk = NULL, audit = NULL) {
  if (is.null(cb)) cb <- codebook_load()
  iss <- codebook_issues(cb)

  use <- dbGetQuery(con, "
    SELECT issue_code, count(*) AS mentions, count(DISTINCT post_id) AS posts,
           avg(confidence) AS mean_conf, avg(salience) AS mean_salience
      FROM post_issues
     WHERE coder_id <> 'keyword-sieve' AND issue_code <> 'none'
     GROUP BY issue_code")

  # Seed agreement is only meaningful on posts the model ACTUALLY SAW. The
  # scope gate deliberately withholds out-of-scope-only posts, so counting
  # every sieve hit in the denominator would permanently condemn codes like
  # courts_bail and national_partisan as "over-firing" when in truth they were
  # never given the chance to be confirmed. Restrict to classified posts.
  sieve <- dbGetQuery(con, "
    SELECT s.issue_code, count(DISTINCT s.post_id) AS sieve_posts_seen
      FROM post_issues s
     WHERE s.coder_id = 'keyword-sieve' AND s.issue_code <> 'none'
       AND s.post_id IN (SELECT post_id FROM post_issues WHERE coder_id <> 'keyword-sieve')
     GROUP BY s.issue_code")

  h <- iss |>
    left_join(use,   by = c("code" = "issue_code")) |>
    left_join(sieve, by = c("code" = "issue_code")) |>
    mutate(mentions = coalesce(mentions, 0L), posts = coalesce(posts, 0L),
           sieve_posts_seen = coalesce(sieve_posts_seen, 0L),
           # Ratio, not precision: values above 1 mean the model finds the issue
           # in comments the seeds missed (good recall signal for the seeds),
           # below 1 means the seeds fire where the model disagrees.
           seed_agreement = ifelse(sieve_posts_seen > 0, posts / sieve_posts_seen, NA_real_),
           share = ifelse(sum(mentions) > 0, mentions / sum(mentions), NA_real_))

  # Flags, in plain language, because these drive an editorial decision.
  h <- h |> mutate(flag = case_when(
    posts == 0 & sieve_posts_seen == 0       ~ "not yet exercised - no evidence either way",
    posts == 0 & sieve_posts_seen > 0        ~ "seeds fire but model never uses it - definition may be wrong",
    share > 0.15                             ~ "over-used - likely too broad, consider splitting",
    !is.na(seed_agreement) & seed_agreement < 0.35 ~ "seeds over-fire on comments the model saw - tighten regex",
    !is.na(seed_agreement) & seed_agreement > 2.0  ~ "model finds it where seeds miss - widen the seeds",
    !is.na(mean_conf) & mean_conf < 0.65     ~ "low model confidence - definition may be ambiguous",
    posts < 3                                ~ "rarely used - may be too narrow",
    TRUE                                     ~ "ok"))

  # Coverage gap: the sieve found something, the model said 'none'. These are
  # the comments most likely to be pointing at a code the codebook lacks.
  gap <- dbGetQuery(con, "
    SELECT count(*) AS n FROM (
      SELECT post_id FROM post_issues WHERE coder_id = 'keyword-sieve' AND issue_code <> 'none'
      INTERSECT
      SELECT post_id FROM post_issues
       WHERE coder_id <> 'keyword-sieve' GROUP BY post_id
      HAVING max(CASE WHEN issue_code <> 'none' THEN 1 ELSE 0 END) = 0)")

  list(version = cb$version, codes = arrange(h, desc(mentions)),
       coverage_gap_posts = gap$n[1])
}

# ---- review notes: the comment/adjust surface -------------------------------
codebook_note <- function(con, issue_code, kind, note, proposed = NULL,
                          author = Sys.getenv("USER", "unknown"), version = NULL) {
  stopifnot(kind %in% c("note","split","merge","redefine","new","retire"))
  if (is.null(version)) version <- codebook_load()$version
  rid <- substr(digest::digest(list(issue_code, kind, note, Sys.time())), 1, 12)
  db_upsert(con, "codebook_review", data.frame(
    review_id = rid, issue_code = issue_code, version = version, kind = kind,
    note = note, proposed = proposed %||% NA_character_, status = "open",
    author = author, created_at = Sys.time(), stringsAsFactors = FALSE), "review_id")
  message("codebook note recorded: ", rid, " (", kind, " / ", issue_code, ")")
  invisible(rid)
}

codebook_open_reviews <- function(con) {
  dbGetQuery(con, "
    SELECT review_id, issue_code, kind, note, proposed, version, author, created_at
      FROM codebook_review WHERE status = 'open' ORDER BY created_at DESC")
}

`%||%` <- function(a, b) if (is.null(a)) b else a
