# taxonomy.R — loads the versioned codebook and provides the keyword sieve.
#
# The codebook itself now lives in codebook/codebook.yml (see R/codebook.R).
# This file is the runtime view of it: it exposes ISSUES, JURISDICTIONS,
# WEAK_GATE_CODES and CODEBOOK_VERSION in the shapes the pipeline expects, and
# implements the sieve.
#
# CODEBOOK_VERSION is a content hash. Every coded row records it, so revising a
# definition never silently rewrites the meaning of already-coded history.
suppressMessages({library(yaml); library(digest); library(tibble); library(stringr); library(dplyr)})

.rx <- function(x) stringr::regex(x, ignore_case = TRUE)

.codebook_file <- function() {
  p <- Sys.getenv("OKCP_CODEBOOK", "codebook/codebook.yml")
  if (file.exists(p)) return(p)
  # Allow sourcing from a subdirectory (e.g. dashboard/ during a Quarto render).
  alt <- file.path("..", p)
  if (file.exists(alt)) return(alt)
  stop("codebook not found at ", p, call. = FALSE)
}

.CB <- yaml::read_yaml(.codebook_file())

ISSUES <- bind_rows(lapply(.CB$issues, function(x) tibble(
  code = x$code, label = x$label %||% x$code, scope = x$scope,
  definition = x$definition, seeds = x$seeds)))

JURISDICTIONS <- bind_rows(lapply(.CB$jurisdictions, function(x) tibble(
  code = x$code, label = x$label %||% x$code, seeds = x$seeds)))

WEAK_GATE_CODES <- unlist(.CB$weak_gate_codes %||% character())

CODEBOOK_VERSION <- local({
  sem <- list(
    issues = lapply(.CB$issues[order(vapply(.CB$issues, `[[`, "", "code"))], function(x)
      list(code = x$code, scope = x$scope, definition = x$definition, seeds = x$seeds)),
    jurisdictions = lapply(.CB$jurisdictions[order(vapply(.CB$jurisdictions, `[[`, "", "code"))],
      function(x) list(code = x$code, seeds = x$seeds)),
    weak_gate = sort(WEAK_GATE_CODES))
  substr(digest::digest(sem, algo = "sha256"), 1, 12)
})

# ---- sieve ------------------------------------------------------------------
#
# RECALL FILTER, NOT A FINDING. The sieve decides which comments are worth a
# model call. Word boundaries are mandatory: without them "river" matches
# "driver", "ALR" matches "alright", "STR" matches "strata".
sieve_issues <- function(txt) {
  txt <- coalesce(txt, "")
  hit <- vapply(ISSUES$seeds, function(s) str_detect(txt, .rx(s)), logical(1))
  ISSUES$code[hit]
}

detect_jurisdiction <- function(txt) {
  txt <- coalesce(txt, "")
  hit <- vapply(JURISDICTIONS$seeds, function(s) str_detect(txt, .rx(s)), logical(1))
  if (!any(hit)) return(NA_character_)
  paste(JURISDICTIONS$code[hit], collapse = ",")
}

sieve_corpus <- function(posts, text_col = "body_local", title_col = NULL) {
  txt <- coalesce(posts[[text_col]], "")
  if (!is.null(title_col)) txt <- paste(coalesce(posts[[title_col]], ""), txt)
  bind_rows(lapply(seq_along(txt), function(i) {
    codes <- sieve_issues(txt[i])
    if (!length(codes)) return(NULL)
    tibble(post_id = posts$post_id[i], issue_code = codes,
           jurisdiction = detect_jurisdiction(txt[i]))
  }))
}

ON_BALLOT_SCOPES <- c("local", "ballot", "shared")
issue_scope <- function(code) ISSUES$scope[match(code, ISSUES$code)]
issue_label <- function(code)
  ifelse(code %in% ISSUES$code, ISSUES$label[match(code, ISSUES$code)], code)

`%||%` <- function(a, b) if (is.null(a)) b else a
