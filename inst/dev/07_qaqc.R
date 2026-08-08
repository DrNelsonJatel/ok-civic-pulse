#!/usr/bin/env Rscript
# QAQC — run before every commit and after every pipeline change.
#
# Ordered by what actually goes wrong. The leak checks come first because a
# text leak is the only failure here that cannot be undone once pushed.
#
# Exits non-zero on any FAIL so it can gate CI.
suppressMessages({library(DBI); library(dplyr)})
source("R/taxonomy.R")

FAILED <- 0L; SKIPPED <- 0L
res <- function(ok, lbl, detail = "") {
  if (!ok) FAILED <<- FAILED + 1L
  cat(sprintf("  [%s] %-50s %s\n", if (ok) "ok" else "FAIL", lbl, detail))
}
# The DuckDB files are deliberately not committed, so CI cannot run the data
# checks. Skipping is not the same as passing: a skip is reported and does not
# affect the exit code, but it is never silent.
skip <- function(lbl, why) {
  SKIPPED <<- SKIPPED + 1L
  cat(sprintf("  [skip] %-50s %s\n", lbl, why))
}
# DuckDB allows one writer and no concurrent readers across processes, so a
# QAQC run started while the daily job or a backfill is going does not just
# fail its DB checks — an unguarded dbConnect() aborts the whole script, taking
# the leak checks down with it. Absent and locked both mean "cannot check now",
# and both must degrade to a visible skip rather than a crash.
try_connect <- function(path) {
  if (!file.exists(path)) return(NULL)
  tryCatch(dbConnect(duckdb::duckdb(), path, read_only = TRUE),
           error = function(e) NULL)
}
section <- function(x) cat("\n== ", x, " ==\n", sep = "")

# ---------------------------------------------------------------- leaks -----
section("1. DISCLOSURE (a leak here is irreversible once pushed)")

serve <- "db/serve.duckdb"
if (file.exists(serve)) {
  con <- dbConnect(duckdb::duckdb(), serve, read_only = TRUE)
  leaks <- character()
  for (t in dbListTables(con)) {
    cols <- names(dbGetQuery(con, sprintf("SELECT * FROM %s LIMIT 0", t)))
    bad <- intersect(cols, c("body_local", "handle", "evidence"))
    if (length(bad)) leaks <- c(leaks, sprintf("%s(%s)", t, paste(bad, collapse = ",")))
  }
  res(length(leaks) == 0, "serve DB carries no text/handle columns", paste(leaks, collapse = " "))
  dbDisconnect(con, shutdown = TRUE)
} else skip("serve DB disclosure check", "db/serve.duckdb absent (expected in CI)")

# Published artifacts must not contain any comment fragment or handle.
con <- try_connect("db/civic_pulse.duckdb")
if (!is.null(con) && file.exists("docs/index.html")) {
  s <- dbGetQuery(con, "SELECT body_local FROM posts WHERE length(body_local)>120 LIMIT 300")$body_local
  # Staff accounts are institutional, not people, and their names legitimately
  # appear in our own source-registry labels ("Castanet News Comments...").
  hd <- dbGetQuery(con, "SELECT handle FROM actors
                          WHERE handle IS NOT NULL AND length(handle) > 5
                            AND COALESCE(is_staff, FALSE) = FALSE")$handle
  dbDisconnect(con, shutdown = TRUE)
  h <- paste(readLines("docs/index.html", warn = FALSE), collapse = "\n")
  frag <- substr(s, 20, 70)
  nfrag <- sum(vapply(frag, function(f) grepl(f, h, fixed = TRUE), logical(1)))
  # MUST be fixed = TRUE. Handles contain regex metacharacters ("that.bcboy",
  # "Guy.Q.Robins"), so a \\b...\\b pattern both misses real matches and
  # invents false ones - this check reported 2 phantom leaks before the fix.
  hitlist <- hd[vapply(hd, function(x) grepl(x, h, fixed = TRUE), logical(1))]

  # Some handles collide with the dashboard's OWN vocabulary. Two real examples:
  # a user called "Resident" (the word appears in findings prose: "Residents and
  # council are focused on different things") and one called "enderby" (a
  # codebook jurisdiction code, printed in the jurisdiction panel). Neither is a
  # leak — the string is in the page because our template or codebook puts it
  # there, and a handle identical to a place name identifies nobody.
  #
  # The allowlist is derived, not hand-maintained: any string the TEMPLATE
  # SOURCE or the codebook already contains is explained without a leak. This
  # narrows the check rather than weakening it — a handle that appears in the
  # HTML and is NOT explainable by our own sources still fails. Exclusions are
  # printed, so the narrowing is never silent.
  tmpl <- paste(unlist(lapply(
    c("dashboard/index.qmd", "R/findings.R", "codebook/codebook.yml"),
    function(f) if (file.exists(f)) readLines(f, warn = FALSE) else character())),
    collapse = "\n")
  explained <- hitlist[vapply(hitlist, function(x)
    grepl(x, tmpl, fixed = TRUE) ||
    grepl(tolower(x), tolower(tmpl), fixed = TRUE), logical(1))]
  if (length(explained))
    cat(sprintf("  [note] %-50s %s\n", "handles explained by template/codebook vocabulary",
                paste(head(explained, 5), collapse = ", ")))
  hitlist <- setdiff(hitlist, explained)
  nhand <- length(hitlist)
  res(nfrag == 0, "published dashboard contains no comment text", paste(nfrag, "fragments"))
  res(nhand == 0, "published dashboard contains no handles",
      if (nhand) paste(head(hitlist, 3), collapse = ", ") else "")
} else {
  # Never let this one pass by silence: it is the check that catches an
  # irreversible disclosure. If it could not run, say so loudly.
  if (!is.null(con)) dbDisconnect(con, shutdown = TRUE)
  skip("published-artifact leak check",
       "civic_pulse.duckdb absent/locked, or docs/index.html not built")
}

# .gitignore must actually be effective. A trailing "# comment" on a pattern
# line is NOT gitignore syntax and silently matches nothing — this exact bug
# nearly committed the DuckDB file.
gi <- if (file.exists(".gitignore")) readLines(".gitignore", warn = FALSE) else character()
bad_gi <- grep("^[^#].*\\s#", gi, value = TRUE)
res(length(bad_gi) == 0, ".gitignore has no trailing-comment patterns",
    paste(head(bad_gi, 2), collapse = " | "))
if (nzchar(Sys.which("git"))) {
  tracked <- system2("git", c("ls-files"), stdout = TRUE, stderr = FALSE)
  risky <- grep("\\.duckdb$|output/batch_.*\\.rds$|py_local/|\\.Renviron$", tracked, value = TRUE)
  res(length(risky) == 0, "no text-bearing files tracked by git",
      paste(head(risky, 3), collapse = " "))
}

# ---------------------------------------------------------------- code ------
section("2. CODE")
files <- list.files(c("R", "inst/dev"), pattern = "\\.R$", full.names = TRUE)
bad <- files[!vapply(files, function(f)
  tryCatch({ parse(f); TRUE }, error = function(e) FALSE), logical(1))]
res(length(bad) == 0, sprintf("all %d R files parse", length(files)), paste(bad, collapse = " "))

# Parsing is not enough. 02_daily.R called classify_new() for weeks and that
# function did not exist anywhere in R/ — the scheduled job aborted at that
# line every night, after the scrape but before the rollup, so posts piled up
# while nothing was coded and the wrapper skipped the report. The file parsed
# perfectly the whole time.
#
# So: for every entry-point script, source ONLY what it sources, then check
# that every function it calls actually resolves. This is the cheapest possible
# guard against a runtime NameError in an unattended job.
resolvable <- function(script) {
  txt  <- readLines(script, warn = FALSE)
  code <- parse(script)
  env  <- new.env(parent = globalenv())
  srcs <- unlist(regmatches(txt, gregexpr('(?<=source\\(")[^"]+(?=")', txt, perl = TRUE)))
  for (s in unique(srcs)) if (file.exists(s))
    try(sys.source(s, envir = env), silent = TRUE)

  # Functions the script DEFINES ITSELF count as resolvable. Without this the
  # check reports every local helper as missing (add_col, ok, no ...), and a
  # check that cries wolf is a check everyone learns to ignore — which is how
  # the real classify_new() gap would have survived it too.
  local_fns <- unlist(lapply(code, function(e) {
    if (is.call(e) && length(e) >= 3 &&
        as.character(e[[1]]) %in% c("<-", "=", "<<-") &&
        is.call(e[[3]]) && identical(as.character(e[[3]][[1]]), "function"))
      as.character(e[[2]]) else NULL
  }))

  called <- unique(unlist(lapply(code, function(e)
    tryCatch(codetools::findGlobals(as.function(list(e)), merge = FALSE)$functions,
             error = function(z) character()))))
  called <- setdiff(called, c("", NA, local_fns))
  called[!vapply(called, function(f)
    exists(f, envir = env, mode = "function") ||
    exists(f, envir = globalenv(), mode = "function"), logical(1))]
}
if (requireNamespace("codetools", quietly = TRUE)) {
  entry <- list.files("inst/dev", pattern = "^[0-9].*\\.R$", full.names = TRUE)
  missing_fns <- lapply(entry, function(f)
    tryCatch(resolvable(f), error = function(e) character()))
  names(missing_fns) <- basename(entry)
  bad_fns <- missing_fns[lengths(missing_fns) > 0]
  res(length(bad_fns) == 0, "every function called in inst/dev resolves",
      paste(sprintf("%s: %s", names(bad_fns),
                    vapply(bad_fns, paste, character(1), collapse = ",")), collapse = "; "))
} else skip("entry-point function resolution", "codetools not installed")

# ---------------------------------------------------------------- data ------
section("3. DATA INTEGRITY")
con <- try_connect("db/civic_pulse.duckdb")
if (is.null(con)) {
  skip("data integrity checks", "civic_pulse.duckdb absent or locked by another job")
} else {
  q <- function(sql) dbGetQuery(con, sql)[[1]]
  res(q("SELECT count(*) FROM posts WHERE source_id IS NULL") == 0, "every post is source-tagged")
  res(q("SELECT count(*) FROM posts WHERE posted_at IS NULL") == 0, "every post has a timestamp")
  res(q("SELECT count(*) FROM post_issues i LEFT JOIN posts p ON p.post_id=i.post_id
           WHERE p.post_id IS NULL") == 0, "no orphan issue codes")
  res(q("SELECT count(*)-count(DISTINCT actor_key) FROM actors") == 0, "actor_key is unique")
  valid <- paste(sprintf("'%s'", c(ISSUES$code, "none")), collapse = ",")
  res(q(sprintf("SELECT count(*) FROM post_issues WHERE coder_id LIKE 'claude:%%'
                   AND issue_code NOT IN (%s)", valid)) == 0,
      "all model codes exist in the codebook")
  res(q("SELECT count(*) FROM post_issues WHERE coder_id LIKE 'claude:%'
           AND scope NOT IN ('local','ballot','shared','provincial','federal','none')") == 0,
      "all scopes are in the vocabulary")
  res(q("SELECT count(*) FROM post_issues WHERE confidence IS NOT NULL
           AND (confidence < 0 OR confidence > 1)") == 0, "confidence within [0,1]")
  # Self-loops are legitimate in the raw table (people quote themselves) but
  # must never reach a network model.
  res(q("SELECT count(*) FROM posts") > 0, "corpus is non-empty", format(q("SELECT count(*) FROM posts")))
  dbDisconnect(con, shutdown = TRUE)

  source("R/db.R"); source("R/sna.R")
  con <- db_connect(read_only = TRUE)
  g <- reply_graph(con)
  res(sum(igraph::which_loop(g)) == 0, "no self-loops reach the reply graph")
  DBI::dbDisconnect(con, shutdown = TRUE)
}

# ------------------------------------------------------------- codebook -----
section("4. CODEBOOK")
cb <- tryCatch({ source("R/codebook.R"); codebook_load() }, error = function(e) e)
# NOTE: at top level R requires `else` on the same line as the closing brace,
# otherwise the file fails to parse - which this very script then reports.
if (inherits(cb, "error")) {
  res(FALSE, "codebook loads and validates", conditionMessage(cb))
} else {
  res(TRUE, "codebook loads and validates", paste("v", cb$version))
  res(identical(cb$version, CODEBOOK_VERSION),
      "codebook.R and taxonomy.R agree on version")
}

cat(sprintf("\n%s  %d failed, %d skipped\n",
            if (FAILED == 0) "PASS —" else "FAIL —", FAILED, SKIPPED))
quit(status = if (FAILED == 0) 0 else 1)
