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
if (file.exists("db/civic_pulse.duckdb") && file.exists("docs/index.html")) {
  con <- dbConnect(duckdb::duckdb(), "db/civic_pulse.duckdb", read_only = TRUE)
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
  nhand <- length(hitlist)
  res(nfrag == 0, "published dashboard contains no comment text", paste(nfrag, "fragments"))
  res(nhand == 0, "published dashboard contains no handles",
      if (nhand) paste(head(hitlist, 3), collapse = ", ") else "")
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

# ---------------------------------------------------------------- data ------
section("3. DATA INTEGRITY")
if (!file.exists("db/civic_pulse.duckdb")) {
  skip("data integrity checks", "db/civic_pulse.duckdb absent (expected in CI)")
} else {
  con <- dbConnect(duckdb::duckdb(), "db/civic_pulse.duckdb", read_only = TRUE)
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
