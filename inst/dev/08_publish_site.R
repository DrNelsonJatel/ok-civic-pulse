#!/usr/bin/env Rscript
# Render the dashboard into docs/ and publish it.
#
# WHY THIS EXISTS. GitHub Pages serves docs/ from the default branch, so the
# public page at drnelsonjatel.github.io/ok-civic-pulse only changes when a
# rendered docs/ is COMMITTED. Both halves of that were manual: nothing
# rendered the dashboard and nothing committed it, so the published page sat at
# its last hand-render (2026-08-09) for five weeks while the pipeline ran
# successfully every single morning. A daily job that collects, codes and
# reports but never publishes looks identical, from its own logs, to one that
# works.
#
# ORDER MATTERS, TWICE:
#   * The briefs are copied AFTER the render. Quarto normally clears its
#     output directory; here it declines to, warning "Refusing to remove
#     directory ... since it is not a subdirectory of the main project
#     directory" because output-dir points outside dashboard/. Do not rely on
#     that: it is a warning about an unexpected configuration, not a promise,
#     so the copy stays after the render where it is safe either way.
#   * The Library tab indexes output/reports (the local source) but links to
#     reports/<file> (the published copy). The render and the copy happen
#     seconds apart in this one script so the index and the files agree. Split
#     them across two jobs and the table will list briefs that 404.
#
# The briefs are safe to publish: they are aggregate-only by construction (no
# comment text, no handles, no speaker names) and that was verified against a
# rendered PDF before this script was written. The DuckDB, which does hold
# text, is git-ignored and is not touched here.
suppressMessages({library(DBI)})

# Every path here is relative to the repo root, the same convention as the rest
# of inst/dev, so refuse to run from anywhere else rather than writing docs/
# into some arbitrary directory.
if (!file.exists(file.path("dashboard", "index.qmd")))
  stop("run this from the repo root (dashboard/index.qmd not found here).", call. = FALSE)

KEEP     <- as.integer(Sys.getenv("OKCP_PUBLISH_KEEP_BRIEFS", "60"))
NO_PUSH  <- identical(Sys.getenv("OKCP_NO_PUSH"), "1")
SERVE    <- Sys.getenv("OKCP_SERVE_DB", "db/serve.duckdb")
STAMP_ID <- "okcp-build-stamp"   # must match dashboard/index.qmd

step <- function(msg) message(sprintf("[publish] %s", msg))

if (!file.exists(SERVE))
  stop("no serve db at ", SERVE, " — run inst/dev/08_export_serve.R first.",
       call. = FALSE)

# ---- 1. render --------------------------------------------------------------
# Run from the repo root: `quarto render dashboard` renders that project, and
# its output-dir (../docs, relative to the project) lands at docs/ here. Every
# path inside index.qmd is relative to dashboard/, which Quarto sets for us.
step("rendering dashboard")
rc <- system2("quarto", c("render", "dashboard"), wait = TRUE)
if (rc != 0) stop("quarto render failed (rc=", rc, ") — nothing published.",
                  call. = FALSE)

idx <- file.path("docs", "index.html")
if (!file.exists(idx)) stop("render reported success but docs/index.html is missing.",
                            call. = FALSE)

# Read the artifact back and assert the build stamp is in it. `quarto render`
# can exit 0 having produced a page whose data chunks all failed soft, and this
# is the cheap check that the page we are about to publish was actually built
# from today's data rather than recovered from a freeze/cache.
html <- readLines(idx, warn = FALSE)
if (!any(grepl(STAMP_ID, html, fixed = TRUE)))
  stop("rendered page carries no build stamp (#", STAMP_ID, ") — refusing to publish ",
       "a page whose provenance cannot be read back.", call. = FALSE)
today <- format(Sys.Date(), "%Y-%m-%d")
if (!any(grepl(today, html, fixed = TRUE)))
  stop("rendered page does not mention today's date (", today, ") — the render ",
       "probably came from a stale serve copy. Refusing to publish.", call. = FALSE)
step(sprintf("rendered docs/index.html (%s KB), build stamp present",
             round(file.size(idx) / 1024)))

# ---- 2. publish the brief archive ------------------------------------------
# The Library tab is only a library if the files it lists are actually served.
# Briefs live in output/reports, which is git-ignored, so the published copies
# go to docs/reports and are pruned to the most recent KEEP to bound the repo.
src <- file.path("output", "reports")
dst <- file.path("docs", "reports")
dir.create(dst, recursive = TRUE, showWarnings = FALSE)
pdfs <- sort(list.files(src, pattern = "\\.pdf$", full.names = TRUE), decreasing = TRUE)
if (!length(pdfs)) {
  step("no briefs in output/reports — Library will render its empty state")
} else {
  pdfs <- head(pdfs, KEEP)
  ok <- file.copy(pdfs, dst, overwrite = TRUE)
  if (!all(ok)) stop("failed to copy brief(s): ",
                     paste(basename(pdfs[!ok]), collapse = ", "), call. = FALSE)
  stale <- setdiff(list.files(dst, pattern = "\\.pdf$"), basename(pdfs))
  if (length(stale)) unlink(file.path(dst, stale))
  step(sprintf("published %d brief(s), pruned %d", length(pdfs), length(stale)))
}

# ---- 3. commit and push docs/ ONLY -----------------------------------------
# `git commit --only docs` and not `git add -A`: a scheduled job and an
# interactive session share one working copy and one index, so a job that
# stages broadly can sweep up whatever Nelson had staged at 06:20 — including,
# in this repo, files that are git-ignored for text-leak reasons if they were
# ever force-added. --only commits these paths and leaves the index alone.
git <- function(...) system2("git", c(...), stdout = TRUE, stderr = TRUE)

changed <- length(git("status", "--porcelain", "--", "docs")) > 0
# `git add -- docs` first: --only commits the named paths and ignores whatever
# else is staged, but it can only commit paths git already knows about, and a
# first publish brings NEW files (docs/reports, new site_libs assets). Scoping
# the add to docs keeps the shared-index protection intact.
if (changed) git("add", "--", "docs")
if (!changed) {
  step("docs/ is byte-identical to the last publish — nothing to commit")
} else {
  msg <- sprintf("Publish dashboard %s", format(Sys.time(), "%Y-%m-%d %H:%M %Z"))
  out <- git("commit", "--only", "docs", "-m", msg)
  if (!is.null(attr(out, "status")) && attr(out, "status") != 0)
    stop("commit failed:\n", paste(out, collapse = "\n"), call. = FALSE)
  step(paste("committed:", msg))

  if (NO_PUSH) {
    step("OKCP_NO_PUSH=1 — committed but not pushed")
  } else {
    out <- git("push", "origin", "HEAD")
    if (!is.null(attr(out, "status")) && attr(out, "status") != 0)
      stop("push failed (the page will NOT update):\n", paste(out, collapse = "\n"),
           call. = FALSE)
    step("pushed — GitHub Pages will rebuild within a minute or two")
  }
}
