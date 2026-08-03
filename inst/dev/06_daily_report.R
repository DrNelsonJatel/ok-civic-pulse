#!/usr/bin/env Rscript
# Render the branded daily brief.
#
# Runs AFTER 02_daily.R, because it reads db/serve.duckdb — the de-texted copy
# the daily job exports. It never touches the live ingest DB, so a render can
# safely overlap a crawl (DuckDB is single-writer and would otherwise fail on
# a lock).
#
# Typst, not LaTeX: ~1s render with no TeX installation, which is what makes a
# daily automated PDF practical at all.
suppressMessages({library(DBI)})

serve <- "db/serve.duckdb"
if (!file.exists(serve)) stop("db/serve.duckdb missing — run export_serve_db() first.", call. = FALSE)

# Refuse to publish a stale brief silently. A report dated today that is built
# from week-old data is worse than no report.
con <- dbConnect(duckdb::duckdb(), serve, read_only = TRUE)
fresh <- dbGetQuery(con, "SELECT max(posted_at) AS m FROM posts_meta")$m
dbDisconnect(con, shutdown = TRUE)
age <- as.integer(Sys.Date() - as.Date(fresh))
MAX_AGE <- as.integer(Sys.getenv("OKCP_MAX_REPORT_AGE_DAYS", "3"))
if (is.na(age)) stop("cannot determine corpus freshness", call. = FALSE)
if (age > MAX_AGE)
  stop(sprintf("corpus is %d days stale (limit %d) — refusing to render a brief that looks current. Run 02_daily.R.",
               age, MAX_AGE), call. = FALSE)

out <- sprintf("output/reports/civic-pulse-%s.pdf", format(Sys.Date(), "%Y-%m-%d"))
dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)

message("rendering daily brief (corpus ", age, " day(s) old)...")
rc <- system2("quarto", c("render", "report/daily.qmd"), stdout = TRUE, stderr = TRUE)
if (!file.exists("report/daily.pdf"))
  stop("render produced no PDF:\n", paste(tail(rc, 15), collapse = "\n"), call. = FALSE)

file.copy("report/daily.pdf", out, overwrite = TRUE)
message("wrote ", out, " (", round(file.size(out) / 1024), " KB)")

# Email delivery: the Gmail-token-as-mounted-file pattern applies here rather
# than an env var, because gargle's cache lookup fails in a headless run.
# Deliberately not wired up until recipients are confirmed.
if (nzchar(Sys.getenv("OKCP_MAIL_TO")))
  message("NOTE: OKCP_MAIL_TO is set but delivery is not enabled yet — ",
          "confirm recipients before any send.")
