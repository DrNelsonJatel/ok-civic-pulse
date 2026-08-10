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
#
# TWO THINGS THIS GUARD MUST GET RIGHT, AND ORIGINALLY GOT WRONG:
#
# 1. IGNORE FUTURE-DATED ROWS. eScribe ingests SCHEDULED meetings — the
#    calendar is fetched to Sys.Date() + 30 — so an agenda item carries the
#    date the meeting will happen. max(posted_at) was therefore always in the
#    future, age was always NEGATIVE (it logged "corpus -9 day(s) old"), and
#    `age > MAX_AGE` could never be true. The tripwire was incapable of firing.
#    Had Castanet collection stopped, the brief would have kept publishing on
#    frozen data indefinitely, looking current because council had meetings
#    scheduled.
#
# 2. CHECK EVERY SOURCE, NOT THE POOLED MAXIMUM. A single max() over all posts
#    means the freshest source masks a dead one: eScribe updating daily would
#    hide Castanet having failed weeks ago. Staleness is per-source, and the
#    brief should refuse if ANY active source has gone quiet.
con <- dbConnect(duckdb::duckdb(), serve, read_only = TRUE)
fresh <- dbGetQuery(con, "
  SELECT source_id, max(posted_at) AS m
    FROM posts_meta
   WHERE posted_at <= now()          -- exclude scheduled future meetings
   GROUP BY source_id")
dbDisconnect(con, shutdown = TRUE)
MAX_AGE <- as.integer(Sys.getenv("OKCP_MAX_REPORT_AGE_DAYS", "3"))
if (!nrow(fresh)) stop("cannot determine corpus freshness", call. = FALSE)
fresh$age <- as.integer(Sys.Date() - as.Date(fresh$m))
if (any(is.na(fresh$age))) stop("cannot determine corpus freshness", call. = FALSE)
# eScribe is meeting-driven: council does not sit every week, so a fortnight of
# silence is normal there and means nothing is wrong. Castanet is continuous
# and a three-day gap is a real fault.
lim <- ifelse(fresh$source_id == "kelowna_escribe", max(MAX_AGE, 21L), MAX_AGE)
stale <- fresh$age > lim
if (any(stale))
  stop(sprintf("refusing to render a brief that looks current — stale source(s): %s",
               paste(sprintf("%s %d days (limit %d)", fresh$source_id[stale],
                             fresh$age[stale], lim[stale]), collapse = "; ")),
       call. = FALSE)
age <- max(fresh$age)

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
