#!/usr/bin/env Rscript
# Export the de-texted serve copy that BOTH the dashboard and the daily brief
# read.
#
# WHY THIS IS NOW ITS OWN DAILY STEP. export_serve_db() was called from exactly
# one place: the last line of 03_weekly.R, and no scheduler has ever run the
# weekly script. So db/serve.duckdb was frozen at whatever day Nelson last ran
# the weekly job by hand (2026-08-09), while db/civic_pulse.duckdb kept growing
# every morning. Three things downstream then went quietly wrong at once:
#
#   1. the dashboard rendered five-week-old numbers, because it reads the serve
#      copy by design (DuckDB is single-writer, and the serve copy is the
#      de-texted one);
#   2. the daily brief REFUSED to render every morning for 35 days — its
#      staleness guard measured the serve copy, so it was reporting the age of
#      the export, not the age of the corpus, and "castanet_forums 38 days" was
#      true of the file and false of the crawl;
#   3. nothing in either log said "stale export", because every component was
#      behaving exactly as written.
#
# The export is cheap (a few seconds, ~6 MB) and the weekly analyses it carries
# come from output/weekly/*.rds on disk, so running it daily neither recomputes
# the ERGM nor drops it.
#
# Read-only connection on purpose: this must never be the thing that takes the
# write lock, and it has no reason to write to the ingest database.
source("R/db.R"); source("R/serve.R")

con <- db_connect(read_only = TRUE)
on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)

export_serve_db(con)

# Report freshness PER SOURCE into the log. A pooled maximum lets the freshest
# source mask a dead one, which is the fault this project has now hit twice.
fresh <- dbGetQuery(con, "
  SELECT source_id, count(*) AS n, max(posted_at) AS newest
    FROM posts WHERE posted_at <= now() GROUP BY source_id ORDER BY source_id")
for (i in seq_len(nrow(fresh)))
  message(sprintf("  freshness: %-16s %7s posts, newest %s (%d day(s) ago)",
                  fresh$source_id[i], format(fresh$n[i], big.mark = ","),
                  format(as.Date(fresh$newest[i])),
                  as.integer(Sys.Date() - as.Date(fresh$newest[i]))))
