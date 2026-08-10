#!/usr/bin/env Rscript
# Backup, in two tiers, because the two halves of this corpus have very
# different replacement costs and very different disclosure rules.
#
# WHAT IS AT RISK. db/civic_pulse.duckdb is ~112 MB, git-ignored (it holds
# comment text, which Castanet's ToU forbids redistributing and which includes
# real names from council minutes), and exists in exactly one place. It
# contains roughly five days of polite crawling and US$56 of Opus
# classification. Time Machine reports this path as included but its
# destination did not mount, so in practice nothing was protecting it.
#
# TIER 1 — full local snapshot. Protects against corruption, a bad migration,
# or an accidental DELETE (this session has already lost a sieve table to a
# mistimed timeout). Does NOT protect against disk loss, and must not leave the
# machine, because it contains text and names.
#
# TIER 2 — de-identified recovery archive. The EXPENSIVE half of the corpus is
# the classification, and classification is de-identified codes, not text. This
# archive carries post_issues, post_sentiment and post metadata with no
# body_local, no handles, no evidence. It is safe to store anywhere — cloud,
# another machine, an external drive — and it means a total disk loss costs
# only the re-crawlable text (free, a few days) and not the US$56 of labels.
# Restoring is a join on post_id after a re-crawl.
suppressMessages({library(DBI); library(dplyr)})

DB    <- "db/civic_pulse.duckdb"
DEST  <- Sys.getenv("OKCP_BACKUP_DIR", "~/Backups/ok-civic-pulse")
DEST  <- path.expand(DEST)
STAMP <- format(Sys.time(), "%Y%m%d-%H%M%S")
dir.create(DEST, recursive = TRUE, showWarnings = FALSE)

stopifnot(file.exists(DB))
if (dir.exists("db/.okcp.lock"))
  stop("a job holds the DB lock — backing up mid-write would capture a torn file", call. = FALSE)

# ---- tier 1: full snapshot --------------------------------------------------
full <- file.path(DEST, sprintf("civic_pulse-%s.duckdb", STAMP))
ok <- file.copy(DB, full, overwrite = FALSE)
if (!ok) stop("full snapshot copy failed", call. = FALSE)

# Verify by OPENING it and counting rows. A byte-for-byte copy that will not
# open is not a backup, and file.copy() returning TRUE proves nothing about
# whether the result is a valid database.
v <- dbConnect(duckdb::duckdb(), full, read_only = TRUE)
n_posts <- dbGetQuery(v, "SELECT count(*) n FROM posts")$n
n_codes <- dbGetQuery(v, "SELECT count(*) n FROM post_issues")$n
dbDisconnect(v, shutdown = TRUE)
message(sprintf("tier 1: %s (%.0f MB) — verified %s posts, %s coded rows",
                basename(full), file.size(full) / 1e6,
                format(n_posts, big.mark = ","), format(n_codes, big.mark = ",")))

# ---- tier 2: de-identified recovery archive ---------------------------------
con <- dbConnect(duckdb::duckdb(), DB, read_only = TRUE)
arc <- file.path(DEST, sprintf("labels-%s.duckdb", STAMP))
if (file.exists(arc)) unlink(arc)
out <- dbConnect(duckdb::duckdb(), arc)
put <- function(nm, sql) {
  d <- dbGetQuery(con, sql); dbWriteTable(out, nm, d, overwrite = TRUE); nrow(d)
}
n <- list(
  # native_id + source_id are what let a re-crawl re-attach these labels.
  posts_key   = put("posts_key", "
    SELECT post_id, source_id, native_id, actor_key, thread_t, forum_id,
           article_id, author_user_id, posted_at, seq_in_thread, quote_count, n_chars
      FROM posts"),
  post_issues = put("post_issues", "SELECT * FROM post_issues"),
  sentiment   = put("post_sentiment", "SELECT * FROM post_sentiment"),
  threads     = put("threads", "
    SELECT t_id, source_id, forum_id, article_id, listing_replies,
           first_post_at, last_post_at, posts_captured FROM threads"),
  edges       = put("edges_reply", "
    SELECT edge_id, source_id, from_user_id, to_user_id, from_actor_key,
           to_actor_key, from_post_id, to_post_id, thread_t, forum_id,
           posted_at, edge_type, weight FROM edges_reply"),
  actors      = put("actors", "
    SELECT actor_key, source_id, user_id, join_date, total_posts_at_capture,
           is_staff FROM actors"),
  audit       = put("audit_sample", "SELECT * FROM audit_sample"),
  cb_version  = put("codebook_version", "SELECT * FROM codebook_version"),
  cb_review   = put("codebook_review", "SELECT * FROM codebook_review"),
  cursors     = put("crawl_cursor", "SELECT * FROM crawl_cursor"))
dbDisconnect(out, shutdown = TRUE); dbDisconnect(con, shutdown = TRUE)

# Prove the archive carries no text or names before calling it shippable.
chk <- dbConnect(duckdb::duckdb(), arc, read_only = TRUE)
leak <- unlist(lapply(dbListTables(chk), function(t)
  intersect(names(dbGetQuery(chk, sprintf("SELECT * FROM %s LIMIT 0", t))),
            c("body_local", "handle", "evidence", "title"))))
dbDisconnect(chk, shutdown = TRUE)
if (length(leak))
  stop("recovery archive contains text/name columns: ", paste(leak, collapse = ", "),
       call. = FALSE)

message(sprintf("tier 2: %s (%.1f MB) — %s, text-free and safe to store offsite",
                basename(arc), file.size(arc) / 1e6,
                paste(sprintf("%s=%d", names(n), unlist(n)), collapse = ", ")))

# Keep the three most recent of each tier; older ones are noise.
for (pat in c("^civic_pulse-", "^labels-")) {
  f <- sort(list.files(DEST, pattern = pat, full.names = TRUE), decreasing = TRUE)
  if (length(f) > 3) { unlink(f[-seq_len(3)]); message("pruned ", length(f) - 3, " old ", pat) }
}
message("\nbackup dir: ", DEST)
