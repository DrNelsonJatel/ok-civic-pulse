#!/usr/bin/env Rscript
# One-off migration: introduce source meta-tagging.
#
# Everything ingested so far came from Castanet's forums, but that was implicit
# — nothing in the data said so. This stamps every existing row with its source
# and introduces `actor_key` as a cross-source identity, so a Reddit user and a
# phpBB user can coexist without colliding in a single numeric id space.
#
# DuckDB cannot change a primary key in place, so `actors` is rebuilt rather
# than altered.
source("R/db.R"); suppressMessages({library(DBI); library(dplyr)})
con <- db_connect()

add_col <- function(tbl, col, type, default = NULL) {
  cols <- dbGetQuery(con, sprintf("SELECT * FROM %s LIMIT 0", tbl))
  if (col %in% names(cols)) { message("  ", tbl, ".", col, " exists"); return(invisible()) }
  dbExecute(con, sprintf("ALTER TABLE %s ADD COLUMN %s %s%s", tbl, col, type,
                         if (is.null(default)) "" else paste(" DEFAULT", default)))
  message("  + ", tbl, ".", col)
}

message("posts / threads / edges_reply:")
for (spec in list(c("posts","source_id","VARCHAR","'castanet_forums'"),
                  c("posts","actor_key","VARCHAR",NA),
                  c("posts","native_id","VARCHAR",NA),
                  c("threads","source_id","VARCHAR","'castanet_forums'"),
                  c("edges_reply","source_id","VARCHAR","'castanet_forums'"),
                  c("edges_reply","from_actor_key","VARCHAR",NA),
                  c("edges_reply","to_actor_key","VARCHAR",NA)))
  add_col(spec[1], spec[2], spec[3], if (is.na(spec[4])) NULL else spec[4])

message("backfilling castanet identities...")
dbExecute(con, "UPDATE posts SET source_id='castanet_forums' WHERE source_id IS NULL")
dbExecute(con, "UPDATE posts SET actor_key = 'castanet:' || CAST(author_user_id AS VARCHAR)
                 WHERE actor_key IS NULL AND author_user_id IS NOT NULL")
dbExecute(con, "UPDATE posts SET native_id = CAST(post_id AS VARCHAR) WHERE native_id IS NULL")
dbExecute(con, "UPDATE threads SET source_id='castanet_forums' WHERE source_id IS NULL")
dbExecute(con, "UPDATE edges_reply SET source_id='castanet_forums' WHERE source_id IS NULL")
dbExecute(con, "UPDATE edges_reply SET
                 from_actor_key = 'castanet:' || CAST(from_user_id AS VARCHAR),
                 to_actor_key   = 'castanet:' || CAST(to_user_id AS VARCHAR)
                 WHERE from_actor_key IS NULL")

# actors: rebuild, because the primary key changes from user_id to actor_key.
has_key <- "actor_key" %in% names(dbGetQuery(con, "SELECT * FROM actors LIMIT 0"))
if (!has_key) {
  message("rebuilding actors with actor_key primary key...")
  # CREATE TABLE AS SELECT copies rows but SILENTLY DROPS EVERY CONSTRAINT,
  # including the primary key. An upsert keyed on actor_key then fails with
  # "not referenced by a UNIQUE/PRIMARY KEY CONSTRAINT". Declare the table
  # explicitly and INSERT into it.
  dbExecute(con, "CREATE TABLE actors_new (
      actor_key VARCHAR PRIMARY KEY, source_id VARCHAR, user_id BIGINT,
      handle VARCHAR, join_date DATE, total_posts_at_capture INTEGER,
      is_staff BOOLEAN, first_seen_in_corpus TIMESTAMP, last_seen_in_corpus TIMESTAMP)")
  dbExecute(con, "INSERT INTO actors_new
    SELECT 'castanet:' || CAST(user_id AS VARCHAR) AS actor_key,
           'castanet_forums' AS source_id, user_id, handle, join_date,
           total_posts_at_capture, is_staff, first_seen_in_corpus, last_seen_in_corpus
      FROM actors WHERE user_id IS NOT NULL")
  dbExecute(con, "DROP TABLE actors")
  dbExecute(con, "ALTER TABLE actors_new RENAME TO actors")
} else message("actors already migrated")

db_init(con)   # creates sources / source_daily if absent

message("\nverification:")
print(dbGetQuery(con, "SELECT source_id, count(*) posts, count(DISTINCT actor_key) actors
                         FROM posts GROUP BY 1"))
print(dbGetQuery(con, "SELECT count(*) actors, count(DISTINCT actor_key) keys FROM actors"))
dbDisconnect(con, shutdown = TRUE)
