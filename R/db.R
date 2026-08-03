# db.R — DuckDB connection, schema init, idempotent upsert.
suppressMessages({library(DBI); library(duckdb)})

DB_PATH <- Sys.getenv("OKCP_DB", "db/civic_pulse.duckdb")

db_connect <- function(path = DB_PATH, read_only = FALSE) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  dbConnect(duckdb::duckdb(), dbdir = path, read_only = read_only)
}

db_init <- function(con, schema = "sql/schema.sql") {
  lines <- readLines(schema, warn = FALSE)
  lines <- sub("--.*$", "", lines)                  # strip SQL comments
  sql   <- paste(lines, collapse = "\n")
  for (stmt in strsplit(sql, ";")[[1]]) {
    stmt <- trimws(stmt)
    if (nchar(stmt)) dbExecute(con, stmt)
  }
  invisible(TRUE)
}

# Upsert `df` into `table` keyed on `keys`, refreshing all non-key columns.
#
# NOTE the COALESCE guard on updates: a re-scrape that fails to parse a field
# must not overwrite a good earlier value with NULL. drought-sna lost every
# article_id this way — a later pass wrote NA over a correctly-resolved id and
# the bipartite table silently froze. Only `null_safe` columns are protected;
# pass null_safe = character() to get plain last-write-wins.
db_upsert <- function(con, table, df, keys, null_safe = NULL) {
  if (!nrow(df)) return(invisible(0L))
  tmp <- paste0("stg_", table)
  duckdb::duckdb_register(con, tmp, df)
  on.exit(duckdb::duckdb_unregister(con, tmp), add = TRUE)
  cols    <- names(df)
  updates <- setdiff(cols, keys)
  if (is.null(null_safe)) null_safe <- updates   # default: protect everything
  set_cl <- vapply(updates, function(u) {
    if (u %in% null_safe)
      sprintf('"%s" = COALESCE(excluded."%s", %s."%s")', u, u, table, u)
    else
      sprintf('"%s" = excluded."%s"', u, u)
  }, character(1))
  conflict <- if (length(updates))
    sprintf("ON CONFLICT (%s) DO UPDATE SET %s",
            paste(sprintf('"%s"', keys), collapse = ", "),
            paste(set_cl, collapse = ", "))
  else
    sprintf("ON CONFLICT (%s) DO NOTHING", paste(sprintf('"%s"', keys), collapse = ", "))
  dbExecute(con, sprintf(
    'INSERT INTO %s (%s) SELECT %s FROM %s %s',
    table,
    paste(sprintf('"%s"', cols), collapse = ", "),
    paste(sprintf('"%s"', cols), collapse = ", "),
    tmp, conflict))
}

db_log <- function(con, batch_id, mode, forums = 0L, threads = 0L,
                   posts_new = 0L, posts_updated = 0L, errors = 0L, notes = "") {
  db_upsert(con, "scrape_log", data.frame(
    batch_id = batch_id, run_at = Sys.time(), mode = mode,
    forums_seen = as.integer(forums), threads_seen = as.integer(threads),
    posts_new = as.integer(posts_new), posts_updated = as.integer(posts_updated),
    errors = as.integer(errors), notes = notes,
    stringsAsFactors = FALSE), "batch_id")
}
