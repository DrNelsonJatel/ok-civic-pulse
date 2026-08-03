# sieve_run.R — apply the keyword sieve to uncoded posts.
#
# IMPORTANT: sieve rows are a RECALL FILTER, written with coder_id
# 'keyword-sieve'. They decide which comments deserve a model call. They are
# NOT findings — `governance_process` alone fires on any comment containing
# "election" or "council", which would wildly overstate local scope. Every
# metric and every report reads model/human codes only (coder_id <> sieve).
suppressMessages({library(dplyr); library(DBI)})

SIEVE_CODER <- "keyword-sieve"

sieve_new_posts <- function(con, limit = Inf) {
  posts <- dbGetQuery(con, sprintf("
    SELECT p.post_id, p.body_local, t.title
      FROM posts p
      LEFT JOIN threads t ON t.t_id = p.thread_t
     WHERE p.body_local IS NOT NULL
       AND p.post_id NOT IN (SELECT post_id FROM post_issues WHERE coder_id = '%s')
     %s", SIEVE_CODER,
    if (is.finite(limit)) paste("LIMIT", as.integer(limit)) else ""))
  if (!nrow(posts)) { message("sieve: nothing new"); return(invisible(0L)) }

  hits <- sieve_corpus(posts, text_col = "body_local", title_col = "title")
  # Posts the sieve finds nothing in still get a row, so they are not re-sieved
  # on every run. 'none' is a real classification, not a gap.
  miss <- setdiff(posts$post_id, unique(hits$post_id))
  if (length(miss)) hits <- bind_rows(hits, tibble::tibble(
    post_id = miss, issue_code = "none", jurisdiction = NA_character_))

  out <- hits |>
    mutate(scope = ifelse(issue_code == "none", "none", issue_scope(issue_code)),
           stance = NA_character_, salience = NA_real_, confidence = NA_real_,
           coder_id = SIEVE_CODER, coded_at = Sys.time())
  db_upsert(con, "post_issues", as.data.frame(out),
            c("post_id", "issue_code", "coder_id"))
  message(sprintf("sieve: %d posts -> %d issue rows (%d with no issue)",
                  nrow(posts), nrow(out), length(miss)))
  invisible(nrow(out))
}
