#!/usr/bin/env Rscript
# Evidence for widening seeds: for each flagged code, find the terms that are
# distinctive of posts the MODEL coded with that code but the SIEVE missed.
# Proposals must come from the corpus, not from imagination — a guessed seed
# looks just as plausible as a measured one and is far likelier to be wrong.
source("R/db.R"); source("R/taxonomy.R"); source("R/codebook.R")
suppressMessages({library(DBI); library(dplyr); library(stringr)})
con <- db_connect()

h <- readRDS("output/weekly/codebook_health.rds")$codes
widen <- h |> filter(grepl("widen the seeds", flag)) |>
  select(code, mentions, posts, sieve_posts_seen, seed_agreement) |>
  arrange(desc(seed_agreement))
cat("=== codes whose seeds under-recall ===\n")
print(as.data.frame(widen |> mutate(seed_agreement = round(seed_agreement, 2))), row.names = FALSE)

STOP <- c(letters, "the","and","that","for","with","this","have","are","was","not","but","you",
          "they","their","from","would","will","has","been","its","it's","all","can","who","what",
          "there","which","when","them","were","than","then","more","some","just","about","one",
          "out","get","like","also","only","other","into","over","any","how","said","say","says",
          "people","think","know","need","much","many","even","because","should","could","being",
          "does","done","make","made","time","way","new","now","see","going","good","well","back")

for (i in seq_len(nrow(widen))) {
  cd <- widen$code[i]
  d <- dbGetQuery(con, sprintf("
    SELECT p.body_local,
           CASE WHEN s.post_id IS NULL THEN 'missed_by_sieve' ELSE 'caught' END AS grp
      FROM post_issues i
      JOIN posts p ON p.post_id = i.post_id
      LEFT JOIN (SELECT DISTINCT post_id FROM post_issues
                  WHERE coder_id = 'keyword-sieve' AND issue_code = '%s') s
             ON s.post_id = i.post_id
     WHERE i.coder_id = 'claude:claude-opus-5' AND i.issue_code = '%s'
       AND p.body_local IS NOT NULL", cd, cd))
  miss <- filter(d, grp == "missed_by_sieve")
  if (nrow(miss) < 10) next
  toks <- unlist(str_extract_all(tolower(paste(miss$body_local, collapse = " ")),
                                 "[a-z][a-z'-]{3,}"))
  toks <- toks[!toks %in% STOP]
  # Drop terms the existing seed regex already matches — those are not gaps.
  cur <- ISSUES$seeds[match(cd, ISSUES$code)]
  keep <- !vapply(unique(toks), function(t)
    isTRUE(tryCatch(str_detect(t, regex(cur, ignore_case = TRUE)), error = function(e) FALSE)),
    logical(1))
  tb <- sort(table(toks)[names(which(keep))], decreasing = TRUE)
  cat(sprintf("\n--- %s : %d model-coded posts, %d missed by seeds ---\n",
              cd, nrow(d), nrow(miss)))
  cat("   candidate terms: ", paste(head(names(tb), 18), collapse = ", "), "\n")
}
dbDisconnect(con, shutdown = TRUE)
