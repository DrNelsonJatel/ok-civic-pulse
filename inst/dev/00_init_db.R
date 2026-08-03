#!/usr/bin/env Rscript
# Initialise the DuckDB schema and seed the forum registry (with live sizes).
source("R/db.R"); source("R/fetch.R"); source("R/forums.R")
suppressMessages(library(dplyr))

con <- db_connect(); db_init(con)

live <- tryCatch(index_forums(), error = function(e) {
  message("index_forums failed (", conditionMessage(e), ") — seeding without live sizes")
  tibble::tibble(forum_id = integer(), name = character(),
                 n_topics = integer(), n_posts = integer())
})

reg <- FORUM_REGISTRY |>
  left_join(select(live, forum_id, n_topics, n_posts), by = "forum_id") |>
  mutate(last_indexed = Sys.time())

db_upsert(con, "forums", as.data.frame(reg), "forum_id")

cat("forums registered:\n")
print(dbGetQuery(con,
  "SELECT forum_id, name, kind, priority, active, n_topics, n_posts
     FROM forums ORDER BY active DESC, priority, forum_id"))

crawl_budget <- dbGetQuery(con,
  "SELECT sum(n_posts) posts, sum(n_topics) topics FROM forums WHERE active")
cat(sprintf("\nactive crawl surface: %s topics / %s posts (lifetime)\n",
            format(crawl_budget$topics, big.mark = ","),
            format(crawl_budget$posts,  big.mark = ",")))

DBI::dbDisconnect(con, shutdown = TRUE)
