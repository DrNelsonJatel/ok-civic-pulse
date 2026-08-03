#!/usr/bin/env Rscript
# Weekly enrichment: the expensive, slow, or fragile analyses.
#
# Everything here is OPTIONAL by design. The daily job (02_daily.R) produces the
# dashboard and the PDF on its own; this run adds topic models, scaling, the
# ERGM, and the Python visuals. Each step is wrapped so a failure is logged and
# skipped rather than taking the week's run down with it.
source("R/db.R"); source("R/taxonomy.R"); source("R/election.R")
source("R/codebook.R"); source("R/metrics.R"); source("R/sna.R")
source("R/topics.R"); source("R/trends.R"); source("R/scaling.R")
source("R/sentiment.R"); source("R/py_viz.R"); source("R/serve.R")
suppressMessages({library(dplyr); library(DBI)})

step <- function(name, expr) {
  message("\n== ", name, " ==")
  tryCatch(force(expr), error = function(e) {
    message("   SKIPPED — ", conditionMessage(e)); NULL })
}

con <- db_connect(); db_init(con)
dir.create("output/weekly", recursive = TRUE, showWarnings = FALSE)
out <- list()

cb <- step("codebook", { cb <- codebook_load(); codebook_register(con, cb); cb })

out$health <- step("codebook health", {
  h <- codebook_health(con, cb)
  saveRDS(h, "output/weekly/codebook_health.rds")
  message("   coverage gap: ", h$coverage_gap_posts, " posts; ",
          sum(h$codes$flag != "ok"), " codes flagged")
  h })

corp <- step("corpus", build_corpus(con))
dfm  <- step("dfm", if (!is.null(corp)) build_dfm(corp))

out$topics <- step("seeded topic model", {
  tm <- fit_seeded_topics(dfm, residual = 6L)
  res <- residual_report(tm)
  saveRDS(list(terms = topic_terms(tm), residual = res, stable = tm$stable),
          "output/weekly/topics.rds")
  # Residual topics with real mass are candidate new codes — record them as
  # open codebook reviews so they surface in the review tool rather than
  # evaporating with the session.
  for (i in seq_len(min(3L, nrow(res)))) {
    codebook_note(con, issue_code = paste0("(residual) ", res$topic[i]), kind = "new",
                  note = sprintf("Unseeded topic carrying %.1f%% mean prevalence — possible codebook gap.",
                                 100 * res$mean_gamma[i]),
                  proposed = res$top_terms[i], author = "topic-model")
  }
  res })

out$scales <- step("LSX scaling", {
  lapply(names(LSX_SCALES), function(s) {
    r <- fit_lss(dfm, s)
    if (isTRUE(r$ok)) {
      sc <- score_documents(r, dfm, corp)
      saveRDS(list(profile = issue_scale_profile(con, sc), terms = scale_terms(r),
                   label = r$label), file.path("output/weekly", paste0("lss_", s, ".rds")))
      message("   ", s, ": fitted on ", length(r$seeds_used), " seeds")
    } else message("   ", s, ": ", r$reason)
    r }) })

out$bursts <- step("burst detection", {
  b <- all_bursts(con, min_events = 20L)
  saveRDS(b, "output/weekly/bursts.rds"); message("   ", nrow(b), " burst intervals"); b })

out$ergm <- step("ERGM (weekly, gated)", {
  g <- reply_graph(con)
  fit <- fit_ergm(g)
  saveRDS(fit, "output/weekly/ergm.rds")
  message("   ", if (isTRUE(fit$ok)) "converged" else paste("suppressed —", fit$reason))
  fit })

step("Python visuals (optional)", { bertopic_over_time(con); scattertext_local(con) })

step("export serve db", export_serve_db(con))

message("\nweekly run complete")
dbDisconnect(con, shutdown = TRUE)
