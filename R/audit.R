# audit.R — stratified audit of the cheap bulk tier.
#
# Bulk coding runs on claude-haiku-4-5 because a free pilot has to be free.
# That is only defensible if its accuracy is MEASURED, so a stratified sample
# is re-coded by claude-opus-5 and the two are compared.
#
# The strata matter as much as the sample size. Auditing only the posts the
# gate let through would measure precision and silently ignore recall — the
# posts the gate SKIPPED are exactly where a false negative hides. So the
# sample deliberately draws from all three populations:
#
#   gated_in             — model-coded. Measures bulk-vs-audit agreement.
#   skipped_out_of_scope — sieve found only federal/provincial codes.
#                          Measures the cost of the 27% gate cut.
#   skipped_no_code      — sieve found nothing at all.
#                          Measures the sieve's own miss rate.
suppressMessages({library(dplyr); library(DBI)})

# Draw a sample. Deterministic given `seed` so a reported accuracy figure can
# be reproduced from the repo.
audit_sample <- function(con, n_in = 200L, n_out = 60L, n_none = 40L,
                         sample_id = format(Sys.Date()), seed = 42L) {
  set.seed(seed)
  pred <- gate_predicate()

  pool <- dbGetQuery(con, sprintf("
    SELECT p.post_id,
           CASE WHEN g.gate_open THEN 'gated_in'
                WHEN g.any_code  THEN 'skipped_out_of_scope'
                ELSE 'skipped_no_code' END AS stratum
      FROM posts p
      JOIN (SELECT post_id,
                   max(CASE WHEN issue_code <> 'none' THEN 1 ELSE 0 END) = 1 AS any_code,
                   %s AS gate_open
              FROM post_issues WHERE coder_id = '%s' GROUP BY post_id) g
        ON g.post_id = p.post_id
     WHERE p.body_local IS NOT NULL AND length(p.body_local) > 20", pred, SIEVE_CODER))
  if (!nrow(pool)) { message("audit: empty pool"); return(invisible(0L)) }

  want <- c(gated_in = n_in, skipped_out_of_scope = n_out, skipped_no_code = n_none)
  drawn <- bind_rows(lapply(names(want), function(s) {
    cand <- filter(pool, stratum == s)
    if (!nrow(cand)) return(NULL)
    slice_sample(cand, n = min(nrow(cand), want[[s]]))
  }))
  drawn$sample_id <- sample_id
  drawn$drawn_at  <- Sys.time()

  db_upsert(con, "audit_sample", as.data.frame(drawn), "post_id")
  message(sprintf("audit: drew %d posts (%s)", nrow(drawn),
                  paste(sprintf("%s=%d", names(table(drawn$stratum)),
                                as.integer(table(drawn$stratum))), collapse = ", ")))
  invisible(nrow(drawn))
}

# Re-code the sample with the high tier. Runs synchronously — the sample is
# small by construction, and a batch round trip isn't worth the latency here.
audit_run <- function(con, model = MODEL_AUDIT, batch_size = CLASSIFY_BATCH) {
  coder <- coder_id_for(model)
  todo <- dbGetQuery(con, sprintf("
    SELECT p.post_id, p.body_local, t.title
      FROM audit_sample s
      JOIN posts p ON p.post_id = s.post_id
      LEFT JOIN threads t ON t.t_id = p.thread_t
     WHERE p.post_id NOT IN (SELECT post_id FROM post_issues WHERE coder_id = '%s')
     ORDER BY p.post_id", coder))
  if (!nrow(todo)) { message("audit: sample already coded"); return(invisible(0L)) }
  message(sprintf("audit: coding %d posts with %s", nrow(todo), model))

  n_ok <- 0L; usage <- c(in_ = 0, out = 0, cr = 0, cw = 0)
  chunks <- split(todo, ceiling(seq_len(nrow(todo)) / batch_size))
  for (k in seq_along(chunks)) {
    res <- tryCatch({
      out <- anthropic_req("/v1/messages") |>
        req_body_json(build_body(chunks[[k]], model, AUDIT_EFFORT), auto_unbox = TRUE) |>
        req_perform() |> resp_body_json()
      list(rows = results_to_rows(parse_classify_response(out), coder), u = out$usage)
    }, error = function(e) { message("  audit batch ", k, " ERR: ", conditionMessage(e)); NULL })
    if (is.null(res)) next
    if (nrow(res$rows)) {
      db_upsert(con, "post_issues", as.data.frame(res$rows), c("post_id","issue_code","coder_id"))
      n_ok <- n_ok + nrow(chunks[[k]])
    }
    usage <- usage + c(res$u$input_tokens %||% 0, res$u$output_tokens %||% 0,
                       res$u$cache_read_input_tokens %||% 0,
                       res$u$cache_creation_input_tokens %||% 0)
  }
  report_cost(model, usage, n_ok, discount = 1)
  invisible(n_ok)
}

# Cohen's kappa — chance-corrected agreement. Raw percent agreement flatters
# any label set this skewed toward 'local', so kappa is the number to report.
cohens_kappa <- function(a, b) {
  lv <- union(unique(a), unique(b))
  tab <- table(factor(a, lv), factor(b, lv))
  n <- sum(tab); if (n == 0) return(NA_real_)
  po <- sum(diag(tab)) / n
  pe <- sum(rowSums(tab) * colSums(tab)) / n^2
  if (isTRUE(all.equal(pe, 1))) return(NA_real_)
  (po - pe) / (1 - pe)
}

# Collapse a post's codes to one label per coder: does it contain a
# locally-actionable issue, and what is its highest-salience issue code?
post_label <- function(con, coder) {
  dbGetQuery(con, sprintf("
    SELECT post_id,
           max(CASE WHEN scope IN ('local','ballot','shared') THEN 1 ELSE 0 END) AS is_local,
           arg_max(issue_code, COALESCE(salience, 0)) AS top_code,
           arg_max(scope, COALESCE(salience, 0))      AS top_scope
      FROM post_issues WHERE coder_id = '%s' GROUP BY post_id", coder))
}

audit_report <- function(con, bulk = MODEL_BULK, audit = MODEL_AUDIT) {
  s <- dbGetQuery(con, "SELECT post_id, stratum FROM audit_sample")
  if (!nrow(s)) { message("audit: no sample drawn"); return(invisible(NULL)) }
  a <- post_label(con, coder_id_for(audit))
  if (!nrow(a)) { message("audit: sample not yet coded by ", audit); return(invisible(NULL)) }
  b <- post_label(con, coder_id_for(bulk))

  joined <- s |> left_join(a, by = "post_id", suffix = c("", ".a")) |>
    rename(a_is_local = is_local, a_top = top_code, a_scope = top_scope) |>
    left_join(b, by = "post_id") |>
    rename(b_is_local = is_local, b_top = top_code, b_scope = top_scope)

  # Precision-side: where both tiers coded the same post.
  ov <- filter(joined, stratum == "gated_in", !is.na(a_is_local), !is.na(b_is_local))
  agree <- if (nrow(ov)) list(
    n              = nrow(ov),
    local_agree    = mean(ov$a_is_local == ov$b_is_local),
    local_kappa    = cohens_kappa(ov$a_is_local, ov$b_is_local),
    code_agree     = mean(ov$a_top == ov$b_top, na.rm = TRUE),
    scope_kappa    = cohens_kappa(ov$a_scope, ov$b_scope)
  ) else NULL

  # Recall-side: of the posts the gate SKIPPED, how many does the high tier say
  # were locally actionable after all? This is the number that decides whether
  # the 27% cost saving was worth taking.
  miss <- joined |> filter(stratum != "gated_in", !is.na(a_is_local)) |>
    group_by(stratum) |>
    summarise(n = n(), false_negatives = sum(a_is_local == 1),
              fn_rate = mean(a_is_local == 1), .groups = "drop")

  cat("\n=== AUDIT:", audit, "vs", bulk, "===\n")
  if (is.null(agree)) cat("no overlapping coded posts in the gated_in stratum yet\n")
  else cat(sprintf(
    "gated_in n=%d | is-local agreement %.1f%% (kappa %.2f) | top-code agreement %.1f%% | scope kappa %.2f\n",
    agree$n, 100*agree$local_agree, agree$local_kappa, 100*agree$code_agree, agree$scope_kappa))
  if (nrow(miss)) { cat("\nfalse negatives in SKIPPED strata:\n"); print(as.data.frame(miss)) }
  invisible(list(agreement = agree, missed = miss))
}
