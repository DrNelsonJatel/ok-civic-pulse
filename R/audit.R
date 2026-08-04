# audit.R — measuring the classifier against a human standard.
#
# HISTORY, because it changed the design twice:
#
# v1 (2026-08-03): bulk = claude-haiku-4-5, audit = claude-opus-5. The strata
# deliberately included posts the scope gate had SKIPPED, so recall could be
# measured rather than assumed. That decision paid for itself immediately —
# the audit found Haiku at kappa 0.39 on the is-local judgement, systematically
# under-calling "local" (14 misses vs 1 false positive, n=60), and found the
# gate discarding 20-27% of genuinely locally-actionable comments.
#
# v2 (2026-08-04): bulk promoted to claude-opus-5 and the gate opened to "all".
# A model cannot audit itself, so the audit tier is now HUMAN adjudication —
# which is the right standard anyway: a reviewer will want agreement against a
# domain expert, not against a second model.
#
# Because the gate no longer skips anything, the skipped strata are gone; the
# sample is now drawn at random, stratified by the model's own confidence so
# the uncertain tail is over-represented where disagreement actually lives.
#
# Set MODEL_AUDIT to a DIFFERENT model than MODEL_BULK if you want a
# model-vs-model check as well; audit_report() refuses to compare a model to
# itself.
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

HUMAN_CODER <- Sys.getenv("OKCP_HUMAN_CODER", "human:nelson")

audit_report <- function(con, bulk = MODEL_BULK, audit = HUMAN_CODER) {
  # Comparing a model against itself yields kappa 1.0 and means nothing.
  if (identical(coder_id_for(bulk), audit) || identical(bulk, audit)) {
    message("audit: bulk and audit coder are the same (", bulk,
            ") — a model cannot audit itself. Use human adjudication ",
            "(app/adjudicate.R) or set MODEL_AUDIT to a different model.")
    return(invisible(NULL))
  }
  s <- dbGetQuery(con, "SELECT post_id, stratum FROM audit_sample")
  if (!nrow(s)) { message("audit: no sample drawn"); return(invisible(NULL)) }
  # `audit` may be a bare coder_id (human:nelson) or a model name.
  a_coder <- if (grepl(":", audit)) audit else coder_id_for(audit)
  a <- post_label(con, a_coder)
  if (!nrow(a)) { message("audit: sample not yet coded by ", a_coder); return(invisible(NULL)) }
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

  cat("\n=== AUDIT:", a_coder, "vs", coder_id_for(bulk), "===\n")
  if (is.null(agree)) cat("no overlapping coded posts in the gated_in stratum yet\n")
  else cat(sprintf(
    "gated_in n=%d | is-local agreement %.1f%% (kappa %.2f) | top-code agreement %.1f%% | scope kappa %.2f\n",
    agree$n, 100*agree$local_agree, agree$local_kappa, 100*agree$code_agree, agree$scope_kappa))
  if (nrow(miss)) { cat("\nfalse negatives in SKIPPED strata:\n"); print(as.data.frame(miss)) }
  invisible(list(agreement = agree, missed = miss))
}
