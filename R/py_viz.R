# py_viz.R — optional Python visualisation, via reticulate.
#
# DELIBERATELY OUT OF THE DAILY PATH. This runs weekly. The daily ingest,
# classification, metrics and PDF are pure R + DuckDB and must stay that way:
# adding a Python environment to the daily job would put the free-CI story at
# the mercy of a torch wheel. Everything here is wrapped so that a missing
# interpreter, a missing package, or an outright failure produces a skipped
# artifact and a logged reason — never a broken daily run.
#
# ---------------------------------------------------------------------------
# PRIVACY CONSTRAINT — read before adding any new visual.
#
# The project's posture is that comment TEXT never leaves this machine. Several
# popular Python visualisations embed source documents directly in their HTML
# output so a reader can click a term and see example comments. That is exactly
# the redistribution the Castanet ToU prohibits.
#
#   SAFE TO PUBLISH  — visuals whose output contains only TERMS, topic labels,
#                      coordinates and counts:
#                      visualize_topics, visualize_barchart,
#                      visualize_topics_over_time, visualize_hierarchy,
#                      visualize_heatmap.
#   LOCAL ONLY       — visualize_documents (hover shows full comments) and
#                      scattertext (embeds the corpus by design). These are
#                      genuinely useful for analysis, so they are supported,
#                      but they write to a git-ignored directory and are never
#                      copied into the published site.
# ---------------------------------------------------------------------------
suppressMessages({library(dplyr); library(DBI)})

PY_PUBLIC_DIR <- "output/py"          # publishable artifacts
PY_LOCAL_DIR  <- "output/py_local"    # git-ignored; may contain comment text

py_ready <- function(modules = c("bertopic")) {
  if (!requireNamespace("reticulate", quietly = TRUE))
    return(list(ok = FALSE, reason = "reticulate not installed"))
  ok <- tryCatch(reticulate::py_available(initialize = TRUE), error = function(e) FALSE)
  if (!isTRUE(ok)) return(list(ok = FALSE, reason = "no usable Python interpreter"))
  missing <- modules[!vapply(modules, reticulate::py_module_available, logical(1))]
  if (length(missing))
    return(list(ok = FALSE, reason = paste("missing Python modules:",
                                           paste(missing, collapse = ", "))))
  list(ok = TRUE)
}

# BERTopic over time — the visual that answers "which issues rose as the
# campaign approached". Transformer embeddings handle short forum comments
# considerably better than bag-of-words LDA does.
bertopic_over_time <- function(con, out_dir = PY_PUBLIC_DIR, min_topic_size = 10L,
                               nr_bins = 20L, min_chars = 60L) {
  st <- py_ready(c("bertopic", "plotly"))
  if (!isTRUE(st$ok)) { message("bertopic: skipped — ", st$reason); return(invisible(st)) }

  d <- dbGetQuery(con, sprintf("
    SELECT post_id, body_local, posted_at FROM posts
     WHERE body_local IS NOT NULL AND length(body_local) >= %d
       AND posted_at IS NOT NULL ORDER BY posted_at", min_chars))
  if (nrow(d) < 100L) {
    message("bertopic: skipped — only ", nrow(d), " documents"); return(invisible(NULL))
  }
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  res <- tryCatch({
    bt <- reticulate::import("bertopic")
    model <- bt$BERTopic(min_topic_size = as.integer(min_topic_size),
                         calculate_probabilities = FALSE, verbose = FALSE)
    fit <- model$fit_transform(as.list(d$body_local))
    topics <- fit[[1]]

    # Terms-only figures. These are safe to publish: nothing in the HTML
    # contains a comment.
    model$visualize_barchart(top_n_topics = as.integer(12))$write_html(
      file.path(out_dir, "bertopic_terms.html"))
    model$visualize_hierarchy()$write_html(file.path(out_dir, "bertopic_hierarchy.html"))

    tot <- model$topics_over_time(as.list(d$body_local), as.list(as.character(d$posted_at)),
                                  nr_bins = as.integer(nr_bins))
    model$visualize_topics_over_time(tot, top_n_topics = as.integer(10))$write_html(
      file.path(out_dir, "bertopic_over_time.html"))

    info <- model$get_topic_info()
    utils::write.csv(reticulate::py_to_r(info), file.path(out_dir, "bertopic_topics.csv"),
                     row.names = FALSE)
    list(ok = TRUE, n_docs = nrow(d), files = list.files(out_dir, full.names = TRUE))
  }, error = function(e) list(ok = FALSE, reason = conditionMessage(e)))

  if (isTRUE(res$ok)) message("bertopic: wrote ", length(res$files), " artifacts to ", out_dir)
  else message("bertopic: failed — ", res$reason)
  invisible(res)
}

# scattertext: LOCAL ONLY. It embeds the corpus so a reader can click a term
# and read the comments behind it — genuinely the best way to see what
# separates locally-actionable discourse from the rest, and genuinely
# un-publishable under a no-redistribution licence.
scattertext_local <- function(con, out_dir = PY_LOCAL_DIR, min_chars = 60L) {
  st <- py_ready(c("scattertext", "spacy"))
  if (!isTRUE(st$ok)) { message("scattertext: skipped — ", st$reason); return(invisible(st)) }
  d <- dbGetQuery(con, sprintf("
    SELECT p.post_id, p.body_local,
           CASE WHEN max(CASE WHEN i.scope IN ('local','ballot','shared') THEN 1 ELSE 0 END) = 1
                THEN 'locally actionable' ELSE 'provincial/federal' END AS category
      FROM posts p JOIN post_issues i ON i.post_id = p.post_id
     WHERE i.coder_id <> 'keyword-sieve' AND i.issue_code <> 'none'
       AND p.body_local IS NOT NULL AND length(p.body_local) >= %d
     GROUP BY p.post_id, p.body_local", min_chars))
  if (nrow(d) < 100L) { message("scattertext: skipped — too few documents"); return(invisible(NULL)) }
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  res <- tryCatch({
    stx <- reticulate::import("scattertext")
    spacy <- reticulate::import("spacy")
    nlp <- spacy$load("en_core_web_sm")
    pd <- reticulate::import("pandas")
    df <- pd$DataFrame(list(text = as.list(d$body_local), category = as.list(d$category)))
    corpus <- stx$CorpusFromPandas(df, category_col = "category", text_col = "text",
                                   nlp = nlp)$build()
    html <- stx$produce_scattertext_explorer(
      corpus, category = "locally actionable",
      category_name = "Locally actionable", not_category_name = "Provincial/federal",
      minimum_term_frequency = as.integer(5), width_in_pixels = as.integer(1000))
    writeLines(html, file.path(out_dir, "scattertext_scope.html"))
    list(ok = TRUE, file = file.path(out_dir, "scattertext_scope.html"))
  }, error = function(e) list(ok = FALSE, reason = conditionMessage(e)))

  if (isTRUE(res$ok))
    message("scattertext: wrote ", res$file,
            "  [LOCAL ONLY — contains comment text, do not publish]")
  else message("scattertext: failed — ", res$reason)
  invisible(res)
}

# One-time environment setup, documented rather than run automatically:
#   reticulate::virtualenv_create("okcp")
#   reticulate::virtualenv_install("okcp", c("bertopic", "scattertext", "spacy", "plotly"))
#   reticulate::use_virtualenv("okcp")
#   system("python -m spacy download en_core_web_sm")
# BERTopic pulls torch (~2 GB), which is precisely why this stays weekly and
# out of CI.
py_setup_hint <- function() {
  cat(paste(
    "One-time Python setup (weekly enrichment only, not needed for the daily run):",
    '  reticulate::virtualenv_create("okcp")',
    '  reticulate::virtualenv_install("okcp", c("bertopic","scattertext","spacy","plotly"))',
    '  reticulate::use_virtualenv("okcp")',
    '  system("python -m spacy download en_core_web_sm")',
    sep = "\n"), "\n")
}
