# scaling.R — Latent Semantic Scaling (LSX).
#
# WHY THIS EXISTS: lexicon sentiment failed on this corpus. Measured on the
# pilot, sentimentr scored model-labelled "oppose" comments at +0.026 and
# "support" at +0.094 — both positive, barely separated, useless as a signal.
# General-purpose polarity lexicons are built for product reviews, not for
# sarcastic municipal argument.
#
# LSX replaces it with a scale built FROM THIS CORPUS. You supply a handful of
# seed words defining the poles; it fits word embeddings on the actual
# documents and projects every comment onto that axis. The result is a
# domain-specific dimension that means something concrete — not a generic
# "positivity" score that means nothing here.
suppressMessages({library(quanteda); library(dplyr); library(DBI)})

# Seed sets define the poles. These are deliberately short: LSX propagates from
# seeds through the embedding, so a few unambiguous anchors beat a long list of
# arguable ones. Negative weights mark the low end of the scale.
LSX_SCALES <- list(
  # Does the comment want more built, or less?
  development = list(
    label = "Restrictive <-> Pro-development",
    seeds = c(density = 1, growth = 1, build = 1, housing = 1, supply = 1, approve = 1,
              sprawl = -1, overdevelopment = -1, greedy = -1, character = -1,
              traffic = -1, congestion = -1)),
  # Confidence in the local institution, as distinct from mood.
  institutional_trust = list(
    label = "Distrust <-> Trust in local government",
    seeds = c(transparent = 1, accountable = 1, listened = 1, consultation = 1,
              competent = 1, responsive = 1,
              corrupt = -1, incompetent = -1, wasted = -1, ignored = -1,
              arrogant = -1, clueless = -1))
)

fit_lss <- function(dfm, scale = "development", k = 300L, min_docs = 300L) {
  if (!requireNamespace("LSX", quietly = TRUE))
    return(list(ok = FALSE, reason = "LSX not installed"))
  sc <- LSX_SCALES[[scale]]
  if (is.null(sc)) return(list(ok = FALSE, reason = paste("unknown scale:", scale)))
  n <- quanteda::ndoc(dfm)
  if (n < min_docs)
    return(list(ok = FALSE, reason = sprintf(
      "%d documents — LSX needs ~%d+ for a stable embedding", n, min_docs)))

  # Only seeds present in the vocabulary can anchor anything. Silently missing
  # seeds are the main way an LSX scale ends up meaningless, so they are
  # reported rather than dropped quietly.
  present <- intersect(names(sc$seeds), colnames(dfm))
  missing <- setdiff(names(sc$seeds), colnames(dfm))
  if (length(present) < 4)
    return(list(ok = FALSE, reason = sprintf(
      "only %d of %d seed words occur in the corpus (%s) — scale would be arbitrary",
      length(present), length(sc$seeds), paste(missing, collapse = ", "))))

  suppressMessages(library(LSX))
  seedvec <- sc$seeds[present]
  fit <- tryCatch(LSX::textmodel_lss(dfm, seeds = seedvec, k = min(k, quanteda::nfeat(dfm) - 1L),
                                     cache = FALSE),
                  error = function(e) e)
  if (inherits(fit, "error")) return(list(ok = FALSE, reason = conditionMessage(fit)))
  list(ok = TRUE, fit = fit, scale = scale, label = sc$label,
       seeds_used = present, seeds_missing = missing)
}

# Project documents onto the fitted scale and attach them to issue codes, so
# the axis can be read per issue ("which issues attract the most restrictive
# framing?") rather than only per document.
score_documents <- function(lss, dfm, corp) {
  if (!isTRUE(lss$ok)) return(NULL)
  # predict() is an S3 method registered by LSX, not an exported function —
  # LSX::predict() errors with "not an exported object".
  s <- stats::predict(lss$fit, newdata = dfm)
  tibble::tibble(post_id = quanteda::docvars(corp)$post_id,
                 day = as.Date(quanteda::docvars(corp)$posted_at),
                 lss_score = as.numeric(s))
}

issue_scale_profile <- function(con, scores, min_posts = 5L) {
  if (is.null(scores)) return(tibble::tibble())
  ic <- dbGetQuery(con, "
    SELECT post_id, issue_code, scope FROM post_issues
     WHERE coder_id <> 'keyword-sieve' AND issue_code <> 'none'")
  inner_join(ic, scores, by = "post_id") |>
    group_by(issue_code, scope) |>
    summarise(n = n(), mean_score = mean(lss_score, na.rm = TRUE),
              sd_score = sd(lss_score, na.rm = TRUE), .groups = "drop") |>
    filter(n >= min_posts) |> arrange(mean_score)
}

# The words the fitted scale actually keyed on. Always inspect these before
# believing a scale — if the top terms look arbitrary, the scale is arbitrary.
scale_terms <- function(lss, n = 20L) {
  if (!isTRUE(lss$ok)) return(NULL)
  b <- stats::coef(lss$fit)   # S3 method, same reason as predict() above
  tibble::tibble(term = names(b), weight = as.numeric(b)) |>
    arrange(desc(weight)) |>
    slice(c(1:n, (dplyr::n() - n + 1):dplyr::n())) |>
    mutate(pole = ifelse(weight > 0, "high end", "low end"))
}
