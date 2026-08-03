# topics.R — seeded topic models driven by the codebook.
#
# WHY SEEDED, not plain LDA: the codebook already encodes what we are looking
# for. Unsupervised LDA would rediscover some of it, mislabel the rest, and
# produce topics nobody can map back to a level of government. Seeding anchors
# topics to codebook codes so the output is directly comparable to the model
# coding — and the RESIDUAL topics (fitted without seeds) are the payoff: they
# capture what the corpus is discussing that the codebook has no code for.
# Those are the candidate new codes that make the codebook evolve.
#
# CORPUS SIZE WARNING: topic models need thousands of documents. At pilot scale
# (hundreds) the fits are unstable and must be read as exploratory only. That
# check is enforced below rather than left to the reader.
suppressMessages({library(quanteda); library(dplyr); library(DBI); library(stringr)})

MIN_DOCS_STABLE <- 2000L

# Codebook seeds are ICU regexes for the sieve. Topic models want plain word
# and phrase patterns, so strip the regex machinery. Derived at runtime rather
# than stored, so the keywords can never drift from the seeds they come from.
seeds_to_keywords <- function(seed_regex) {
  parts <- str_split(seed_regex, "\\|")[[1]]
  kw <- parts |>
    str_remove_all("\\(\\?[<!=][^)]*\\)") |>   # lookahead / lookbehind groups
    str_remove_all("\\\\b") |>                  # word boundaries
    str_replace_all("\\[- ?\\]", " ") |>        # [- ] optional hyphen/space
    str_replace_all("\\[[^]]*\\]", " ") |>      # any other character class
    str_remove_all("[()?*+^$]") |>
    str_squish()
  kw <- kw[nzchar(kw) & !str_detect(kw, "[\\\\{}]")]
  unique(tolower(kw))
}

codebook_dictionary <- function(issues = ISSUES) {
  lst <- setNames(lapply(issues$seeds, seeds_to_keywords), issues$code)
  lst <- lst[lengths(lst) > 0]
  quanteda::dictionary(lst)
}

# ---- corpus -----------------------------------------------------------------
build_corpus <- function(con, min_chars = 60L) {
  d <- dbGetQuery(con, sprintf("
    SELECT p.post_id, p.body_local, p.posted_at, p.forum_id, p.thread_t
      FROM posts p
     WHERE p.body_local IS NOT NULL AND length(p.body_local) >= %d", min_chars))
  if (!nrow(d)) return(NULL)
  # Attach the model's own labels as document variables so topics can be
  # cross-tabulated against the codebook coding.
  lab <- dbGetQuery(con, "
    SELECT post_id,
           max(CASE WHEN scope IN ('local','ballot','shared') THEN 1 ELSE 0 END) AS is_local,
           arg_max(issue_code, COALESCE(salience,0)) AS top_code
      FROM post_issues WHERE coder_id <> 'keyword-sieve' GROUP BY post_id")
  d <- left_join(d, lab, by = "post_id")
  corp <- quanteda::corpus(d$body_local,
                           docvars = d[, c("post_id","posted_at","forum_id","is_local","top_code")])
  quanteda::docnames(corp) <- sprintf("p%.0f", d$post_id)
  corp
}

# Forum-specific noise on top of the standard English stoplist. phpBB renders
# quotes as "<handle> wrote:" followed by a date, so without these the topic
# model spends whole topics on quoting mechanics — the first pilot fit returned
# "wrote, aug, 1st, jul, 31st" as a topic, which is a citation artifact rather
# than anything anyone discussed.
OKCP_STOP <- c(
  "wrote", "said", "quote", "originally", "posted", "edit", "edited",
  "jan","feb","mar","apr","jun","jul","aug","sep","sept","oct","nov","dec",
  "january","february","march","april","june","july","august","september",
  "october","november","december",
  "st","nd","rd","th",           # ordinal suffixes left by number removal
  "castanet", "forum", "thread", "post", "lol", "yeah", "gonna", "gotta",
  "just", "like", "get", "got", "one", "also", "even", "much", "many",
  "can", "will", "well", "way", "back", "still", "really", "thing", "things")

build_dfm <- function(corp, min_termfreq = 3L, min_docfreq = 2L, extra_stop = OKCP_STOP) {
  quanteda::tokens(corp, remove_punct = TRUE, remove_numbers = TRUE,
                   remove_symbols = TRUE, remove_url = TRUE) |>
    quanteda::tokens_tolower() |>
    quanteda::tokens_remove(c(quanteda::stopwords("en"), extra_stop), min_nchar = 3) |>
    # Ordinal-date leftovers like "31st" survive remove_numbers because they
    # are alphanumeric; kill them by pattern.
    quanteda::tokens_remove("^[0-9]+(st|nd|rd|th)$", valuetype = "regex") |>
    quanteda::dfm() |>
    quanteda::dfm_trim(min_termfreq = min_termfreq, min_docfreq = min_docfreq)
}

# ---- seeded LDA with residual topics ---------------------------------------
#
# `residual` is the important argument: it fits N extra unseeded topics. Those
# absorb whatever the codebook does not describe, which is exactly the evidence
# a codebook revision needs.
fit_seeded_topics <- function(dfm, dict = codebook_dictionary(), residual = 6L,
                              max_iter = 1500L, seed = 42L) {
  n <- quanteda::ndoc(dfm)
  stable <- n >= MIN_DOCS_STABLE
  if (!stable)
    warning(sprintf(paste("topic model fitted on %d documents; seeded LDA is unstable",
                          "below ~%d. Treat as exploratory, not as a finding."),
                    n, MIN_DOCS_STABLE), call. = FALSE)
  suppressMessages(library(seededlda))
  set.seed(seed)
  fit <- seededlda::textmodel_seededlda(dfm, dictionary = dict, residual = residual,
                                        max_iter = max_iter, verbose = FALSE)
  list(fit = fit, n_docs = n, stable = stable, residual = residual)
}

# Top terms per topic, tidy. Residual topics are named "other1".."otherN" by
# seededlda and are flagged here so they are never mistaken for codebook codes.
topic_terms <- function(tm, n = 12L) {
  tt <- seededlda::terms(tm$fit, n)
  out <- lapply(colnames(tt), function(k) tibble::tibble(
    topic = k, rank = seq_len(nrow(tt)), term = tt[, k]))
  dplyr::bind_rows(out) |>
    dplyr::mutate(is_residual = grepl("^other", topic),
                  label = ifelse(is_residual, paste0("[unseeded] ", topic), issue_label(topic)))
}

# Document-topic proportions joined back to dates, for prevalence over time.
topic_prevalence <- function(tm, corp) {
  theta <- tm$fit$theta
  dv <- quanteda::docvars(corp)
  tibble::as_tibble(theta) |>
    dplyr::mutate(post_id = dv$post_id, day = as.Date(dv$posted_at),
                  is_local = dv$is_local) |>
    tidyr::pivot_longer(-c(post_id, day, is_local),
                        names_to = "topic", values_to = "gamma")
}

# The codebook-evolution payoff: which unseeded topics carry real mass, and
# what are they about? A residual topic with high prevalence is a gap in the
# codebook, and its top terms are the draft definition.
residual_report <- function(tm, n_terms = 15L) {
  tt <- topic_terms(tm, n_terms) |> dplyr::filter(is_residual)
  mass <- colMeans(tm$fit$theta)
  res <- tibble::tibble(topic = names(mass), mean_gamma = as.numeric(mass)) |>
    dplyr::filter(grepl("^other", topic)) |>
    dplyr::arrange(dplyr::desc(mean_gamma))
  res |> dplyr::left_join(
    tt |> dplyr::group_by(topic) |>
      dplyr::summarise(top_terms = paste(term, collapse = ", "), .groups = "drop"),
    by = "topic")
}

# ---- keyATM: same seeds, but with covariates -------------------------------
#
# keyATM's advantage over seededlda is that topic prevalence can be modelled as
# a function of document covariates — here, election phase and jurisdiction.
# That answers "which issues rise as the campaign approaches" as a model
# parameter rather than an eyeballed line chart.
fit_keyatm_covariate <- function(dfm, corp, dict = codebook_dictionary(),
                                 n_residual = 5L, iter = 500L, seed = 42L) {
  if (!requireNamespace("keyATM", quietly = TRUE))
    return(list(ok = FALSE, reason = "keyATM not installed"))
  suppressMessages(library(keyATM))
  if (quanteda::ndoc(dfm) < 300L)
    return(list(ok = FALSE, reason = sprintf("only %d documents — keyATM covariate model needs more",
                                             quanteda::ndoc(dfm))))
  kd  <- keyATM::keyATM_read(texts = dfm)
  dv  <- quanteda::docvars(corp)
  cov <- data.frame(phase = factor(vapply(as.Date(dv$posted_at), election_phase, "")))
  # keyATM silently fails on keywords absent from the vocabulary, so restrict
  # each topic's keywords to terms that actually occur, and drop topics left
  # with none.
  vocab <- colnames(dfm)
  keys <- lapply(quanteda::as.list(dict), function(w) intersect(tolower(w), vocab))
  keys <- keys[lengths(keys) > 0]
  if (!length(keys)) return(list(ok = FALSE, reason = "no codebook keyword occurs in the vocabulary"))
  fit <- tryCatch(keyATM::keyATM(docs = kd, no_keyword_topics = n_residual,
                                 keywords = keys, model = "covariates",
                                 model_settings = list(covariates_data = cov,
                                                       covariates_formula = ~ phase),
                                 options = list(seed = seed, iterations = iter, verbose = FALSE)),
                  error = function(e) e)
  if (inherits(fit, "error")) return(list(ok = FALSE, reason = conditionMessage(fit)))
  list(ok = TRUE, fit = fit, covariates = cov)
}
