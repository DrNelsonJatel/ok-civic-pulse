# sna.R — reply and co-participation networks, descriptives, ERGM.
#
# Two networks, deliberately:
#
#  1. REPLY — directed, from the blockquote cite. Unambiguous but SPARSE:
#     quoting is the only reply signal in a flat phpBB thread, and in the
#     drought-sna corpus that yielded 763 edges across 2,505 posts (~0.3 per
#     post). Treating that as the whole interaction structure understates
#     connectivity badly.
#  2. CO-PARTICIPATION — undirected actor-actor, weighted by shared threads.
#     Dense, and captures "these people show up in the same arguments" even
#     when nobody quotes anybody.
#
# Report both. A claim that rests on the reply graph alone should say so.
suppressMessages({library(igraph); library(dplyr); library(DBI)})

reply_graph <- function(con, since = NULL, issue = NULL, drop_staff = TRUE) {
  w <- c("e.from_user_id IS NOT NULL", "e.to_user_id IS NOT NULL")
  if (!is.null(since)) w <- c(w, sprintf("e.posted_at >= DATE '%s'", format(since)))
  if (!is.null(issue)) w <- c(w, sprintf(
    "e.from_post_id IN (SELECT post_id FROM post_issues
       WHERE issue_code = '%s' AND coder_id <> 'keyword-sieve')", issue))
  el <- dbGetQuery(con, sprintf(
    "SELECT e.from_user_id, e.to_user_id, e.posted_at
       FROM edges_reply e WHERE %s", paste(w, collapse = " AND ")))
  if (drop_staff) {
    staff <- dbGetQuery(con, "SELECT user_id FROM actors WHERE is_staff")$user_id
    el <- filter(el, !(from_user_id %in% staff), !(to_user_id %in% staff))
  }
  # Self-loops are real in the data — people quote their own earlier post to
  # continue an argument — but they are not interaction, and ergm rejects a
  # network containing them. Drop them here rather than in each consumer.
  el <- filter(el, from_user_id != to_user_id)
  if (!nrow(el)) return(make_empty_graph(directed = TRUE))
  graph_from_data_frame(
    el |> count(from_user_id, to_user_id, name = "weight") |>
      mutate(across(1:2, as.character)),
    directed = TRUE)
}

copart_graph <- function(con, since = NULL, min_shared = 2L) {
  w <- "p.author_user_id IS NOT NULL"
  if (!is.null(since)) w <- paste(w, sprintf("AND p.posted_at >= DATE '%s'", format(since)))
  bip <- dbGetQuery(con, sprintf(
    "SELECT DISTINCT p.author_user_id, p.thread_t FROM posts p WHERE %s", w))
  if (nrow(bip) < 2) return(make_empty_graph(directed = FALSE))
  bi <- graph_from_data_frame(
    mutate(bip, author_user_id = as.character(author_user_id),
                thread_t = paste0("t", thread_t)), directed = FALSE)
  V(bi)$type <- startsWith(V(bi)$name, "t")
  proj <- bipartite_projection(bi, which = "false")
  # Two actors must share at least `min_shared` threads. Without this every pair
  # in a single busy thread becomes an edge and the projection is near-complete
  # — visually and statistically useless.
  #
  # NOTE this threshold is ARBITRARY. backbone_copart() below replaces it with a
  # statistical test; prefer that where the corpus is large enough.
  # subgraph.edges() was deprecated in igraph 2.1.0.
  subgraph_from_edges(proj, which(E(proj)$weight >= min_shared), delete.vertices = TRUE)
}

# Statistically validated backbone of the co-participation projection.
#
# A fixed weight cutoff ("share >= 2 threads") is a judgement call with no
# justification: it treats a tie between two prolific posters, who co-occur by
# sheer volume, the same as a tie between two occasional ones, for whom
# co-occurrence is genuinely informative. The disparity filter tests each edge
# against a null model of the node's own weight distribution and keeps only
# ties that are stronger than chance given how active that actor is.
backbone_copart <- function(con, since = NULL, alpha = 0.05) {
  if (!requireNamespace("backbone", quietly = TRUE))
    return(list(ok = FALSE, reason = "backbone not installed"))
  w <- "p.author_user_id IS NOT NULL"
  if (!is.null(since)) w <- paste(w, sprintf("AND p.posted_at >= DATE '%s'", format(since)))
  bip <- dbGetQuery(con, sprintf(
    "SELECT DISTINCT p.author_user_id, p.thread_t FROM posts p WHERE %s", w))
  if (nrow(bip) < 20) return(list(ok = FALSE, reason = "too few actor-thread pairs"))
  m <- table(as.character(bip$author_user_id), as.character(bip$thread_t))
  proj <- tcrossprod(as.matrix(m))          # actor x actor co-participation counts
  diag(proj) <- 0
  # backbone::disparity() returns a binary MATRIX, not an igraph object — there
  # is no `class` argument in this version. Convert here.
  # disparity() is deprecated in favour of backbone_from_weighted().
  bb <- tryCatch(
    backbone::backbone_from_weighted(proj, model = "disparity",
                                     alpha = alpha, narrative = FALSE),
    error = function(e) tryCatch(backbone::disparity(proj, alpha = alpha,
                                                    narrative = FALSE),
                                 error = function(e2) e2))
  if (inherits(bb, "error")) return(list(ok = FALSE, reason = conditionMessage(bb)))
  g <- igraph::graph_from_adjacency_matrix(bb, mode = "undirected", diag = FALSE)
  g <- igraph::delete_vertices(g, which(igraph::degree(g) == 0))
  list(ok = TRUE, graph = g, alpha = alpha,
       n_before = sum(proj > 0) / 2, n_after = igraph::gsize(g))
}

# Who holds the network together. Reported on HASHED actor keys only — this is
# structural position, never an identification of a person.
actor_centrality <- function(g, top = 15L) {
  if (gorder(g) == 0) return(tibble::tibble())
  tibble::tibble(
    actor      = V(g)$name,
    in_degree  = as.integer(degree(g, mode = "in")),
    out_degree = as.integer(degree(g, mode = "out")),
    betweenness = round(betweenness(g, directed = is_directed(g)), 1),
    eigen      = round(eigen_centrality(g, directed = FALSE)$vector, 3)
  ) |> dplyr::arrange(dplyr::desc(betweenness)) |> head(top)
}

# Community structure, with the caveat that matters: modularity on a sparse
# quote-reply graph splits largely along THREADS, not along stable factions.
communities_summary <- function(g) {
  gg <- if (is_directed(g)) as_undirected(g, mode = "collapse") else g
  comp <- components(gg)
  giant <- induced_subgraph(gg, which(comp$membership == which.max(comp$csize)))
  cl <- cluster_louvain(giant)
  tibble::tibble(community = seq_along(sizes(cl)),
                 members = as.integer(sizes(cl))) |>
    dplyr::arrange(dplyr::desc(members)) |>
    dplyr::mutate(share = members / sum(members),
                  modularity = round(modularity(cl), 3))
}

net_descriptives <- function(g) {
  if (gorder(g) == 0) return(list(n = 0L, m = 0L))
  gg <- if (is_directed(g)) as_undirected(g, mode = "collapse") else g
  comp <- components(gg)
  giant <- induced_subgraph(gg, which(comp$membership == which.max(comp$csize)))
  list(
    n = gorder(g), m = gsize(g),
    density = edge_density(g),
    n_components = comp$no,
    giant_frac = max(comp$csize) / gorder(g),
    transitivity = transitivity(giant, "global"),
    mean_dist = mean_distance(giant),
    reciprocity = if (is_directed(g)) reciprocity(g) else NA_real_,
    n_communities = length(unique(cluster_louvain(giant)$membership))
  )
}

# Small-world sigma against Erdos-Renyi graphs of matched size and density.
small_world <- function(g, n_rand = 30L) {
  gg <- if (is_directed(g)) as_undirected(g, mode = "collapse") else g
  comp <- components(gg); giant <- induced_subgraph(gg, which(comp$membership == which.max(comp$csize)))
  if (gorder(giant) < 10) return(list(sigma = NA_real_))
  sims <- replicate(n_rand, {
    r <- sample_gnm(gorder(giant), gsize(giant))
    # Random graphs at this density are frequently disconnected; average over
    # reachable pairs so L is defined rather than Inf.
    c(C = transitivity(r, "global"), L = mean_distance(r, unconnected = TRUE))
  })
  Cr <- mean(sims["C", ], na.rm = TRUE); Lr <- mean(sims["L", ], na.rm = TRUE)
  Co <- transitivity(giant, "global");   Lo <- mean_distance(giant)
  list(C_obs = Co, L_obs = Lo, C_rand = Cr, L_rand = Lr,
       sigma = (Co / Cr) / (Lo / Lr))
}

# ---- LONGITUDINAL: how the network changes over time ------------------------
#
# A single snapshot cannot distinguish a stable community from one that
# reassembles from different people every month. These are cumulative AND
# per-window measures: cumulative shows whether the network is consolidating,
# per-window shows who is actually active now.
network_timeline <- function(con, by = "month", min_edges = 10L, drop_staff = TRUE) {
  el <- dbGetQuery(con, "
    SELECT from_user_id, to_user_id, posted_at FROM edges_reply
     WHERE from_user_id IS NOT NULL AND to_user_id IS NOT NULL
       AND from_user_id <> to_user_id AND posted_at IS NOT NULL")
  if (drop_staff) {
    staff <- dbGetQuery(con, "SELECT user_id FROM actors WHERE is_staff")$user_id
    el <- filter(el, !(from_user_id %in% staff), !(to_user_id %in% staff))
  }
  if (!nrow(el)) return(tibble::tibble())
  el$win <- as.Date(cut(as.Date(el$posted_at), by))
  wins <- sort(unique(el$win))

  meas <- function(d) {
    if (nrow(d) < min_edges) return(NULL)
    g <- graph_from_data_frame(
      d |> count(from_user_id, to_user_id, name = "w") |> mutate(across(1:2, as.character)),
      directed = TRUE)
    gu <- as_undirected(g, mode = "collapse")
    cm <- components(gu)
    gt <- induced_subgraph(gu, which(cm$membership == which.max(cm$csize)))
    tibble::tibble(actors = gorder(g), ties = gsize(g),
                   density = edge_density(g),
                   giant_frac = max(cm$csize) / gorder(g),
                   transitivity = transitivity(gt, "global"),
                   reciprocity = reciprocity(g),
                   mean_degree = 2 * gsize(g) / gorder(g))
  }

  bind_rows(lapply(wins, function(w) {
    a <- meas(filter(el, win == w))
    b <- meas(filter(el, win <= w))          # cumulative
    if (is.null(a) && is.null(b)) return(NULL)
    bind_rows(
      if (!is.null(a)) mutate(a, window = w, view = "per-window"),
      if (!is.null(b)) mutate(b, window = w, view = "cumulative"))
  }))
}

# Actor turnover: are the same people arguing month to month, or a new cast?
actor_turnover <- function(con, by = "month") {
  d <- dbGetQuery(con, "
    SELECT author_user_id AS a, posted_at FROM posts
     WHERE author_user_id IS NOT NULL AND posted_at IS NOT NULL")
  if (!nrow(d)) return(tibble::tibble())
  d$win <- as.Date(cut(as.Date(d$posted_at), by))
  wins <- sort(unique(d$win))
  bind_rows(lapply(seq_along(wins)[-1], function(i) {
    now  <- unique(d$a[d$win == wins[i]])
    prev <- unique(d$a[d$win == wins[i - 1]])
    if (!length(now) || !length(prev)) return(NULL)
    tibble::tibble(window = wins[i], actors = length(now),
                   retained = length(intersect(now, prev)),
                   retention = length(intersect(now, prev)) / length(prev),
                   new = length(setdiff(now, prev)))
  }))
}

# ---- ERGM, with a convergence gate -----------------------------------------
#
# A daily ERGM is not sensible: fitting is minutes-to-hours on a graph of this
# size and MCMC degeneracy is common. This runs WEEKLY, and the gate is the
# point — a model that fails to converge is reported as unavailable rather than
# printed as though its coefficients meant something.
fit_ergm <- function(g, node_attr = NULL, max_nodes = 800L, seed = 42L,
                     triad_max_nodes = 300L) {
  if (!requireNamespace("ergm", quietly = TRUE))
    return(list(ok = FALSE, reason = "ergm not installed"))
  if (gorder(g) < 30)
    return(list(ok = FALSE, reason = sprintf("graph too small (%d nodes)", gorder(g))))
  if (gorder(g) > max_nodes)
    return(list(ok = FALSE, reason = sprintf("graph too large for a weekly fit (%d > %d nodes)",
                                             gorder(g), max_nodes)))
  suppressMessages({library(ergm); library(network); library(intergraph)})
  set.seed(seed)
  nw <- intergraph::asNetwork(g)
  if (!is.null(node_attr)) for (nm in names(node_attr))
    network::set.vertex.attribute(nw, nm, unname(node_attr[[nm]][match(igraph::V(g)$name, names(node_attr[[nm]]))]))

  # A LADDER of specifications, most informative first. A sparse quote-reply
  # graph frequently cannot support triadic terms — GWESP is the usual culprit
  # for non-mixing MCMC — but that is no reason to report nothing: the simpler
  # dyadic models are still estimable and still say something. Reporting "the
  # ERGM failed" when `edges + mutual` would have converged is a false negative.
  specs <- c(
    full   = "edges + mutual + gwesp(0.25, fixed = TRUE) + gwidegree(0.5, fixed = TRUE)",
    triad  = "edges + mutual + gwesp(0.25, fixed = TRUE)",
    dyadic = "edges + mutual",
    simple = "edges")
  if (!is.null(node_attr) && "misinfo_scope" %in% names(node_attr))
    specs["full"] <- paste(specs["full"], "+ nodematch('misinfo_scope')")

  # Skip the triadic rungs on a large graph.
  #
  # Ladder order is "most informative first", which is right when every rung is
  # cheap. It is wrong here: on this quote-reply structure `full` and `triad`
  # have NEVER converged — the 154-node fit reached `dyadic` only after both
  # failed — and GWESP on a several-hundred-node graph costs up to 60 MCMLE
  # iterations of minutes each before failing. On the 887-node graph that is
  # hours of compute to arrive at the same `dyadic` answer.
  #
  # This is an empirical shortcut, not a theoretical one: if a future corpus
  # ever produces a converging triadic fit on a small graph, the full ladder
  # still runs there and the threshold can be revisited.
  if (gorder(g) > triad_max_nodes) {
    specs <- specs[c("dyadic", "simple")]
    message(sprintf("   (%d nodes > %d: skipping triadic specs, which have not converged on this graph structure)",
                    gorder(g), triad_max_nodes))
  }

  fit <- NULL; used <- NA_character_; tried <- character()
  for (nm in names(specs)) {
    form <- stats::as.formula(paste("nw ~", specs[[nm]]))
    f <- tryCatch(ergm::ergm(form, control = ergm::control.ergm(
      MCMC.burnin = 20000, MCMC.samplesize = 8000, seed = seed)),
      error = function(e) e)
    tried <- c(tried, nm)
    if (inherits(f, "error")) next
    cf <- stats::coef(f)
    if (any(!is.finite(cf)) || any(abs(cf) > 20)) next   # degenerate; try simpler
    fit <- f; used <- nm; break
  }
  if (is.null(fit))
    return(list(ok = FALSE, tried = tried,
                reason = paste("no specification converged; tried:",
                               paste(tried, collapse = ", "))))

  # Degeneracy / convergence gate. A degenerate fit produces coefficients that
  # look publishable and mean nothing, so refuse rather than report.
  # NB: mcmc.diagnostics() has no `out` argument — passing one leaks "not a
  # graphical parameter" warnings and plots to the device. The coefficient
  # sanity check below is what actually gates degeneracy.
  degen <- tryCatch({
    cf <- stats::coef(fit)
    any(!is.finite(cf)) || any(abs(cf) > 20)
  }, error = function(e) TRUE)
  if (isTRUE(degen))
    return(list(ok = FALSE, reason = "model degenerate or non-converged — suppressed"))

  s <- summary(fit)
  list(ok = TRUE, spec = used, formula = specs[[used]], tried = tried,
       n = gorder(g), m = gsize(g),
       coefs = as.data.frame(s$coefficients), aic = stats::AIC(fit))
}

# Fit on the longest recent window that is small enough to estimate.
#
# The all-time reply graph passed 800 nodes once the Castanet backfill landed
# (887 nodes, 25,170 edges) and fit_ergm() suppressed itself — correct, because
# an ERGM on a graph that size is not a weekly job, but it silently removed a
# requested analysis from the dashboard. "Too big, so nothing" is the wrong
# answer when "the last 90 days, which is what an election-season reader cares
# about anyway" is available.
#
# Windows are tried longest-first so the fit is always the most data that will
# estimate, and the window used is recorded in `spec` so a reader is never left
# guessing which period the coefficients describe.
fit_ergm_windowed <- function(con, windows = c(Inf, 365, 180, 90, 60, 30),
                              max_nodes = 800L, ...) {
  last_reason <- "no window produced an estimable graph"
  for (w in windows) {
    since <- if (is.finite(w)) Sys.Date() - w else NULL
    g <- reply_graph(con, since = since)
    lbl <- if (is.finite(w)) sprintf("last %dd", w) else "all time"
    if (gorder(g) > max_nodes) {
      last_reason <- sprintf("graph too large even at the shortest window (%d nodes)", gorder(g))
      next
    }
    if (gorder(g) < 30) {
      last_reason <- sprintf("graph too small at %s (%d nodes)", lbl, gorder(g))
      next
    }
    fit <- fit_ergm(g, max_nodes = max_nodes, ...)
    if (isTRUE(fit$ok)) {
      fit$window <- lbl
      fit$spec <- paste0(fit$spec, " (", lbl, ")")
      message(sprintf("   ERGM fitted on %s: %d nodes, %d ties", lbl, gorder(g), gsize(g)))
      return(fit)
    }
    last_reason <- fit$reason %||% last_reason
    message(sprintf("   %s: %s", lbl, last_reason))
  }
  list(ok = FALSE, reason = last_reason)
}

`%||%` <- function(a, b) if (is.null(a)) b else a
