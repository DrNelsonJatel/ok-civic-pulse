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
  # Backbone: two actors must share at least `min_shared` threads. Without this
  # every pair in a single busy thread becomes an edge and the projection is
  # near-complete — visually and statistically useless.
  subgraph.edges(proj, which(E(proj)$weight >= min_shared), delete.vertices = TRUE)
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

# ---- ERGM, with a convergence gate -----------------------------------------
#
# A daily ERGM is not sensible: fitting is minutes-to-hours on a graph of this
# size and MCMC degeneracy is common. This runs WEEKLY, and the gate is the
# point — a model that fails to converge is reported as unavailable rather than
# printed as though its coefficients meant something.
fit_ergm <- function(g, node_attr = NULL, max_nodes = 800L, seed = 42L) {
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

  # GWESP captures triadic closure without the near-certain degeneracy of a
  # bare triangle term.
  terms <- "edges + mutual + gwesp(0.25, fixed = TRUE) + gwidegree(0.5, fixed = TRUE)"
  if (!is.null(node_attr) && "misinfo_scope" %in% names(node_attr))
    terms <- paste(terms, "+ nodematch('misinfo_scope')")
  form <- stats::as.formula(paste("nw ~", terms))

  fit <- tryCatch(ergm::ergm(form, control = ergm::control.ergm(
    MCMC.burnin = 20000, MCMC.samplesize = 8000, seed = seed)),
    error = function(e) e)
  if (inherits(fit, "error"))
    return(list(ok = FALSE, reason = paste("fit error:", conditionMessage(fit))))

  # Degeneracy / convergence gate. A degenerate fit produces coefficients that
  # look publishable and mean nothing, so refuse rather than report.
  degen <- tryCatch({
    mcmc <- ergm::mcmc.diagnostics(fit, out = FALSE)
    any(!is.finite(stats::coef(fit))) || any(abs(stats::coef(fit)) > 20)
  }, error = function(e) TRUE)
  if (isTRUE(degen))
    return(list(ok = FALSE, reason = "model degenerate or non-converged — suppressed"))

  s <- summary(fit)
  list(ok = TRUE, formula = terms, n = gorder(g), m = gsize(g),
       coefs = as.data.frame(s$coefficients), aic = stats::AIC(fit))
}
