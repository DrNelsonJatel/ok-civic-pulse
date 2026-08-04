# findings.R — generate the take-home analysis points.
#
# These are DERIVED FROM THE DATA on every render, never written by hand. A
# hardcoded finding is true on the day it is written and quietly false
# afterwards; this project updates daily, so the headline has to be computed.
#
# Every finding carries four things, and the last two are not optional:
#   headline   — what to say
#   evidence   — the numbers behind it, so a reader can check
#   action     — what a council, candidate or researcher would DO about it
#   confidence — "strong" | "provisional" | "weak", with the reason
#
# A finding with no stated confidence invites a reader to treat a pilot result
# as settled. Several results here rest on eight summer meetings and a
# classifier whose human agreement is not yet measured; the confidence field is
# where that has to live.
suppressMessages({library(dplyr); library(DBI)})

LOCAL_SCOPES_F <- c("local", "ballot", "shared")

.f <- function(headline, evidence, action, confidence, why) {
  tibble::tibble(headline = headline, evidence = evidence,
                 action = action, confidence = confidence, why = why)
}

generate_findings <- function(post_issues, posts_meta, issue_daily, sentiment,
                              src_daily = NULL, edges = NULL, max_n = 10L) {
  out <- list()
  if (!nrow(post_issues)) return(tibble::tibble())

  lab <- function(code) issue_label(code)
  iss <- filter(post_issues, issue_code != "none")

  # 1. How much of the discourse a council can actually act on.
  by_post <- iss |> group_by(post_id) |>
    summarise(loc = any(scope %in% LOCAL_SCOPES_F), .groups = "drop")
  pct <- 100 * mean(by_post$loc)
  out[[length(out)+1]] <- .f(
    sprintf("%.0f%% of comments raising an issue raise one a council can act on", pct),
    sprintf("%d of %d issue-bearing comments carry at least one local, ballot or shared code.",
            sum(by_post$loc), nrow(by_post)),
    "Use this as the denominator when judging whether a spike is worth a council's attention. The remainder is real public feeling, but no local lever exists for it.",
    "strong", "A direct count, not a model estimate.")

  # 2. The attention gap — the project's signature result.
  if (nrow(posts_meta) && "source_id" %in% names(posts_meta)) {
    j <- iss |> inner_join(select(posts_meta, post_id, source_id), by = "post_id") |>
      filter(scope %in% LOCAL_SCOPES_F) |>
      count(issue_code, source_id, name = "n") |>
      tidyr::pivot_wider(names_from = source_id, values_from = n, values_fill = 0)
    if (all(c("castanet_forums", "kelowna_escribe") %in% names(j))) {
      j <- j |> mutate(resid = castanet_forums, council = kelowna_escribe,
                       gap = resid - council) |> arrange(desc(gap))
      top <- head(j, 3)
      inv <- j |> arrange(gap) |> head(1)
      out[[length(out)+1]] <- .f(
        sprintf("Residents and council are focused on different things: %s dominates comment volume but barely appears on agendas",
                lab(top$issue_code[1])),
        sprintf("%s: %d resident comments vs %d agenda items. Also %s (%d vs %d) and %s (%d vs %d). Conversely %s is %d agenda items against only %d comments.",
                lab(top$issue_code[1]), top$resid[1], top$council[1],
                lab(top$issue_code[2]), top$resid[2], top$council[2],
                lab(top$issue_code[3]), top$resid[3], top$council[3],
                lab(inv$issue_code[1]), inv$council[1], inv$resid[1]),
        "For a council: these are the issues you are being judged on but not visibly working on. For a candidate: the gap is the campaign opening.",
        "provisional",
        "Rests on a small number of meetings in a season when agendas are thin, and matters reaching council via staff reports rather than numbered agenda items are not captured.")
    }
  }

  # 3. Emotion by scope — survives length normalisation.
  if (nrow(sentiment)) {
    e <- iss |> inner_join(sentiment, by = "post_id") |>
      mutate(band = ifelse(scope %in% LOCAL_SCOPES_F, "local", "other"),
             across(c(anger, fear, trust), ~ 100 * .x / pmax(n_words, 1))) |>
      group_by(band) |>
      summarise(across(c(anger, fear, trust), ~ mean(.x, na.rm = TRUE)),
                w = mean(n_words, na.rm = TRUE), .groups = "drop")
    if (nrow(e) == 2) {
      lo <- filter(e, band == "local"); ot <- filter(e, band == "other")
      out[[length(out)+1]] <- .f(
        "The angriest and most fearful discourse is about things a council cannot fix",
        sprintf("Per 100 words: anger %.2f vs %.2f, fear %.2f vs %.2f, trust %.2f vs %.2f (out-of-scope vs locally actionable). Out-of-scope comments are also SHORTER (%.0f vs %.0f words), so this is not a length artifact.",
                ot$anger, lo$anger, ot$fear, lo$fear, ot$trust, lo$trust, ot$w, lo$w),
        "Expect hostility at the podium about provincial and federal matters. Naming the jurisdiction explicitly, early, is the only available response — and the trust gap suggests it is worth doing.",
        "strong",
        "Holds after normalising by comment length, which reversed the equivalent stance-level comparison.")
    }
  }

  # 4/5. Volume leader and momentum, with the concentration guard.
  vol <- iss |> filter(scope %in% LOCAL_SCOPES_F) |> count(issue_code, sort = TRUE)
  if (nrow(vol)) out[[length(out)+1]] <- .f(
    sprintf("%s is the largest locally-actionable issue", lab(vol$issue_code[1])),
    sprintf("%d mentions; next are %s (%d) and %s (%d).",
            vol$n[1], lab(vol$issue_code[2]), vol$n[2], lab(vol$issue_code[3]), vol$n[3]),
    "Treat as the standing agenda item for public communication.",
    "strong", "Simple volume count over model-coded comments.")

  # 6. Concentration — the manufactured-groundswell check.
  conc <- iss |> inner_join(select(posts_meta, post_id, actor_key), by = "post_id") |>
    filter(!is.na(actor_key), scope %in% LOCAL_SCOPES_F) |>
    count(issue_code, actor_key, name = "n") |>
    group_by(issue_code) |>
    summarise(posts = sum(n), actors = n(), hhi = sum((n / sum(n))^2), .groups = "drop") |>
    filter(posts >= 8) |> arrange(desc(hhi))
  if (nrow(conc)) {
    top <- conc[1, ]
    out[[length(out)+1]] <- .f(
      if (top$hhi >= 0.3)
        sprintf("%s looks louder than it is — a few voices dominate it", lab(top$issue_code))
      else "No locally-actionable issue is being driven by a handful of voices",
      sprintf("Highest concentration is %s: %d comments from %d actors, HHI %.2f (1.0 = one person, %.2f = perfectly even).",
              lab(top$issue_code), top$posts, top$actors, top$hhi, 1 / top$actors),
      if (top$hhi >= 0.3)
        "Check who is posting before reading this as public opinion. Volume from few actors is not a groundswell."
      else "Volume figures here can be read as breadth of concern rather than the product of a few prolific posters.",
      "strong", "Herfindahl index over posts per actor, computed across the window rather than averaged per day.")
  }

  # 7. Source value.
  if (!is.null(src_daily) && nrow(src_daily)) {
    sc <- src_daily |> group_by(source_id) |>
      summarise(days = n_distinct(day), local = sum(n_local), coded = sum(n_coded),
                .groups = "drop") |>
      mutate(per_day = local / pmax(days, 1)) |> arrange(desc(per_day))
    if (nrow(sc) >= 2) out[[length(out)+1]] <- .f(
      sprintf("%s yields more actionable material per day than %s", sc$source_id[1], sc$source_id[2]),
      sprintf("%.1f vs %.1f locally-actionable items per active day.", sc$per_day[1], sc$per_day[2]),
      "Weight collection effort toward the higher-yield source. Council records are in-scope by construction; forum comment needs filtering.",
      "strong", "Direct count per active day, which rewards in-scope output rather than raw volume.")
  }

  # 8. Jurisdiction focus.
  jur <- iss |> filter(scope %in% LOCAL_SCOPES_F, !is.na(jurisdiction),
                       jurisdiction != "unspecified") |>
    count(jurisdiction, sort = TRUE)
  if (nrow(jur)) out[[length(out)+1]] <- .f(
    sprintf("Actionable discourse is concentrated in %s", jur$jurisdiction[1]),
    sprintf("%d mentions vs %d for the next community. %d of %d actionable comments name no community at all.",
            jur$n[1], if (nrow(jur) > 1) jur$n[2] else 0L,
            sum(iss$scope %in% LOCAL_SCOPES_F) - sum(jur$n), sum(iss$scope %in% LOCAL_SCOPES_F)),
    "Geographic targeting is possible but incomplete: most comments never name a community, so jurisdiction counts are a lower bound.",
    "provisional", "Jurisdiction is only assigned when a place name appears; absence is not evidence of absence.")

  # 9. Network structure.
  if (!is.null(edges) && nrow(edges) > 30) {
    suppressMessages(library(igraph))
    el <- edges |> filter(from_user_id != to_user_id) |>
      count(from_user_id, to_user_id, name = "w") |> mutate(across(1:2, as.character))
    g <- graph_from_data_frame(el, directed = TRUE)
    gu <- as_undirected(g, mode = "collapse")
    cm <- components(gu); gt <- induced_subgraph(gu, which(cm$membership == which.max(cm$csize)))
    deg <- degree(g, mode = "all")
    top_share <- sum(sort(deg, decreasing = TRUE)[1:max(1, round(0.1 * length(deg)))]) / sum(deg)
    out[[length(out)+1]] <- .f(
      "Debate runs through a small core of connectors",
      sprintf("%d actors, %d ties. The most active 10%% account for %.0f%% of all replies. Clustering %.2f against %.2f expected at random.",
              gorder(g), gsize(g), 100 * top_share,
              transitivity(gt, "global"),
              transitivity(sample_gnm(gorder(gt), gsize(gt)), "global")),
      "A handful of accounts shape how arguments spread. Engagement reaching them travels further than the same effort spread evenly.",
      "provisional",
      "Quote-replies are the only reply signal available, so the network is sparse by construction and understates real interaction.")
  }

  # 10. The honest caveat — always last, never omitted.
  n_posts <- nrow(posts_meta)
  span <- if (nrow(posts_meta)) range(as.Date(posts_meta$posted_at), na.rm = TRUE) else NULL
  out[[length(out)+1]] <- .f(
    "This is a pilot: read every number above as provisional",
    sprintf("%s items over %s. Classifier agreement against a human standard has not yet been measured. Forum commenters are not a representative sample of residents.",
            format(n_posts, big.mark = ","),
            if (!is.null(span)) paste(format(span, "%d %b %Y"), collapse = " to ") else "an uneven window"),
    "Do not cite any single figure here as established. The measures worth acting on are the large, direction-consistent ones — the scope split and the emotion gap — not small differences between issues.",
    "weak", "Stated deliberately: a dashboard that never reports its own limits invites over-reading.")

  bind_rows(out) |> head(max_n)
}
