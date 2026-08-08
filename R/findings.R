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
                              src_daily = NULL, edges = NULL, forums = NULL,
                              max_n = 10L) {
  out <- list()
  if (!nrow(post_issues)) return(tibble::tibble())

  lab <- function(code) issue_label(code)
  iss <- filter(post_issues, issue_code != "none")

  # SOMEONE SPOKE vs AN INSTITUTION FILED.
  #
  # Every statement below about "residents" must be computed on posts that a
  # PERSON authored, never on the pooled corpus. Council agenda items are
  # in-scope by construction and written in procedural prose; once the eScribe
  # backfill made them 75% of all rows, pooling silently turned two findings
  # into artifacts — "94% of comments are locally actionable" (true of agendas
  # by definition) and an emotion gap that REVERSED sign because the
  # locally-actionable side had become mostly council minutes.
  #
  # actor_key is the right discriminator, not source_id: a resident who
  # testifies at a public hearing is a voice even though the row arrives via
  # eScribe, and source_id would file them under "council".
  voiced_ids <- if ("actor_key" %in% names(posts_meta))
    posts_meta$post_id[!is.na(posts_meta$actor_key)] else posts_meta$post_id
  agenda_ids <- if (all(c("actor_key", "source_id") %in% names(posts_meta)))
    posts_meta$post_id[is.na(posts_meta$actor_key) &
                       posts_meta$source_id == "kelowna_escribe"] else character()
  voiced <- filter(iss, post_id %in% voiced_ids)

  # 1. How much of RESIDENT discourse a council can actually act on.
  #
  # THIS PERCENTAGE IS A PROPERTY OF THE FORUM SET, NOT OF THE COMMUNITY.
  # It moves purely by deciding what to crawl: adding the B.C. and Kamloops
  # forums — provincial politics and another city — pushes it down without a
  # single resident changing what they talk about. Pooling all nine forums into
  # one headline number silently presents a collection decision as a finding,
  # which is the same error as the raw-count attention gap. So the headline is
  # the Okanagan-region forums, and the out-of-region ones are reported beside
  # it as the contrast they actually are.
  region_of <- if ("forum_id" %in% names(posts_meta) && !is.null(forums) && nrow(forums))
    setNames(forums$region, forums$forum_id) else character()
  pm_region <- if (length(region_of))
    setNames(unname(region_of[as.character(posts_meta$forum_id)]), posts_meta$post_id) else NULL
  OK_REGIONS <- c("central_ok", "south_ok", "north_ok")

  by_post <- voiced |> group_by(post_id) |>
    summarise(loc = any(scope %in% LOCAL_SCOPES_F), .groups = "drop")
  if (!is.null(pm_region)) {
    by_post$region <- unname(pm_region[as.character(by_post$post_id)])
    # eScribe testimony has no forum_id; it is Okanagan by definition.
    by_post$region[is.na(by_post$region)] <- "central_ok"
    by_post$band <- ifelse(by_post$region %in% c(OK_REGIONS, "general"),
                           "okanagan", "out_of_region")
  } else by_post$band <- "okanagan"

  ok_rows <- filter(by_post, band == "okanagan")
  oo_rows <- filter(by_post, band == "out_of_region")
  if (nrow(ok_rows)) {
    pct <- 100 * mean(ok_rows$loc)
    out[[length(out)+1]] <- .f(
      sprintf("%.0f%% of Okanagan resident comments raising an issue raise one a council can act on", pct),
      sprintf("%d of %d issue-bearing comments by an identifiable person in the Okanagan forums carry at least one local, ballot or shared code.%s Council agenda items are excluded — they are in-scope by definition and would inflate this to near 100%%.",
              sum(ok_rows$loc), nrow(ok_rows),
              if (nrow(oo_rows)) sprintf(" For contrast the out-of-region forums (B.C., Kamloops) run at %.0f%% over %d comments.",
                                         100 * mean(oo_rows$loc), nrow(oo_rows)) else ""),
      "Use this as the denominator when judging whether a spike is worth a council's attention. The remainder is real public feeling, but no local lever exists for it.",
      "strong",
      "A direct count over resident-authored posts. Quoted for the Okanagan forums only: this percentage is a property of which forums are collected, not of the community, so pooling in out-of-region forums would move it without anyone changing what they say.")
  }

  # 2. The attention gap — the project's signature result.
  #
  # MUST be computed on SHARES within a COMMON WINDOW, never on raw counts.
  # The two corpora are wildly different sizes (2,644 agenda items against 683
  # forum comments) and cover different spans (eScribe begins Jan 2025;
  # Castanet reaches back to 2022). On raw counts council "out-talks" residents
  # on nearly every issue purely because there are four times as many agenda
  # rows — which would read as a finding about attention when it is only a
  # finding about corpus size. Share-of-own-corpus is scale-free, and clipping
  # both sides to the overlap removes the span artifact.
  if (nrow(posts_meta) && all(c("source_id", "posted_at") %in% names(posts_meta))) {
    pm <- posts_meta |> mutate(day = as.Date(posted_at),
                               side = case_when(post_id %in% agenda_ids ~ "council",
                                                post_id %in% voiced_ids ~ "resident",
                                                TRUE ~ NA_character_))
    span <- pm |> filter(!is.na(side)) |>
      group_by(side) |> summarise(lo = min(day), hi = max(day), .groups = "drop")
    if (nrow(span) == 2) {
      lo <- max(span$lo); hi <- min(span$hi)          # the overlap, not the union
      j <- iss |>
        inner_join(select(pm, post_id, side, day), by = "post_id") |>
        filter(!is.na(side), scope %in% LOCAL_SCOPES_F, day >= lo, day <= hi) |>
        count(issue_code, side, name = "n") |>
        tidyr::pivot_wider(names_from = side, values_from = n, values_fill = 0)
      if (all(c("resident", "council") %in% names(j)) && nrow(j) >= 3) {
        j <- j |>
          mutate(resid = resident, council = council,
                 r_sh = 100 * resid / sum(resid), c_sh = 100 * council / sum(council),
                 gap = r_sh - c_sh) |>
          arrange(desc(gap))
        top <- head(j, 3); inv <- j |> arrange(gap) |> head(1)
        out[[length(out)+1]] <- .f(
          sprintf("Residents and council are focused on different things: %s takes %.0f%% of resident attention but %.0f%% of the agenda",
                  lab(top$issue_code[1]), top$r_sh[1], top$c_sh[1]),
          sprintf("Share of locally-actionable mentions within each corpus, %s to %s. %s: %.1f%% of resident comments (n=%d) vs %.1f%% of agenda items (n=%d). Also %s (%.1f%% vs %.1f%%) and %s (%.1f%% vs %.1f%%). Conversely %s is %.1f%% of the agenda against %.1f%% of comments.",
                  format(lo, "%d %b %Y"), format(hi, "%d %b %Y"),
                  lab(top$issue_code[1]), top$r_sh[1], top$resid[1], top$c_sh[1], top$council[1],
                  lab(top$issue_code[2]), top$r_sh[2], top$c_sh[2],
                  lab(top$issue_code[3]), top$r_sh[3], top$c_sh[3],
                  lab(inv$issue_code[1]), inv$c_sh[1], inv$r_sh[1]),
          "For a council: these are the issues you are being judged on but not visibly working on. For a candidate: the gap is the campaign opening.",
          "provisional",
          "Shares, not counts, and clipped to the window both sources cover — the corpora differ ~4x in size, so raw counts would measure collection effort rather than attention. Matters reaching council via staff reports rather than numbered agenda items are still not captured.")
      }
    }
  }

  # 3. Emotion by scope — survives length normalisation.
  #
  # RESIDENT POSTS ONLY. Agenda items carry no emotion in any meaningful sense
  # and score high on trust vocabulary ("approve", "support", "committee"); on
  # the pooled corpus they made the locally-actionable band look calm and
  # trusting purely because it was mostly minutes, flipping the sign of this
  # comparison against the earlier, correct result.
  if (nrow(sentiment)) {
    e <- voiced |> inner_join(sentiment, by = "post_id") |>
      mutate(band = ifelse(scope %in% LOCAL_SCOPES_F, "local", "other"),
             across(c(anger, fear, trust), ~ 100 * .x / pmax(n_words, 1))) |>
      group_by(band) |>
      summarise(across(c(anger, fear, trust), ~ mean(.x, na.rm = TRUE)),
                w = mean(n_words, na.rm = TRUE), .groups = "drop")
    if (nrow(e) == 2) {
      lo <- filter(e, band == "local"); ot <- filter(e, band == "other")
      # Both the trust claim and the length claim have to be earned from the
      # numbers on this run, not asserted. On the pilot corpus trust differed
      # sharply between bands; after the corpus was corrected to resident-only
      # posts it came out dead level (2.80 vs 2.80), at which point the stock
      # sentence "the trust gap suggests it is worth doing" was simply false.
      # A finding generator that hardcodes its own conclusion will keep
      # printing it long after the data stops supporting it.
      trust_d <- ot$trust - lo$trust
      len_d   <- 100 * (lo$w - ot$w) / max(lo$w, 1)
      out[[length(out)+1]] <- .f(
        "The angriest and most fearful discourse is about things a council cannot fix",
        sprintf("Per 100 words: anger %.2f vs %.2f, fear %.2f vs %.2f, trust %.2f vs %.2f (out-of-scope vs locally actionable). %s",
                ot$anger, lo$anger, ot$fear, lo$fear, ot$trust, lo$trust,
                if (len_d >= 10)
                  sprintf("Out-of-scope comments are also SHORTER (%.0f vs %.0f words), so this is not a length artifact.", ot$w, lo$w)
                else
                  sprintf("Comment length is near-identical across the two bands (%.0f vs %.0f words), so length cannot explain the difference either way.", ot$w, lo$w)),
        paste0("Expect hostility at the podium about provincial and federal matters. Naming the jurisdiction explicitly, early, is the only available response",
               if (abs(trust_d) >= 0.25)
                 sprintf(" — and the trust gap (%.2f vs %.2f) suggests it is worth doing.", ot$trust, lo$trust)
               else ". Trust vocabulary is level across the two bands, so there is no trust gap to appeal to here."),
        "strong",
        "Resident-authored posts only, normalised per 100 words. Pooling council agendas into the locally-actionable band reversed the sign of this comparison, and raw NRC counts reversed it again.")
    }
  }

  # 4/5. Volume leader and momentum, with the concentration guard.
  # Resident voice again — otherwise this just reports the busiest agenda
  # category, which is a fact about council's workload, not about the public.
  vol <- voiced |> filter(scope %in% LOCAL_SCOPES_F) |> count(issue_code, sort = TRUE)
  if (nrow(vol) >= 3) out[[length(out)+1]] <- .f(
    sprintf("%s is the largest locally-actionable issue for residents", lab(vol$issue_code[1])),
    sprintf("%d mentions by identifiable people; next are %s (%d) and %s (%d).",
            vol$n[1], lab(vol$issue_code[2]), vol$n[2], lab(vol$issue_code[3]), vol$n[3]),
    "Treat as the standing agenda item for public communication.",
    "strong", "Simple volume count over model-coded, resident-authored posts.")

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

  # 8. Jurisdiction focus. Resident voice only: Kelowna's own agenda naming
  # Kelowna is not evidence about where public attention sits.
  jur <- voiced |> filter(scope %in% LOCAL_SCOPES_F, !is.na(jurisdiction),
                          jurisdiction != "unspecified") |>
    count(jurisdiction, sort = TRUE)
  if (nrow(jur)) out[[length(out)+1]] <- .f(
    sprintf("Actionable resident discourse is concentrated in %s", jur$jurisdiction[1]),
    sprintf("%d mentions vs %d for the next community. %d of %d actionable comments name no community at all.",
            jur$n[1], if (nrow(jur) > 1) jur$n[2] else 0L,
            sum(voiced$scope %in% LOCAL_SCOPES_F) - sum(jur$n),
            sum(voiced$scope %in% LOCAL_SCOPES_F)),
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
  # Report the two corpora separately. A single pooled "3,327 items" reads as
  # far more resident evidence than actually exists: three quarters of it is
  # council agendas.
  n_posts <- sprintf("%s resident posts and %s council agenda items",
                     format(length(voiced_ids), big.mark = ","),
                     format(length(agenda_ids), big.mark = ","))
  span <- if (nrow(posts_meta)) range(as.Date(posts_meta$posted_at), na.rm = TRUE) else NULL
  out[[length(out)+1]] <- .f(
    "This is a pilot: read every number above as provisional",
    sprintf("%s over %s. Classifier agreement against a human standard has not yet been measured. Forum commenters are not a representative sample of residents.",
            n_posts,
            if (!is.null(span)) paste(format(span, "%d %b %Y"), collapse = " to ") else "an uneven window"),
    "Do not cite any single figure here as established. The measures worth acting on are the large, direction-consistent ones — the scope split and the emotion gap — not small differences between issues.",
    "weak", "Stated deliberately: a dashboard that never reports its own limits invites over-reading.")

  bind_rows(out) |> head(max_n)
}
