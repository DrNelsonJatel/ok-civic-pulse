# classify.R — two-tier Claude classification.
#
# There is no official Anthropic SDK for R, so this is raw HTTP via httr2.
#
# TIERING (free-pilot design):
#   bulk  — claude-haiku-4-5 through the Message Batches API. Batches are 50%
#           off and this workload is entirely latency-insensitive, so bulk
#           coding runs at roughly $0.45 per 1,000 comments.
#   audit — claude-opus-5 over a stratified sample, to MEASURE the bulk tier's
#           agreement rather than assuming it. Also re-codes the low-confidence
#           tail. This is what makes the cheap tier defensible instead of
#           merely cheap.
#
# MODEL-SPECIFIC REQUEST SHAPES — these differ and getting them wrong 400s:
#   claude-opus-5    thinking is ON by default (adaptive); max_tokens caps
#                    thinking PLUS response. output_config.effort supported.
#                    Never send temperature/top_p/top_k — removed, returns 400.
#                    Do NOT disable thinking to save money: on Opus 5 that can
#                    leak <thinking> tags into the visible response. Lower
#                    effort instead.
#   claude-haiku-4-5 does NOT support output_config.effort — sending it errors.
#                    No adaptive thinking either. Structured outputs ARE
#                    supported, which is the part that matters here.
#
# Prompt caching carries the taxonomy prefix in both tiers; it is the single
# biggest cost lever on a corpus this size (cache reads bill at ~0.1x).
suppressMessages({library(httr2); library(jsonlite); library(dplyr); library(DBI)})

# Promoted from claude-haiku-4-5 on 2026-08-04 after the first audit.
# Haiku scored kappa 0.39 against Opus 5 on the is-local judgement and
# systematically UNDER-called "local" (14 misses vs 1 false positive on n=60).
# That is the single judgement the product rests on, and $15/month was not
# worth a fair-agreement classifier. Opus 5 codes ~100 posts for $0.40 with
# caching active (its 512-token cache minimum works where Haiku's 4096 did not).
MODEL_BULK  <- Sys.getenv("OKCP_MODEL_BULK",  "claude-opus-5")
MODEL_AUDIT <- Sys.getenv("OKCP_MODEL_AUDIT", "claude-opus-5")
AUDIT_EFFORT <- Sys.getenv("OKCP_AUDIT_EFFORT", "medium")
CLASSIFY_BATCH <- as.integer(Sys.getenv("OKCP_CLASSIFY_BATCH", "10"))

coder_id_for <- function(model) paste0("claude:", model)

# Per-MTok list prices, for the cost estimator only.
PRICES <- list(
  "claude-opus-5"    = c(input = 5.00, output = 25.00),
  "claude-sonnet-5"  = c(input = 3.00, output = 15.00),
  "claude-haiku-4-5" = c(input = 1.00, output = 5.00)
)

anthropic_key <- function() {
  k <- Sys.getenv("ANTHROPIC_API_KEY")
  if (!nzchar(k)) stop("ANTHROPIC_API_KEY not set (add it to ~/.Renviron).", call. = FALSE)
  # drought-sna lesson: a double-prefixed sk-ant-sk-ant-... key 401s with a
  # completely unhelpful message. Catch it here instead.
  if (grepl("^sk-ant-sk-ant-", k)) stop("ANTHROPIC_API_KEY is double-prefixed 'sk-ant-sk-ant-'.", call. = FALSE)
  k
}

# httr2's default error message is just "HTTP 400 Bad Request", which hides the
# API's actual explanation. That cost real time once: a CREDIT EXHAUSTION
# ("Your credit balance is too low") is returned as a 400 and looked identical
# to a malformed request, so a run kept firing 44 more doomed batches instead
# of stopping. Surface the message, and make it recognisable.
anthropic_req <- function(path) {
  request(paste0("https://api.anthropic.com", path)) |>
    req_headers(`x-api-key` = anthropic_key(),
                `anthropic-version` = "2023-06-01",
                `content-type` = "application/json") |>
    req_error(body = function(resp) {
      msg <- tryCatch(resp_body_json(resp)$error$message, error = function(e) NULL)
      if (is.null(msg)) NULL else msg
    }) |>
    req_retry(max_tries = 4, backoff = \(i) 2^i) |>
    req_timeout(300)
}

# Conditions that make every subsequent call pointless. Retrying through these
# just burns wall-clock and rate limit.
.is_fatal_api_error <- function(msg) {
  grepl("credit balance|billing|insufficient.*quota|has been disabled",
        msg, ignore.case = TRUE)
}

# ---- prompt + schema --------------------------------------------------------
taxonomy_prompt <- function() {
  lines <- vapply(seq_len(nrow(ISSUES)), function(i) {
    sprintf("- %s [%s] — %s", ISSUES$code[i], ISSUES$scope[i], ISSUES$definition[i])
  }, character(1))
  paste0(
"You are coding public comments from Castanet (an Okanagan, British Columbia news
site) for a research project on civic discourse ahead of the BC general local
election on 2026-10-17.

For each comment, identify which governance issues it raises, and — this is the
central judgement — whether each issue is something a LOCAL government can
actually act on, or a provincial/federal matter being directed at city hall.

SCOPE VALUES
  local      — a municipal council or regional board has direct authority
  ballot     — not municipal, but the office IS elected on 2026-10-17
               (school trustees; regional district electoral area directors)
  shared     — a real local role, but the province holds the main levers
  provincial — provincial responsibility, often misdirected at council
  federal    — federal responsibility
  none       — no governance content (chit-chat, jokes, pure abuse)

Use the scope shown against each issue code below unless the comment clearly
frames the issue at a different level; if it does, use the level the comment is
actually about and reflect any doubt in your confidence score.

ISSUE CODES
", paste(lines, collapse = "\n"), "

JURISDICTIONS (use the code, or null if no specific community is named)
", paste(JURISDICTIONS$code, collapse = ", "), "

CODING RULES
1. Only code issues the comment ACTUALLY raises. A passing mention of a place
   name is not an issue. Most comments raise zero, one, or two issues.
2. If a comment raises no governance issue at all, return a single entry with
   code 'none' and scope 'none'.
3. salience: 0-1, how central the issue is to the comment. A comment entirely
   about potholes scores ~1.0 on roads_traffic; a one-clause aside scores ~0.2.
4. confidence: 0-1, how sure you are of the code AND scope assignment.
5. stance: the commenter's position toward the government action or proposal
   under discussion — support, oppose, mixed, or neutral. Judge the stance
   toward the POLICY, not the commenter's mood.
6. Do not infer beyond the text. Sarcasm is common; read for intent, and where
   genuinely ambiguous, lower the confidence rather than guessing.
7. These are real pseudonymous people. Code the comment, never the person.")
}

# JSON schema for output_config.format. Every object needs
# additionalProperties:false plus an explicit `required` list; no
# minLength/maximum/recursive schemas.
classify_schema <- function() {
  list(
    type = "object", additionalProperties = FALSE,
    required = list("results"),
    properties = list(results = list(
      type = "array",
      items = list(
        type = "object", additionalProperties = FALSE,
        required = list("post_id", "issues"),
        properties = list(
          post_id = list(type = "string"),
          issues = list(
            type = "array",
            items = list(
              type = "object", additionalProperties = FALSE,
              required = list("code","scope","jurisdiction","stance","salience","confidence"),
              properties = list(
                code         = list(type = "string", enum = as.list(c(ISSUES$code, "none"))),
                scope        = list(type = "string",
                                    enum = list("local","ballot","shared","provincial","federal","none")),
                jurisdiction = list(type = c("string","null")),
                stance       = list(type = "string",
                                    enum = list("support","oppose","mixed","neutral")),
                salience     = list(type = "number"),
                confidence   = list(type = "number")))))))))
}

# Build the request body for one batch of comments, shaped for the model.
build_body <- function(posts, model, effort = NULL) {
  payload <- lapply(seq_len(nrow(posts)), function(i) list(
    post_id = sprintf("%.0f", posts$post_id[i]),
    thread  = substr(coalesce(posts$title[i], ""), 1, 200),
    comment = substr(posts$body_local[i], 1, 4000)))

  oc <- list(format = list(type = "json_schema", schema = classify_schema()))
  # effort is unsupported on Haiku 4.5 and errors if sent. Only Opus/Sonnet
  # tiers get it.
  if (!is.null(effort) && !grepl("haiku", model)) oc$effort <- effort

  list(
    model = model,
    # On Opus 5 this budget covers thinking AND the JSON response.
    max_tokens = if (grepl("haiku", model)) 8000 else 12000,
    system = list(list(
      type = "text", text = taxonomy_prompt(),
      cache_control = list(type = "ephemeral"))),   # stable cached prefix
    output_config = oc,
    messages = list(list(role = "user", content = paste0(
      "Code each of the following comments. Return one results entry per ",
      "comment, with the post_id exactly as given.\n\n",
      toJSON(payload, auto_unbox = TRUE, pretty = TRUE))))
  )
}

parse_classify_response <- function(out) {
  # Structured output guarantees the schema, but not that the model answered:
  # a safety decline returns HTTP 200 with stop_reason "refusal" and empty or
  # partial content. Check before indexing.
  if (identical(out$stop_reason, "refusal"))
    stop("refusal: ", out$stop_details$category %||% "unknown", call. = FALSE)
  if (identical(out$stop_reason, "max_tokens"))
    warning("hit max_tokens — batch may be truncated", call. = FALSE)
  txt <- Filter(function(b) identical(b$type, "text"), out$content)
  if (!length(txt)) stop("no text block (stop_reason=", out$stop_reason, ")", call. = FALSE)
  fromJSON(txt[[1]]$text, simplifyVector = FALSE)$results
}

# `expected` is the set of post_ids actually sent in this chunk.
#
# The model echoes post_id back as a string, and occasionally returns one that
# is missing, empty, or not a number — as.numeric() then yields NA, which hits
# the NOT NULL constraint on post_issues.post_id and aborts the whole shard
# mid-write. Nine shards of a ten-shard run failed this way.
#
# Dropping the bad rows silently would be the other wrong answer: those are
# posts that were paid for and never coded, and nothing downstream would ever
# say so. So drop them, and SAY how many and why. Ids outside `expected` are
# also rejected — a hallucinated id would otherwise attach labels to an
# unrelated post, which is worse than losing them.
results_to_rows <- function(results, coder, expected = NULL) {
  out <- bind_rows(lapply(results, function(r) {
    if (!length(r$issues)) return(NULL)
    bind_rows(lapply(r$issues, function(x) tibble::tibble(
      post_id      = suppressWarnings(as.numeric(r$post_id %||% NA)),
      issue_code   = x$code %||% "none",
      scope        = x$scope %||% "none",
      jurisdiction = if (is.null(x$jurisdiction)) NA_character_ else as.character(x$jurisdiction),
      stance       = x$stance %||% NA_character_,
      salience     = as.numeric(x$salience %||% NA),
      confidence   = as.numeric(x$confidence %||% NA),
      coder_id     = coder,
      coded_at     = Sys.time())))
  }))
  if (!nrow(out)) return(out)

  bad_id <- is.na(out$post_id)
  unknown <- if (!is.null(expected)) !bad_id & !(out$post_id %in% expected) else rep(FALSE, nrow(out))
  if (any(bad_id))
    message(sprintf("  results_to_rows: dropped %d row(s) with an unusable post_id", sum(bad_id)))
  if (any(unknown))
    message(sprintf("  results_to_rows: dropped %d row(s) whose post_id was not in this chunk (%s)",
                    sum(unknown), paste(head(unique(out$post_id[unknown]), 3), collapse = ", ")))
  out[!bad_id & !unknown, , drop = FALSE]
}

# ---- the gate: which posts are worth a model call ---------------------------
#
# The model's value is adjudicating scope for comments that MIGHT be local.
# Two rules, both measured against the first real crawl (683 posts):
#
#  1. A post must carry at least one LOCAL-CANDIDATE code (scope local, ballot,
#     or shared). Posts whose only codes are federal/provincial — courts_bail,
#     foreign_world, national_partisan, highways_moti — need no judgement; the
#     sieve already knows they are not municipally actionable. This is the big
#     lever: 141 of 528 eligible posts, a 27% cut.
#  2. A weak code (WEAK_GATE_CODES) doesn't count as that local candidate
#     unless it comes with a jurisdiction tag. Measured effect is small (4
#     posts) because governance_process nearly always co-occurs with a place
#     name, but it costs nothing and removes the obvious false positives.
#
# The false-negative risk — a genuinely local comment the sieve under-coded —
# is not assumed away: audit_sample() deliberately draws from the SKIPPED
# strata so the miss rate is measured rather than hoped for.
GATE_LOCAL_SCOPES <- c("local", "ballot", "shared")

# GATE MODE — default "all" since 2026-08-04.
#
# The first audit measured what the scope gate was actually costing, by drawing
# audit samples from the SKIPPED strata. The answer killed the gate:
#
#   skipped_no_code       26.7% were genuinely locally actionable
#   skipped_out_of_scope  20.0% were genuinely locally actionable
#
# The gate saved 28% of model calls and silently discarded a fifth to a quarter
# of the target material. At Opus 5 prices a full corpus re-code is a few
# dollars, so the trade is indefensible: code everything, and keep the sieve for
# prioritisation and analysis rather than as a filter.
#
# Set OKCP_GATE=scoped to restore the old behaviour (documented, not advised).
GATE_MODE <- Sys.getenv("OKCP_GATE", "all")

gate_predicate <- function() {
  weak   <- paste(sprintf("'%s'", WEAK_GATE_CODES), collapse = ",")
  scopes <- paste(sprintf("'%s'", GATE_LOCAL_SCOPES), collapse = ",")
  sprintf("sum(CASE WHEN issue_code <> 'none' AND scope IN (%s)
                     AND (issue_code NOT IN (%s) OR jurisdiction IS NOT NULL)
                    THEN 1 ELSE 0 END) > 0", scopes, weak)
}

# `since` restricts to an analysis window. This is a cost AND a validity
# control, not just a budget lever: the council corpus begins 2025-01-13, so
# pre-window forum posts have no counterpart on the other side of the attention
# gap and cannot enter it, the scope split, or the emotion comparison. Walking
# megathreads from page 0 dragged in 15k posts going back to 2007; coding them
# would have bought nothing for the published figures. They stay in the DB and
# can be coded later if a longer baseline is ever wanted.
gate_sql <- function(target_coder, since = NULL) {
  filt <- if (identical(GATE_MODE, "scoped"))
    sprintf("AND p.post_id IN (SELECT post_id FROM post_issues
                                WHERE coder_id = '%s' GROUP BY post_id HAVING %s)",
            SIEVE_CODER, gate_predicate()) else ""
  win <- if (!is.null(since))
    sprintf("AND p.posted_at >= DATE '%s'", format(as.Date(since))) else ""
  sprintf("
    SELECT DISTINCT p.post_id, p.body_local, t.title
      FROM posts p
      LEFT JOIN threads t ON t.t_id = p.thread_t
     WHERE p.body_local IS NOT NULL AND length(p.body_local) > 20
       %s %s
       AND p.post_id NOT IN (SELECT post_id FROM post_issues WHERE coder_id = '%s')
     ORDER BY p.post_id", filt, win, target_coder)
}

gate_counts <- function(con) {
  dbGetQuery(con, sprintf("
    SELECT count(*) AS all_posts,
           count(*) FILTER (WHERE any_code)  AS sieve_any,
           count(*) FILTER (WHERE gate_open) AS gate_open
      FROM (SELECT post_id,
                   max(CASE WHEN issue_code <> 'none' THEN 1 ELSE 0 END) = 1 AS any_code,
                   %s AS gate_open
              FROM post_issues WHERE coder_id = '%s' GROUP BY post_id)",
    gate_predicate(), SIEVE_CODER))
}

# ---- the daily entry point --------------------------------------------------
#
# 02_daily.R has called classify_new() since it was written, and the function
# did not exist. The scheduled job therefore aborted at that line every run:
# after the scrape (which persists) but BEFORE rebuild_issue_daily() and
# db_log(), so posts accumulated while nothing was ever coded or rolled up, and
# the wrapper skipped the PDF on the non-zero exit. A cron that collects and
# silently never analyses is worse than no cron.
#
# DAILY_MAX is not optional. classify_sync(limit = Inf) over an unclassified
# backlog is unbounded spend from an unattended job — after a backfill that
# backlog was 36,105 posts, roughly US$160 at synchronous rates. The cap makes
# the worst case a known daily figure, and a truncated run is LOGGED rather
# than silent, because a cap you cannot see is indistinguishable from
# "everything is up to date".
DAILY_MAX <- as.integer(Sys.getenv("OKCP_DAILY_MAX", "500"))

classify_new <- function(con, model = MODEL_BULK, limit = DAILY_MAX) {
  pending <- dbGetQuery(con, sprintf(
    "SELECT count(*) n FROM (%s)", gate_sql(coder_id_for(model))))$n
  if (!pending) { message("classify: nothing to do"); return(invisible(0L)) }
  if (pending > limit)
    message(sprintf(paste0("classify: %s posts pending, capping at %d this run ",
                           "(%s left over; raise OKCP_DAILY_MAX or use the ",
                           "batch path in 01/03 for a backlog)"),
                    format(pending, big.mark = ","), limit,
                    format(pending - limit, big.mark = ",")))
  classify_sync(con, model = model, limit = limit)
}

# ---- synchronous path (small runs, daily incremental) -----------------------
classify_sync <- function(con, model = MODEL_BULK, limit = Inf,
                          batch_size = CLASSIFY_BATCH, effort = AUDIT_EFFORT) {
  coder <- coder_id_for(model)
  todo <- dbGetQuery(con, paste(gate_sql(coder),
                                if (is.finite(limit)) paste("LIMIT", as.integer(limit)) else ""))
  if (!nrow(todo)) { message("classify: nothing to do"); return(invisible(0L)) }
  message(sprintf("classify(sync): %d posts, %d batches, model %s",
                  nrow(todo), ceiling(nrow(todo)/batch_size), model))

  n_ok <- 0L; usage <- c(in_ = 0, out = 0, cr = 0, cw = 0)
  chunks <- split(todo, ceiling(seq_len(nrow(todo)) / batch_size))
  for (k in seq_along(chunks)) {
    b <- chunks[[k]]
    res <- tryCatch({
      out <- anthropic_req("/v1/messages") |>
        req_body_json(build_body(b, model, effort), auto_unbox = TRUE) |>
        req_perform() |> resp_body_json()
      list(rows = results_to_rows(parse_classify_response(out), coder), u = out$usage)
    }, error = function(e) {
      m <- conditionMessage(e)
      message("  batch ", k, " ERR: ", m)
      if (.is_fatal_api_error(m))
        stop("ABORTING: the API rejected this and every later call will fail too.\n  ",
             m, "\n  ", n_ok, " posts were coded before this point; re-run to resume.",
             call. = FALSE)
      NULL })
    if (is.null(res)) next
    if (nrow(res$rows)) {
      db_upsert(con, "post_issues", as.data.frame(res$rows),
                c("post_id","issue_code","coder_id"))
      n_ok <- n_ok + nrow(b)
    }
    usage <- usage + c(res$u$input_tokens %||% 0, res$u$output_tokens %||% 0,
                       res$u$cache_read_input_tokens %||% 0,
                       res$u$cache_creation_input_tokens %||% 0)
  }
  report_cost(model, usage, n_ok, discount = 1)
  invisible(n_ok)
}

# ---- Message Batches API (bulk backfill, 50% off) ---------------------------
#
# Up to 100k requests per batch; most finish well inside an hour. Results come
# back in ARBITRARY order, so they are keyed by custom_id, never by position.
# SHARDED. The API accepts up to 100k requests per batch, but the limit that
# bites first is the size of the single POST: every request carries its posts'
# full text, so 2,269 requests is a multi-tens-of-MB upload that died with
# "LibreSSL ... bad record mac" — a TLS-level failure, not an HTTP status, so
# req_retry() cannot see it and the whole submission was lost. The eScribe run
# that worked was 212 requests. Sharding to that scale keeps each upload in
# known-good territory and makes a failure cost one shard instead of the run.
#
# Returns a CHARACTER VECTOR of batch ids.
classify_batch_submit <- function(con, model = MODEL_BULK, limit = Inf,
                                  batch_size = CLASSIFY_BATCH, since = NULL,
                                  max_requests = 250L) {
  coder <- coder_id_for(model)
  todo <- dbGetQuery(con, paste(gate_sql(coder, since),
                                if (is.finite(limit)) paste("LIMIT", as.integer(limit)) else ""))
  if (!nrow(todo)) { message("batch: nothing to do"); return(invisible(character())) }
  chunks <- split(todo, ceiling(seq_len(nrow(todo)) / batch_size))
  shards <- split(seq_along(chunks), ceiling(seq_along(chunks) / max_requests))
  message(sprintf("batch: %s posts -> %d requests -> %d shard(s), model %s",
                  format(nrow(todo), big.mark = ","), length(chunks), length(shards), model))

  ids <- character(); failed <- integer()
  for (s in seq_along(shards)) {
    kk <- shards[[s]]
    reqs <- lapply(kk, function(k) list(
      custom_id = sprintf("chunk-%05d", k),
      params    = build_body(chunks[[k]], model, AUDIT_EFFORT)))
    out <- tryCatch(
      anthropic_req("/v1/messages/batches") |>
        req_body_json(list(requests = reqs), auto_unbox = TRUE) |>
        req_perform() |> resp_body_json(),
      error = function(e) { message("  shard ", s, " FAILED: ", conditionMessage(e)); NULL })
    if (is.null(out)) { failed <- c(failed, s); next }
    # One RDS per batch id, holding only that shard's chunks, so the fetch step
    # can key results back to posts even if the shards land out of order.
    saveRDS(list(batch_id = out$id, model = model, chunks = chunks[kk]),
            file.path("output", paste0("batch_", out$id, ".rds")))
    ids <- c(ids, out$id)
    message(sprintf("  shard %d/%d submitted: %s (%d requests)",
                    s, length(shards), out$id, length(reqs)))
  }
  if (length(failed))
    message("shards that did NOT submit (re-run to retry, already-submitted posts are skipped): ",
            paste(failed, collapse = ", "))
  invisible(ids)
}

classify_batch_fetch <- function(con, batch_id) {
  meta <- readRDS(file.path("output", paste0("batch_", batch_id, ".rds")))
  coder <- coder_id_for(meta$model)

  st <- anthropic_req(paste0("/v1/messages/batches/", batch_id)) |>
    req_perform() |> resp_body_json()
  if (!identical(st$processing_status, "ended")) {
    message("batch ", batch_id, " status: ", st$processing_status,
            " (processing ", st$request_counts$processing, ")")
    return(invisible(FALSE))
  }

  # Results are JSONL, one object per line.
  lines <- anthropic_req(paste0("/v1/messages/batches/", batch_id, "/results")) |>
    req_perform() |> resp_body_string() |> strsplit("\n") |> unlist()
  lines <- lines[nzchar(lines)]

  n_ok <- 0L; usage <- c(in_ = 0, out = 0, cr = 0, cw = 0)
  for (ln in lines) {
    r <- fromJSON(ln, simplifyVector = FALSE)
    if (!identical(r$result$type, "succeeded")) {
      message("  ", r$custom_id, ": ", r$result$type); next
    }
    # meta$chunks is keyed by the same custom_id used at submission, so the
    # exact post_ids sent in this request are known and can be enforced.
    exp_ids <- tryCatch(meta$chunks[[sub("^chunk-0*", "", r$custom_id)]]$post_id,
                        error = function(e) NULL)
    rows <- tryCatch(results_to_rows(parse_classify_response(r$result$message), coder, exp_ids),
                     error = function(e) { message("  ", r$custom_id, " ERR: ", conditionMessage(e)); NULL })
    if (!is.null(rows) && nrow(rows)) {
      db_upsert(con, "post_issues", as.data.frame(rows), c("post_id","issue_code","coder_id"))
      n_ok <- n_ok + length(unique(rows$post_id))
    }
    u <- r$result$message$usage
    usage <- usage + c(u$input_tokens %||% 0, u$output_tokens %||% 0,
                       u$cache_read_input_tokens %||% 0,
                       u$cache_creation_input_tokens %||% 0)
  }
  report_cost(meta$model, usage, n_ok, discount = 0.5)   # Batches bill at 50%
  invisible(TRUE)
}

report_cost <- function(model, usage, n_posts, discount = 1) {
  p <- PRICES[[model]]; if (is.null(p)) return(invisible(NULL))
  cost <- discount * (usage[["in_"]] * p["input"] +
                      usage[["cw"]]  * p["input"] * 1.25 +
                      usage[["cr"]]  * p["input"] * 0.10 +
                      usage[["out"]] * p["output"]) / 1e6
  message(sprintf("%s: %d posts coded, approx US$%.3f%s (cache reads %s tok)",
                  model, n_posts, cost,
                  if (discount < 1) " [batch 50% off]" else "",
                  format(usage[["cr"]], big.mark = ",")))
}

`%||%` <- function(a, b) if (is.null(a)) b else a
