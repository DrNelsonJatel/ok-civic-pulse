# Scope: distilling the Opus classifier into a free, local, reproducible coder

Status: **proposed, not started.** Written 2026-08-07.

## Why do this at all

Not to save money. The one-time Opus pass over the current corpus is ~$25–45,
which is not worth days of ML engineering to avoid.

Two reasons that *are* worth it:

1. **Reproducibility.** This project is heading for a CC-BY release with a DOI.
   A central measurement that requires a paid API key is one nobody else can
   re-run. A local classifier, shipped with pinned weights and a seed, makes
   the pipeline reproducible from a clean clone — and lets the methods section
   report a measured kappa for *both* coders instead of asserting the paid one
   is fine.
2. **Recurring cost of the daily job.** A one-time backfill is cheap; a daily
   classifier running to October 2026 and beyond is not. Distillation moves the
   ongoing cost to ~zero and keeps the API for sampling and drift control.

## Prerequisite: do NOT start before the full Opus pass

The current label set is unsuitable as training data, and the reason is not
its size but its **composition**:

| source | labelled posts | share |
|---|---|---|
| kelowna_escribe (council agendas) | 2,231 | 77% |
| castanet_forums (resident comments) | 666 | 23% |

2,897 labelled posts, 4,570 label rows. A model distilled from this would learn
mostly procedural agenda prose ("THAT Council authorizes...", "receive for
information") and then be applied to forum comments — a domain shift severe
enough to invalidate the exercise. Council text is also in-scope by
construction, so the scope head would learn a prior that is simply wrong for
resident text.

After the Castanet crawl and its Opus pass, the training set inverts to roughly
15k posts dominated by resident comment. **That** is the set to distil from.
Sequencing this after task #11 is not scheduling convenience; it is the
difference between a usable model and a useless one.

## Class support decides what is learnable

From the 4,570 current label rows (post-crawl numbers will be larger, but the
*shape* of the tail will not change much — rare codes are rare because the
issues are rare):

| band | n classes | examples |
|---|---|---|
| n ≥ 100 — trainable | 12 | governance_process (982), land_use_zoning (705), development_approvals (422), housing (361), tax_budget (271), fire_emergency (168) |
| 30 ≤ n < 100 — marginal | 9 | transit_active (67), policing_bylaw (64), climate_flood (40), short_term_rentals (30) |
| n < 30 — not learnable | 8 | homelessness_social (26), crown_corps (11), health_care (8), school_board (6), immigration (2) |

Consequence: this is a **hybrid**, not a replacement. The 12 head classes carry
every published finding and can be distilled. The tail cannot, and must stay
with the sieve or the API. Any design that pretends otherwise will silently
drop rare codes — and rare codes include `school_board` and `regional_district`,
which are *ballot* scope and therefore election-relevant out of proportion to
their volume.

## Decompose by task — they are not one problem

| target | method | rationale |
|---|---|---|
| `jurisdiction` | **gazetteer, no ML** | 19 known communities. Deterministic place-name matching with the existing `(?<!West )Kelowna` guards is more reliable than any classifier and fully reproducible. Removes a third of the schema from the ML problem for free. |
| `issue_code` (12 head classes) | embeddings + one-vs-rest logistic regression | Multi-label (mean 1.58 codes/post, max 11), so per-class binary heads with tuned thresholds — not softmax. |
| `scope` | single multiclass head | Support: local 3253, shared 626, none 363, federal 179, provincial 128, ballot 21. `ballot` is too rare to learn — assign it by rule from issue_code (school_board, regional_district) rather than by model. |
| `stance` | keep with API, or omit | neutral 3309 / oppose 716 / support 321 / mixed 224. Hardest target, most context-dependent, and least load-bearing for the findings. Do not distil it in v1. |
| `salience`, `confidence` | not distilled | Emit calibrated probability as `confidence`; leave `salience` NULL for the local coder. |

## Model choice

Primary: **sentence-transformer embeddings + scikit-learn**, via the existing
`reticulate` bridge. Frozen encoder, trained linear heads. Trains in minutes on
CPU; inference over 30k posts is seconds.

Pinning matters for reproducibility: record the HF model id **and revision**.
Ship the trained heads (small, a few MB) with the release; do **not** ship
embeddings of the corpus — embeddings are partially invertible and would be a
back door around the rule that comment text never leaves the machine.

Fallback if the Python environment proves unreliable (it is already shaky —
`py_viz.R` reports `bertopic` and `scattertext` missing, and renv does not
manage Python): **pure R** with `quanteda.textmodels` (NB/SVM) or `glmnet` on
tf-idf. Lower ceiling, zero Python, still free and reproducible. Worth building
the evaluation harness first so the two can be compared on equal terms.

## Integration — it fits the existing architecture

The schema already anticipated multiple coders, so this needs no migration:

* New `coder_id`: `local:distil-v1` (version it — a retrained model is a new
  coder, not an overwrite).
* Add to `CODER_PRECEDENCE` in `R/metrics.R`, **below** the API models and
  above `keyword-sieve`. `preferred_codes_sql()` then does the right thing
  automatically: where Opus labels exist they win; the local coder fills gaps.
* New files: `R/classify_local.R` (inference), `R/gazetteer.R` (jurisdiction),
  `inst/dev/08_train_distil.R` (training + evaluation report).
* `02_daily.R` gains a switch (`OKCP_CODER=local|api`) so the daily job can run
  free while a monthly sample still goes to the API for drift control.

## Evaluation protocol — the part that decides whether this ships

Distillation is only legitimate if it is measured, in the same units as the
thing it replaces.

1. **Hold out the audit sample entirely** from training. It is the only
   human-standard evidence and must not be contaminated.
2. Report, on the human-adjudicated sample: **Cohen's kappa for the local coder
   and for Opus, side by side**, plus the sieve as a floor. Three numbers, one
   table. This is the deliverable that justifies the substitution — or kills it.
3. Report **per-class precision/recall, never only macro-averaged**. A macro
   average hides exactly the tail failure this design predicts. Any class the
   model cannot do must be visible in the output, not averaged away.
4. Re-run the headline findings under both coders and compare. If the attention
   gap, the scope split and the emotion gap are direction-consistent and close
   in magnitude, the substitution is safe for publication. If a headline moves,
   it does not ship.

Note this depends on the audit sample being finished — currently 20/100 human
labels. **Kappa for the local coder is blocked by the same adjudication that
blocks kappa for Opus.**

## Decision gates

* If local kappa is materially below Opus kappa on the head classes → **do not
  substitute.** Fall back to the hybrid below.
* **Hybrid fallback (still valuable):** use the local model as a *pre-filter*
  rather than a coder — it decides which posts are worth sending to the API.
  That cuts recurring cost substantially with no quality risk to the published
  labels, and it is a strictly easier target than matching Opus.
* If the Python environment costs more than half a day to make reproducible →
  switch to the pure-R path rather than fighting it.

## Effort

Roughly 2–3 working days, sequenced after task #11:

| phase | est. |
|---|---|
| Python env provisioning + pinning (or R fallback) | 0.5 d |
| Gazetteer for jurisdiction | 0.25 d |
| Training pipeline + threshold tuning | 0.75 d |
| Evaluation harness and comparison report | 0.5 d |
| Integration (coder precedence, daily switch, QAQC) | 0.5 d |

## Risks

* **Teacher lock-in.** The student inherits Opus's biases and cannot exceed it.
  Any systematic error in the Opus labels becomes permanent and invisible.
  Mitigated only by the human audit.
* **Drift.** Election-season vocabulary in Sept–Oct 2026 will differ from the
  training window. Keep the monthly API sample and re-check kappa before the
  election, not after.
* **Tail silence.** See the per-class reporting rule above; this is the failure
  mode most likely to slip through.
* **Environment reproducibility.** A local model that only runs on this laptop
  defeats the entire purpose. Pin the encoder revision, or take the R path.
