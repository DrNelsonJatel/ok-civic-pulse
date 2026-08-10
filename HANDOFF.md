# Handoff — state of play, 10 August 2026

Written when the project was set aside for a few days. Everything below is
verifiable from the repo or the database; nothing here is a plan or an
intention.

## One-line status

The pipeline is complete, running autonomously, and producing findings. The
only thing blocking publication is 80 human adjudication decisions, which
nobody but Nelson can make.

## What is running while you are away

`ca.limnology.okcp.daily` fires at **06:20 local, every day** via launchd
(`~/Library/LaunchAgents/ca.limnology.okcp.daily.plist`). Each run:

1. crawls recently-active threads across nine forums,
2. sieves, then classifies up to `OKCP_DAILY_MAX` (default **500**) new posts,
3. rebuilds `issue_daily`, exports the serve copy, renders the branded PDF to
   `output/reports/civic-pulse-<date>.pdf`.

**Cost: roughly US$1.30/day** at the measured rate (US$0.0026/post), and it is
bounded — the 500-post cap means a runaway backlog cannot produce a runaway
bill. A few days away is a few dollars.

To pause it:

    launchctl unload ~/Library/LaunchAgents/ca.limnology.okcp.daily.plist

To resume:

    launchctl load ~/Library/LaunchAgents/ca.limnology.okcp.daily.plist

Logs land in `output/logs/daily-YYYYMMDD.log` (git-ignored, pruned at 60 days).
The wrapper holds a `db/.okcp.lock` mkdir-lock, so a manual job started while
the cron is mid-run logs a clean skip instead of colliding on DuckDB's
single-writer lock.

## Corpus

| | |
|---|---|
| posts | 42,828 (39,364 Castanet + 2,644 eScribe + 153 hearing testimony) |
| coded rows | 36,093 model/human + sieve |
| reply edges | 25,325 |
| window | Castanet 2007–2026 (analysis clipped to 2025-01-13+), eScribe Jan 2025 – Aug 2026 |
| codebook | v3, hash `e4711af5ea64` |
| classification spend | US$51.16 for the Jan-2025+ window (22,660 posts) + ~US$5 earlier |

Freshness at handoff: Castanet 1 day, eScribe 14 days (normal — council does
not sit through summer recess).

## Findings currently on the dashboard

* **57%** of Okanagan resident comments raising an issue raise one a council
  can act on — against **16%** in the out-of-region forums (B.C., Kamloops).
  This figure is reported per forum set on purpose; pooling all nine moves it
  by ~9 points purely by choice of what to crawl.
* **Attention gap** (share of locally-actionable mentions within each corpus,
  2025-01-13 → 2026-08-08): transit 11.6% resident vs 1.8% agenda; policing
  10.5% vs 1.3%; fire 8.0% vs 0.6%. Inverted: zoning 3.0% vs **22.3%**.
* **Emotion**: out-of-scope discourse is angrier (1.97 vs 1.19 per 100 words)
  and more fearful (2.77 vs 1.78). Trust is level, and the finding says so
  rather than asserting a gap that is not there.
* **ERGM** (last 365d, 655 nodes, 4,168 ties): edges −5.23, mutual +5.06.
  Reciprocity dominates, holding across a graph 4× the pilot's.

## What is blocked, and on whom

1. **80 adjudication items — Nelson only.** `shiny::runApp("app/adjudicate.R")`,
   local only, never deploy it (it displays comment text and real names). The
   queue is already ordered by information value: `skipped_out_of_scope` first
   (where the first audit found the model under-calling local 14-to-1 — false
   negatives that checking gated-in items cannot detect), then lowest model
   confidence, uncoded posts first. The panel shows the stratum so you know
   which question you are answering. ~1 hour.

   This one item gates **three** things: Cohen's kappa, the Zenodo DOI, and any
   future validation of a distilled local classifier.

2. **Whether to deepen eScribe** to other Okanagan councils (Vernon, Penticton,
   West Kelowna). The corpus is now 94% resident voice; the council side is
   Kelowna only. Worth deciding before write-up, because it changes what the
   attention gap is a gap *about*.

## Open work, in priority order

| # | item | notes |
|---|---|---|
| 1 | Finish adjudication | see above; blocks everything downstream |
| 2 | Strip quoted blocks before sieving | see `inst/notes/sieve-quote-inclusion.md`; needs its own migration, changes `body_local` semantics for 42k posts |
| 3 | Distillation | scoped in `inst/notes/distillation-scope.md`; deliberately sequenced after the Opus pass, which is now done, so it is unblocked |
| 4 | Email delivery for the daily PDF | PDF itself works |
| 5 | EngagementHQ / Social Pinpoint collector | XHR mapping needed |
| 6 | Cut `v0.1-pilot` + ORCID in CITATION.cff | after kappa exists |

## Backups — read this before trusting the machine

`db/civic_pulse.duckdb` (117 MB) is git-ignored by design: it holds comment
text, which Castanet's ToU forbids redistributing, and real resident names from
council minutes. It therefore lives in exactly one place and contains ~5 days
of crawling plus US$56 of classification.

**Time Machine reports this path as included but its destination failed to
mount**, so at handoff nothing was actually backing it up. Fixing that is a
Nelson task (reconnect the backup drive).

`inst/dev/09_backup.R` writes two tiers to `~/Backups/ok-civic-pulse`:

* **Tier 1** — full snapshot, verified by opening it and counting rows (a copy
  that will not open is not a backup). Contains text; must not leave the
  machine.
* **Tier 2** — `labels-*.duckdb`, 6.8 MB, de-identified: codes, sentiment,
  metadata, edges, cursors. No `body_local`, no handles, no titles; the script
  refuses to write it if any text column sneaks in. **This one is safe to store
  offsite**, and it means total disk loss costs only the re-crawlable text
  (free, a few days) rather than the US$56 of labels. Restore by re-crawling
  and joining on `post_id` / `native_id`.

Run it before any risky migration:

    Rscript inst/dev/09_backup.R

## How to pick this up

    cd ~/Projects/ok-civic-pulse
    git pull
    Rscript inst/dev/07_qaqc.R        # should be PASS, 0 failed
    tail -20 output/logs/daily-$(date +%Y%m%d).log

If QAQC fails or the daily log shows `rc=1`, read the log before touching
anything — the failure modes this project produces are almost always silent
ones that look like success.

## Fresh-clone gotcha

    git config core.hooksPath .githooks

Without it the pre-commit leak guard does not run.

## Standing constraints (do not relax these)

* Comment text and speaker names never leave the machine. `db/` is
  git-ignored; every export is de-texted; `app/adjudicate.R` is local-only.
* Castanet ToU forbids redistribution. Honest UA, ≥2 s delay, `/posting.php`
  disallowed.
* Council minutes name real residents. Names live only in `body_local` and
  `actors.handle`; `actor_key` is a hash.
* Reddit is parked by policy (research must go via RFR; free tier is
  non-commercial only).
* `OKCP_GATE` stays `all`. The measured false-negative rate (26.7% / 20.0%) and
  the quote-inclusion artifact both argue against gating.
* Never commit `.duckdb`, `output/batch_*.rds`, `output/py_local/`, `.Renviron`.

## A pattern worth remembering

Nearly every bug found in this project was a **silent failure that looked like
success**: a backfill reporting "DONE" having lost 80% of its work; a scheduler
that would have collected nightly and analysed nothing, forever; an Action tab
reporting "not enough coded data" on 35,462 coded rows; a staleness guard that
could never fire because future-dated council meetings held it down. Most of
the guards now in the codebase exist to make that class of failure loud. When
something here looks fine, check that it *can* fail.
