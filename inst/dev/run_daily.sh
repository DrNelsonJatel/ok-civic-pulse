#!/bin/bash
# run_daily.sh — wrapper launchd invokes once a day.
#
# WHY A WRAPPER AND NOT `Rscript inst/dev/02_daily.R` DIRECTLY:
#
#   * launchd runs with a minimal PATH and an arbitrary cwd. Every path in this
#     project is relative to the repo root, and R is not on launchd's default
#     PATH.
#   * DuckDB permits ONE writer. If a backfill, a weekly run, or yesterday's job
#     is still going, an unguarded daily run dies on a lock error — and since
#     launchd records only the exit code, that is indistinguishable from "ran
#     fine, nothing to do". The lock below turns a collision into a logged skip.
#   * A run that dies partway still needs to leave its log behind, so output is
#     appended to a dated file rather than discarded.
#
# LOCKING: macOS has no flock(1) (util-linux), so this uses mkdir, which is
# atomic on every POSIX filesystem. A PID file inside the lock directory lets a
# crashed run's stale lock be reclaimed instead of blocking the job forever.
#
# The daily job is intentionally cheap (a few pages per active thread, thanks to
# the resume offset), so a missed day is recovered by simply running again.
set -uo pipefail

# BSD date (macOS) has no -I/-Is; `date -Is` fails and substitutes an EMPTY
# string, which silently strips the timestamp off every log line — the one
# field that makes a scheduled job's log worth keeping.
ts() { date "+%Y-%m-%dT%H:%M:%S%z"; }

REPO="${OKCP_REPO:-$HOME/Projects/ok-civic-pulse}"
cd "$REPO" || { echo "cannot cd to $REPO"; exit 1; }

LOGDIR="$REPO/output/logs"
mkdir -p "$LOGDIR"
LOG="$LOGDIR/daily-$(date +%Y%m%d).log"
LOCKDIR="$REPO/db/.okcp.lock"

# Pick up the user's PATH (R, quarto, pandoc) rather than launchd's stub.
if [ -f "$HOME/.zprofile" ]; then . "$HOME/.zprofile" >/dev/null 2>&1 || true; fi
command -v Rscript >/dev/null || export PATH="/usr/local/bin:/opt/homebrew/bin:$PATH"
command -v Rscript >/dev/null || { echo "$(ts) Rscript not found" >>"$LOG"; exit 127; }

if ! mkdir "$LOCKDIR" 2>/dev/null; then
  # Lock exists. Reclaim it only if the recorded process is genuinely gone.
  if [ -f "$LOCKDIR/pid" ] && ! kill -0 "$(cat "$LOCKDIR/pid" 2>/dev/null)" 2>/dev/null; then
    echo "$(ts) reclaiming stale lock from pid $(cat "$LOCKDIR/pid")" >>"$LOG"
    rm -rf "$LOCKDIR"; mkdir "$LOCKDIR" 2>/dev/null || { echo "$(ts) SKIP: lock race" >>"$LOG"; exit 0; }
  else
    echo "$(ts) SKIP: another okcp job holds the lock" >>"$LOG"
    exit 0
  fi
fi
echo $$ > "$LOCKDIR/pid"
trap 'rm -rf "$LOCKDIR"' EXIT INT TERM

{
  echo "===== $(ts) daily start ====="
  Rscript inst/dev/02_daily.R; rc_daily=$?
  echo "----- daily rc=$rc_daily"
  # The report is regenerated only if collection succeeded: a PDF built from a
  # half-written corpus is worse than yesterday's PDF.
  if [ $rc_daily -eq 0 ]; then
    Rscript inst/dev/06_daily_report.R; echo "----- report rc=$?"
  else
    echo "----- report SKIPPED (daily failed)"
  fi
  echo "===== $(ts) daily end ====="
} >>"$LOG" 2>&1

# Prune logs older than 60 days so this never grows without bound.
find "$LOGDIR" -name 'daily-*.log' -mtime +60 -delete 2>/dev/null || true
