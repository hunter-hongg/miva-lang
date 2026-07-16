#!/usr/bin/env bash
# Night-shift runner: OpenCode non-interactive, adds TOML + YAML libs to Miva.
# Detached & self-contained: survives parent session exit, hard time cap, writes DONE marker.
set -uo pipefail

REPO="$HOME/projects/miva-lang"
LOG_DIR="$REPO/.nightshift-logs"
mkdir -p "$LOG_DIR"
TS="$(date +%Y%m%d-%H%M%S)"
LOG="$LOG_DIR/run-$TS.log"
DONE="$LOG_DIR/DONE"
LATEST="$LOG_DIR/latest.log"

# Overall hard cap so it can never run past the morning (3h).
MAX_SECONDS=${MAX_SECONDS:-10800}

rm -f "$DONE"
ln -sf "$LOG" "$LATEST"

cd "$REPO" || { echo "repo missing" > "$DONE"; exit 1; }

PROMPT="$(cat "$REPO/.nightshift-toml-yaml.md")"

{
  echo "=== Night shift START $(date -Is) ==="
  echo "=== Model: kilo/kilo-auto/free  MAX_SECONDS=$MAX_SECONDS ==="
} | tee -a "$LOG"

timeout --signal=TERM --kill-after=60 "$MAX_SECONDS" \
  opencode run \
    --model "kilo/kilo-auto/free" \
    --auto \
    --log-level INFO \
    --title "night-shift: TOML+YAML libs" \
    "$PROMPT" >> "$LOG" 2>&1

STATUS=$?

{
  echo "=== Night shift END $(date -Is) exit=$STATUS ==="
  if [ "$STATUS" -eq 124 ] || [ "$STATUS" -eq 137 ]; then
    echo "=== NOTE: hit MAX_SECONDS time cap ($MAX_SECONDS s) ==="
  fi
} | tee -a "$LOG"

# DONE marker holds the exit status for the cron reporter to read.
echo "$STATUS" > "$DONE"
exit "$STATUS"
