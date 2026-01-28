#!/bin/bash
# repl-ticker: Watch ghci output nicely (filtered, recent activity)
# Shows last N lines, filters noise, auto-updates

if [[ ! -f /tmp/ghci-session.env ]]; then
  echo "No active session. Start with: repl-session-start /path/to/project"
  exit 1
fi

source /tmp/ghci-session.env

N="${1:-15}"  # Default: show last 15 lines

echo "=== REPL Ticker (refreshing every 1s) ==="
echo "Showing last $N lines of /tmp/ghci-out.txt"
echo "Press Ctrl+C to stop"
echo ""

while true; do
  clear
  echo "=== REPL Output (last $N lines) ==="
  echo ""

  if [[ -f /tmp/ghci-out.txt ]]; then
    # Show last N lines, skip empty lines and noise
    tail -$N /tmp/ghci-out.txt | \
      grep -v "^$" | \
      sed 's/^/  /'
  else
    echo "  (no output yet)"
  fi

  echo ""
  echo "Last updated: $(date '+%H:%M:%S')"
  echo "Project: $(basename $PROJECT_DIR)"
  echo ""
  echo "To query: repl-query ':t Something'"
  echo "To stop:  Press Ctrl+C"

  sleep 1
done
