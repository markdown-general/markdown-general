#!/bin/bash
# repl-session-stop: End a logged repl session, run degradation check

if [[ ! -f /tmp/ghci-session.env ]]; then
  echo "No active session"
  exit 1
fi

source /tmp/ghci-session.env

# Kill repl
if [[ -n "$REPL_PID" ]]; then
  kill $REPL_PID 2>/dev/null
  wait $REPL_PID 2>/dev/null
fi

# Log session end
{
  echo ""
  echo "=== REPL SESSION STOP ==="
  echo "Timestamp: $(date)"
  echo ""
} >> "$SESSION_LOG"

# Run degradation check
echo "Checking for degradation patterns..."
/Users/tonyday567/mg/yin/check-repl-degradation.sh "$SESSION_LOG"

# Cleanup
rm -f /tmp/ghci-session.env /tmp/ghci-session.pid

echo "✓ Session ended. Log saved: $SESSION_LOG"
