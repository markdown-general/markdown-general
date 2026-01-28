#!/bin/bash
# repl-session-start: Begin a logged repl session
# Usage: repl-session-start /path/to/project [name]

PROJECT_DIR="${1:-.}"
SESSION_NAME="${2:-default}"

# Setup
LOGS_DIR="/Users/tonyday567/mg/yin/logs"
mkdir -p "$LOGS_DIR"

TIMESTAMP=$(date +%Y%m%d-%H%M%S)
SESSION_LOG="$LOGS_DIR/session-$TIMESTAMP-$SESSION_NAME.log"
SESSION_PID_FILE="/tmp/ghci-session.pid"

# Initialize communication files
rm -f /tmp/ghci-*.txt /tmp/ghci-repl.pid

# Start logging (capture both stdout/stderr)
{
  echo "=== REPL SESSION START ==="
  echo "Timestamp: $(date)"
  echo "Project: $PROJECT_DIR"
  echo "Session name: $SESSION_NAME"
  echo "Log: $SESSION_LOG"
  echo ""
} | tee "$SESSION_LOG"

# Start repl-startup and capture its output (pass full path)
/Users/tonyday567/mg/yin/repl-startup.sh "$PROJECT_DIR" 2>&1 | tee -a "$SESSION_LOG" &

REPL_PID=$!
echo "$REPL_PID" > "$SESSION_PID_FILE"
echo "REPL PID: $REPL_PID" | tee -a "$SESSION_LOG"
echo "Session file: $SESSION_LOG" | tee -a "$SESSION_LOG"
echo ""

# Store session info for later use
echo "SESSION_LOG=$SESSION_LOG" > /tmp/ghci-session.env
echo "REPL_PID=$REPL_PID" >> /tmp/ghci-session.env
echo "PROJECT_DIR=$PROJECT_DIR" >> /tmp/ghci-session.env

echo "✓ Session started. Log at: $SESSION_LOG"
echo "  To query: repl-query ':t Something'"
echo "  To watch: repl-ticker"
echo "  To stop:  repl-session-stop"
