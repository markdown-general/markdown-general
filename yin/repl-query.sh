#!/bin/bash
# repl-query: Send a clean query and get clean response
# Usage: repl-query ":t Something"

QUERY="${1:-}"

if [[ -z "$QUERY" ]]; then
  echo "Usage: repl-query ':t Something'"
  echo "       repl-query ':k Maybe'"
  echo "       repl-query ':i Functor'"
  exit 1
fi

if [[ ! -f /tmp/ghci-session.env ]]; then
  echo "No active session. Start with: repl-session-start /path/to/project"
  exit 1
fi

source /tmp/ghci-session.env

# Remember how many lines were in output before query
BEFORE=$(wc -l < /tmp/ghci-out.txt 2>/dev/null || echo 0)

# Send query
echo "$QUERY" >> /tmp/ghci-in.txt

# Poll for new response (up to 2 seconds)
for attempt in {1..20}; do
  sleep 0.1

  if [[ -f /tmp/ghci-out.txt ]]; then
    AFTER=$(wc -l < /tmp/ghci-out.txt 2>/dev/null || echo 0)

    # If we got new lines, extract them
    if [[ $AFTER -gt $BEFORE ]]; then
      # Get only the new lines (the response)
      RESPONSE=$(tail -n +$((BEFORE + 1)) /tmp/ghci-out.txt | head -5)

      if [[ -n "$RESPONSE" ]] && ! echo "$RESPONSE" | grep -q "^$"; then
        # Clean up: remove ghci prompts
        CLEAN=$(echo "$RESPONSE" | sed 's/^ghci> //' | sed 's/^Prelude> //')
        echo "$CLEAN"
        # Log to session
        echo "Query: $QUERY" >> "$SESSION_LOG"
        echo "Response: $CLEAN" >> "$SESSION_LOG"
        exit 0
      fi
    fi
  fi
done

echo "✗ No response (timeout or repl not ready)"
exit 1
