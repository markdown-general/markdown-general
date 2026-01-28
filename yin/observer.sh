#!/bin/bash
# observer: Send queries and read responses
# Based on: card-repl-query-response.md

INPUT="/tmp/ghci-in.txt"
OUTPUT="/tmp/ghci-out.txt"

send_query() {
  local QUERY="$1"
  local EXPECT="$2"

  echo "Sending: $QUERY"

  # Write query to input file
  echo "$QUERY" >> "$INPUT"

  # Poll output file (up to 20 attempts, 100ms each = 2 seconds)
  for attempt in {1..20}; do
    sleep 0.1

    if [[ -f "$OUTPUT" ]]; then
      # Check if we have the expected pattern in the output
      # (don't require query echo—GHCi might not echo piped input)
      if grep -q "$EXPECT" "$OUTPUT"; then
        echo "  ✓ Got response with: $EXPECT"
        return 0
      fi
    fi
  done

  echo "  ✗ Timeout (pattern not found: $EXPECT)"
  return 1
}

echo "observer starting"
echo ""

PASSED=0
FAILED=0

# Run test queries
send_query ":t fmap" "fmap ::" && ((PASSED++)) || ((FAILED++))
send_query ":k Maybe" "* -> *" && ((PASSED++)) || ((FAILED++))
send_query ":t run" "run ::" && ((PASSED++)) || ((FAILED++))
send_query ":t base" "base ::" && ((PASSED++)) || ((FAILED++))
send_query ":t pipe" "pipe ::" && ((PASSED++)) || ((FAILED++))

echo ""
echo "========================================"
echo "Results: $PASSED passed, $FAILED failed"
echo "========================================"

if [[ $FAILED -eq 0 ]]; then
  echo "✓ Protocol works!"
  exit 0
else
  echo "✗ Some tests failed"
  exit 1
fi
