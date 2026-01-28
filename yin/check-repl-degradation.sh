#!/bin/bash
# check-repl-degradation: Analyze session log for signs of problems
# Usage: check-repl-degradation /path/to/session.log

LOG="${1:-}"

if [[ ! -f "$LOG" ]]; then
  echo "Usage: check-repl-degradation /path/to/session.log"
  exit 1
fi

echo ""
echo "=== Degradation Analysis ==="
echo "Log: $LOG"
echo ""

# Check 1: Startup time
STARTUP_END=$(grep -n "Ok, .* modules loaded\|All good\|Loaded\." "$LOG" | head -1 | cut -d: -f1)
if [[ -n "$STARTUP_END" ]]; then
  STARTUP_LINES=$STARTUP_END
  echo "✓ GHCi startup: $STARTUP_LINES lines (typical: 10-15)"
  if [[ $STARTUP_LINES -gt 20 ]]; then
    echo "  ⚠ WARNING: Startup output is large. Consider:"
    echo "    - Check .ghci config for verbose output"
    echo "    - Use ghcid for faster iteration (no recompile each query)"
  fi
else
  echo "✗ Startup not completed"
fi

echo ""

# Check 2: Build failures
BUILD_ERRORS=$(grep -c "error:\|failed\|Error" "$LOG" || echo 0)
if [[ $BUILD_ERRORS -gt 0 ]]; then
  echo "✗ Build errors detected: $BUILD_ERRORS"
  echo "  Recommendation: run 'cabal build' before repl queries"
  grep "error:" "$LOG" | head -3 | sed 's/^/    /'
else
  echo "✓ No build errors"
fi

echo ""

# Check 3: Repeated compilation
COMPILE_LINES=$(grep -c "Compiling.*interpreted\|Recompiling" "$LOG" || echo 0)
if [[ $COMPILE_LINES -gt 3 ]]; then
  echo "⚠ Multiple compilations detected: $COMPILE_LINES"
  echo "  If doing many queries, consider:"
  echo "    - Use ghcid for live reloading (faster than cabal repl for iteration)"
  echo "    - Or persist your session (don't kill/restart repl between queries)"
else
  echo "✓ Compilation count normal"
fi

echo ""

# Check 4: Query timeouts
TIMEOUTS=$(grep -c "Timeout\|✗" "$LOG" || echo 0)
if [[ $TIMEOUTS -gt 0 ]]; then
  echo "⚠ Query timeouts or failures: $TIMEOUTS"
  echo "  Check observer log or ghci-out.txt for details"
else
  echo "✓ No query failures"
fi

echo ""
echo "=== Recommendations ==="
echo ""
echo "Use this card when switching projects:"
echo "  1. Check project has cabal.project or *.cabal"
echo "  2. Run 'cabal build' first (catches errors early)"
echo "  3. Only then use repl for type queries"
echo ""
echo "If doing heavy iteration:"
echo "  Consider ghcid instead:"
echo "    ghcid -c 'cabal repl' --test ':main'"
echo ""
