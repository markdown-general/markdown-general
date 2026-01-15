# card: action-cabal-docspec ⟜ run documentation tests

**type** ⟜ action / testing

**execution** ⟜ bounded worker / 60s timeout

**input** ⟜ repo directory (must have cabal-docspec available; optional if not installed)

**task** ⟜ cabal-docspec 2>&1; test code examples in documentation

**output** ⟜ ✓/✗ + [pass-count]/[total-count] + [fail-count if any] (or "not-installed")

**capture** ⟜ save output to /tmp/cabal-docspec-[repo].log

**idempotence** ⟜ fully idempotent (doc examples should be deterministic)

**effects** ⟜ reads: doc examples in source, writes: ~/repos/[repo]/dist-newstyle/, spawns: none, network: no

---

## Pre-Check

Verify cabal-docspec is available (optional tool):
```bash
which cabal-docspec > /dev/null 2>&1
if [ $? -ne 0 ]; then
  echo "✓ cabal-docspec not installed; skipping"
  echo "Install with: cabal install cabal-docspec (optional)"
  exit 0  # Not an error; optional tool
fi
```

---

## Run Documentation Tests

```bash
timeout 60 cabal-docspec 2>&1 | tee /tmp/cabal-docspec-[repo].log
RESULT=$?

if [ $RESULT -eq 0 ]; then
  echo "✓ DOCUMENTATION TESTS PASSED"

  # Parse output for counts
  PASS_COUNT=$(grep -c "✓" /tmp/cabal-docspec-[repo].log || echo "?")
  echo "Passed: $PASS_COUNT examples"

elif [ $RESULT -eq 124 ]; then
  echo "✗ DOCUMENTATION TESTS TIMEOUT (exceeded 60s)"
  tail -20 /tmp/cabal-docspec-[repo].log

else
  echo "✗ DOCUMENTATION TEST FAILURES"

  FAIL_COUNT=$(grep -c "✗" /tmp/cabal-docspec-[repo].log || echo "?")
  echo "Failed: $FAIL_COUNT examples"

  echo ""
  echo "Failures:"
  grep -A 2 "✗" /tmp/cabal-docspec-[repo].log | head -30
fi
```

---

## Output Format

Success:
```
action-cabal-docspec | [repo] | ✓ | all-examples-pass | [pass-count]
```

Failure:
```
action-cabal-docspec | [repo] | ✗ | [fail-count]-failures | [pass-count]-passed
```

Not installed (OK):
```
action-cabal-docspec | [repo] | ✓ | not-installed | optional-tool
```

Timeout:
```
action-cabal-docspec | [repo] | ✗ | timeout | exceeded 60s
```

---

## Notes

cabal-docspec tests code examples found in:
- Haddock documentation (in source comments)
- README.md (if configured in .cabal)
- Other documented files

This is optional but valuable: catches documentation drift (when examples become outdated).

If not installed, action succeeds silently; not an error.

Used by:
- cleanit (optional quality check for doc accuracy)

Common issues:
- Example imports wrong (fixit + document update)
- Example output incorrect (update documentation)
- GHC version mismatch in example (pragma needed)

