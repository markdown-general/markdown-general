# card: action-cabal-test ⟜ run unit test suite

**type** ⟜ action / testing

**execution** ⟜ bounded worker / 90s timeout (variable; may need adjustment)

**input** ⟜ repo directory (must build successfully)

**task** ⟜ cabal test all 2>&1; run all unit tests and report results

**output** ⟜ ✓/✗ + [pass-count]/[total-count] + [fail-count if any]

**capture** ⟜ save full output to /tmp/cabal-test-[repo].log for inspection

**idempotence** ⟜ fully idempotent (tests should be deterministic)

**effects** ⟜ reads: test suite + src code, writes: ~/repos/[repo]/dist-newstyle/, spawns: none, network: no

---

## Pre-Check

Verify build exists:
```bash
cabal build all --dry-run > /dev/null 2>&1
if [ $? -ne 0 ]; then
  echo "✗ Project does not build; cannot run tests"
  exit 1
fi
```

---

## Test Execution

```bash
timeout 90 cabal test all 2>&1 | tee /tmp/cabal-test-[repo].log
TEST_RESULT=$?

if [ $TEST_RESULT -eq 0 ]; then
  echo "✓ ALL TESTS PASSED"

  # Parse output for counts (format varies by test framework)
  PASS_COUNT=$(grep -c "passed" /tmp/cabal-test-[repo].log || echo "?")
  echo "Passed: $PASS_COUNT"

elif [ $TEST_RESULT -eq 124 ]; then
  echo "✗ TEST TIMEOUT (exceeded 90s)"
  tail -20 /tmp/cabal-test-[repo].log

else
  echo "✗ TEST FAILURES DETECTED"

  # Try to extract failure count
  FAIL_COUNT=$(grep -c "FAIL" /tmp/cabal-test-[repo].log || echo "?")
  echo "Failed: $FAIL_COUNT"

  # Show last 30 lines of output
  echo ""
  echo "Last test output:"
  tail -30 /tmp/cabal-test-[repo].log
fi
```

---

## Output Format

Success:
```
action-cabal-test | [repo] | ✓ | all-tests-passed | [pass-count]
```

Failure:
```
action-cabal-test | [repo] | ✗ | [fail-count]-failures | [pass-count]-passed
```

Timeout:
```
action-cabal-test | [repo] | ✗ | timeout | exceeded 90s
```

---

## Notes

- Timeout 90s is typical; large test suites may need adjustment
- Test results depend on deterministic test code; flaky tests will cause variability
- Full output captured to /tmp for inspection if needed
- If tests are missing entirely, `cabal test all` may report no tests found (not an error, just no-op)
- Used by: cleanit (comprehensive quality check)

Common test failure patterns:
- Dependency version mismatch (fixed by dependendit)
- Timeout (test suite is slow; increase timeout or optimize tests)
- Environment variable missing (document in yin-notes)
- Pragma/language issue (fixit handles)

