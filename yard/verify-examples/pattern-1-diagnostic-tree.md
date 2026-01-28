# pattern: diagnostic tree ⟜ graduated answer paths

**What it is:** Three-level verification cascade. Don't jump to expensive operations; try fast → safe → complete in order.

**When to use:** Operator has a question but different time/risk budgets.

---

## Example: Build Status

**verification** ⟜ what counts as success at each level?

**Level 1 (cached, instant):**
- Success: build-log.md exists and is recent (< 5 min old)
- Test: `[ -f /tmp/build-log.md ] && [ $(stat -f%m /tmp/build-log.md) -gt $(($(date +%s) - 300)) ]`
- Failure: log missing or stale → proceed to Level 2
- Evidence: Output last 20 lines of log

**Level 2 (interactive, safe):**
- Success: cabal repl launches without errors, can evaluate `:t putStrLn`
- Test: `echo ":t putStrLn" | cabal repl 2>&1 | grep -q "String ->"`
- Failure: repl won't launch → proceed to Level 3
- Evidence: Show cabal repl error message

**Level 3 (complete, slow):**
- Success: cabal build exits with code 0
- Test: `cabal build && echo "✓ Build succeeded" || echo "✗ Build failed"`
- Failure: build errors → STOP, report errors
- Evidence: Show full cabal build output

---

## instruction

**Try in order. Use whichever level answers your question.**

```bash
# Level 1: Is there a recent build log?
tail -20 /tmp/build-log.md

# Level 2: Can the REPL load?
echo ":t putStrLn" | cabal repl

# Level 3: Does a fresh build work?
cabal build
```

---

## verification protocol

**AFTER Level 1:**
- If log is recent and shows ✓, use it. Done.
- If log is stale or missing, proceed to Level 2.
- If you proceed but later need Level 3, that's ok. This is graduated risk.

**AFTER Level 2:**
- If repl launches and evaluates, it's safe to edit. Done.
- If repl won't launch, build is broken. Proceed to Level 3.

**AFTER Level 3:**
- If build succeeds: repo is healthy.
- If build fails: STOP. Report errors. Do not proceed.

**Safety check:**
- Between levels, git status must show no unexpected changes.
- If files modified unexpectedly, investigate before proceeding.

---

## comment

Success here is **graduated transparency**, not binary. Operator knows:
- Fast answer (log): might be stale but good enough for "is it building?"
- Safe answer (repl): expensive but no compilation, good for "can I edit?"
- Complete answer (build): slow but authoritative, good for "is the repo healthy?"

The cascade respects operator time budget while maintaining evidence.
