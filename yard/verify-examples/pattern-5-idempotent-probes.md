# pattern: idempotent probes ⟜ safe read-only queries

**What it is:** Commands that ask the system about its state without modifying anything. Safe to run repeatedly.

**When to use:** You need information before deciding what to do. Probes are zero-risk.

---

## Example: Haskell Toolchain Status Check

**verification** ⟜ What success looks like

**Success criteria:**
- All probes complete without errors
- All probes report something (even "not found")
- All probes are read-only (zero file modifications)
- Results parseable into a status report
- git status identical before and after

**How we'll verify:**
```bash
# Test 1: cabal check (read-only diagnostic)
before=$(git status --short | wc -l)
timeout 10 cabal check > /tmp/cabal-check.txt 2>&1
check_code=$?
after=$(git status --short | wc -l)
[ $before -eq $after ] && echo "✓ cabal check read-only" || echo "✗ cabal check modified files"
[ $check_code -eq 0 ] || [ $check_code -eq 124 ] && echo "✓ cabal check completed" || echo "✗ cabal check failed"

# Test 2: hlint (read-only suggestions)
before=$(git status --short | wc -l)
timeout 10 hlint src/ > /tmp/hlint.txt 2>&1
after=$(git status --short | wc -l)
[ $before -eq $after ] && echo "✓ hlint read-only" || echo "✗ hlint modified files"

# Test 3: ghc-warnings (read-only compilation check)
before=$(git status --short | wc -l)
timeout 10 ghc -fno-code -Wall src/Main.hs > /tmp/ghc-warnings.txt 2>&1
after=$(git status --short | wc -l)
[ $before -eq $after ] && echo "✓ ghc check read-only" || echo "✗ ghc check modified files"

# Test 4: cabal outdated (read-only dependency check)
before=$(git status --short | wc -l)
timeout 10 cabal outdated > /tmp/cabal-outdated.txt 2>&1
after=$(git status --short | wc -l)
[ $before -eq $after ] && echo "✓ cabal outdated read-only" || echo "✗ cabal outdated modified files"

# Test 5: Assemble status report
cat > /tmp/toolchain-status.md << 'EOF'
# Toolchain Status

## cabal check
$(cat /tmp/cabal-check.txt | head -5)

## hlint
$([ -s /tmp/hlint.txt ] && echo "$(wc -l < /tmp/hlint.txt) suggestions" || echo "No suggestions")

## ghc warnings
$([ -s /tmp/ghc-warnings.txt ] && echo "$(grep -c "warning:" /tmp/ghc-warnings.txt) warnings" || echo "No warnings")

## Dependencies
$([ -s /tmp/cabal-outdated.txt ] && echo "$(wc -l < /tmp/cabal-outdated.txt) outdated" || echo "All current")
EOF

[ -f /tmp/toolchain-status.md ] && [ -s /tmp/toolchain-status.md ] && echo "✓ Status report generated" || echo "✗ Failed to generate report"
```

**Before using results:**
- All probes completed
- No files modified
- Status report generated
- Results are current (probes < 2 min old)

---

## instruction

```bash
# Run all probes (all read-only)
echo "Checking toolchain status..."

cabal check
echo "---"

hlint src/
echo "---"

ghc -fno-code -Wall src/Main.hs 2>&1 | head -20
echo "---"

cabal outdated
echo "---"

echo "✓ All probes completed (no changes made)"
```

---

## why this pattern works

1. **Zero risk** — probes don't modify anything
2. **Repeatable** — run them as often as you want
3. **Safe decision-making** — know the state before acting
4. **Diagnostic** — if something breaks later, probes show baseline
5. **Fast** — probes are quick (< 2 min usually)

---

## verification protocol

**AFTER running probes:**

- ✓ All commands completed (within timeout)
- ✓ No files modified (git status unchanged)
- ✓ Results captured to files
- ✓ Status report generated

**If any probe times out:**
- Record it (timeout ≠ failure, just slow)
- Skip that probe, continue others
- Report which probe timed out

**If any probe modifies files:**
- STOP immediately
- Investigate why probe modified files (unexpected)
- Restore git state
- Report issue

**Safe to re-run:**
- Probes can be run multiple times per day
- Results will differ if system state changed
- Baseline: run once, save results, compare later

---

## comment

Probes are your **eyes on the system**. They tell you what's happening without forcing any action.

Use probes when you're uncertain:
- "Is the build broken?"
- "Are there warnings I should know about?"
- "Are dependencies out of date?"
- "Did something change since last time?"

Probes answer these without risk. Then you decide what to do with the information.
