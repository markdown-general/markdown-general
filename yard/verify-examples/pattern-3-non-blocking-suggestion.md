# pattern: non-blocking suggestion ⟜ report without mandating fix

**What it is:** Surface issues without blocking progress. Signal-only, no enforcement.

**When to use:** Style, warnings, suggestions where the repo still works but could be better.

---

## Example: HLint Style Suggestions

**verification** ⟜ What does success look like?

**Success criteria:**
- hlint runs to completion (no crashes)
- hlint outputs suggestions (if any exist)
- Exit code 0 (success, not failure—hlint is advisory)
- Report is generated (regardless of content)
- Repo state unchanged (hlint reads only, no modifications)

**How we'll verify:**
- Test 1: hlint runs without hanging
  ```bash
  timeout 30 hlint src/ > /tmp/hlint-report.md 2>&1
  [ $? -eq 0 ] || [ $? -eq 124 ] && echo "✓ Ran" || echo "✗ Crashed"
  ```

- Test 2: Report contains content (suggestions or "No suggestions")
  ```bash
  [ -s /tmp/hlint-report.md ] && echo "✓ Report generated" || echo "✗ Empty report"
  ```

- Test 3: Exit code indicates advisory status (0 = ok)
  ```bash
  hlint src/ > /dev/null 2>&1
  hlint_code=$?
  [ $hlint_code -eq 0 ] && echo "✓ Advisory (non-blocking)" || echo "! Warning: exit code $hlint_code"
  ```

- Test 4: No files modified
  ```bash
  git status --short | wc -l > /tmp/before.txt
  hlint src/
  git status --short | wc -l > /tmp/after.txt
  diff /tmp/before.txt /tmp/after.txt > /dev/null && echo "✓ Read-only" || echo "✗ Modified files"
  ```

**Failure modes (what's NOT ok):**
- hlint crashes → STOP, report crash
- hlint modifies files (should be read-only) → STOP, investigate
- hlint hangs (takes > 30 sec) → timeout, report as performance issue

---

## instruction

```bash
# Run hlint, capture suggestions (non-blocking)
hlint src/ --hint=Default > /tmp/hlint-report.md 2>&1

# Report results: show suggestions, don't mandate fixes
echo "=== HLint Suggestions (Non-Blocking) ==="
cat /tmp/hlint-report.md
echo ""
echo "Apply these suggestions if desired. Repo is usable either way."
```

---

## verification protocol

**AFTER running hlint:**

- ✓ hlint completed without crashing
- ✓ Report generated (suggestions or empty)
- ✓ No files were modified
- ✓ Repo status unchanged (git status clean)

**If all pass:**
- Report the suggestions to operator
- Mark as "informational, non-blocking"
- Proceed to next task

**If hlint crashes:**
- STOP
- Report crash and error message
- Do not treat hlint failures as blocking repo issues

**Operator choice:**
- Review suggestions at leisure
- Apply fixes as desired (separate task)
- Ignore suggestions (repo still works)

**Safety:**
- hlint is advisory only
- No fix is mandatory
- Repo health NOT dependent on hlint passing

---

## comment

This pattern separates **signal from action**:
- Signal: "Here's what hlint found"
- Action: "Decide if/when to fix it"

Works for:
- Style warnings (hlint, ormolu --check)
- Compiler warnings (ghc -Wall)
- Dependency outdates (cabal outdated)
- Any "could be better" feedback

Key: **report doesn't mandate**. Operator maintains agency.
