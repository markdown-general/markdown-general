# pattern: trace & hypothesis ⟜ diagnose by observation

**What it is:** Systematic execution tracing that teaches debugging while fixing. Observation → hypothesis → test → evidence.

**When to use:** Behavior is broken but root cause unclear. Need to understand the chain before proposing fix.

---

## Example: File Watcher Not Triggering Callbacks

**verification** ⟜ What does correct behavior look like?

**Correct behavior:**
1. File created in watched directory
2. Watcher detects file within 1 second
3. Event callback fires immediately
4. Callback reads file and triggers downstream action
5. Output appears in log/response

**How we'll know each step works:**
- Step 1: `ls -la /path/to/watch/ | tail -1` shows new file
- Step 2: `tail /path/to/watch.log | grep -c "detected"` increases
- Step 3: `tail /path/to/watch.log | grep "callback"` shows timestamp
- Step 4: `tail /path/to/response.log` shows data read
- Step 5: `tail /path/to/output.log` shows final result

**Failure points (what could go wrong):**
- File never created (filesystem issue)
- Watcher never detects (watch loop sleeping or blocked)
- Callback fires but doesn't execute (exception caught silently)
- Read succeeds but downstream fails (bad parse, bad send)
- Everything works in isolation but not together (threading issue)

---

## instruction

**Trace execution chain with logging at each phase:**

```bash
# Add logging before running:
# Phase 1: Does file appear?
echo "Test" > /tmp/ghci-in.txt
sleep 0.2
ls -la /tmp/ghci-in.txt && echo "✓ File created" || echo "✗ File missing"

# Phase 2: Does watcher detect it?
# (watcher logs to /tmp/watcher.log)
tail -5 /tmp/watcher.log | grep "detected" && echo "✓ Detected" || echo "✗ Not detected"

# Phase 3: Does callback fire?
tail -5 /tmp/watcher.log | grep "callback" && echo "✓ Callback fired" || echo "✗ No callback"

# Phase 4: Did it read the file?
tail -5 /tmp/watcher.log | grep "read" && echo "✓ Read successful" || echo "✗ Read failed"

# Phase 5: Did downstream get the data?
tail -5 /tmp/output.log | grep "processed" && echo "✓ Processed" || echo "✗ Not processed"
```

---

## hypothesis formation

**If Phase 2 fails:** Watcher loop is blocked (probably: forever threadDelay in main thread blocking watch detection)

**Proposed fix:** Run watcher and sleep loop in separate async threads

**Test the hypothesis:**
```haskell
-- Before: forever $ threadDelay 1000000 blocks watchDir
-- After:
watcher_thread <- async $ watchDir path callback
sleep_loop <- async $ forever $ threadDelay 1000000
waitBoth watcher_thread sleep_loop
```

**Verification of fix:**
```bash
# Re-run the trace
echo "Test2" > /tmp/ghci-in.txt
sleep 0.2
tail -5 /tmp/watcher.log | grep "detected" && echo "✓ FIX WORKS: Now detects" || echo "✗ FIX FAILED: Still no detect"
```

---

## verification protocol

**BEFORE proposing fix:**
- All 5 phases traced with logging
- Evidence shows exactly which phase fails
- Hypothesis grounded in observation, not speculation

**AFTER implementing fix:**
- Re-run trace at failure point
- Confirm that phase now succeeds
- Run full trace (all 5 phases) to ensure no regression elsewhere

**Safety:**
- If fix introduces new failure in different phase, stop and report
- Each phase is independent; fixing one shouldn't break another
- If it does, something deeper is wrong

**Success condition:**
All 5 phases show ✓. File watcher works end-to-end.

---

## comment

This pattern teaches **debugging methodology** while fixing. The card is:
- A teaching tool (here's how to trace execution)
- A diagnostic tool (find the broken phase)
- A fix specification (hypothesis grounded in evidence)
- A verification checklist (confirm each phase after fixing)

Reusable for any "behavior is broken" situation.
