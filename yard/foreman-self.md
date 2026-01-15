# foreman-self ⟜ operational handbook

**foreman-self** ⟜ practical companion to foreman.md; how to bootstrap, spin both skip-step and bounded cards, read synthesis, operate at altitude

---

## Session Startup ⟜ Bootstrap Your Foreman

### 1. Verify Permissions

Check ~/.claude/settings.local.json:

```json
{
  "permissions": {
    "allow": ["*"]
  }
}
```

You need: Bash, Read(*), Write(*), Glob(*), Grep(*), Task(*), Edit(*)

### 2. Create Directories

```bash
mkdir -p ~/self/foreman/cards
mkdir -p ~/self/foreman/responses
mkdir -p ~/self/foreman/logs
touch ~/self/foreman/foreman-workers.md
touch ~/self/foreman/foreman-notes.md
```

### 3. Initialize foreman-workers.md

Only for bounded workers. If using skip-step only, you can skip.

```markdown
# foreman-workers ⟜ timing and state

| id | card | spin-time | timeout-deadline | status | actual-completion | notes |
|----|------|-----------|------------------|--------|-------------------|-------|
```

### 4. Deploy Permanent Listeners

**For skip-step workflows (tailie):**

```bash
chmod +x ~/markdown-general/artifacts/bin/tailie
nohup ~/markdown-general/artifacts/bin/tailie \
  --watch ~/self/foreman/logs/ \
  --output ~/self/foreman/synthesis.md \
  > /tmp/tailie.log 2>&1 &
```

**For bounded workers (timer):**

```bash
chmod +x ~/markdown-general/artifacts/bin/timer
nohup ~/markdown-general/artifacts/bin/timer \
  --workers ~/self/foreman/foreman-workers.md \
  --responses ~/self/foreman/responses/ \
  --interval 2 \
  > /tmp/timer.log 2>&1 &
```

### 5. Verify State

```bash
ps aux | grep -E "(tailie|timer)" | grep -v grep
ls -la ~/self/foreman/
```

---

## Spinning Cards ⟜ Two Workflows

### Skip-Step Card (Fire-and-Forget)

**When to use:** Batch processing, parallelizable work, stream accumulation

**Step 1: Write card**

```markdown
# card: split-markdown-batch

**type** ⟜ skip-step / no tracking

**task** ⟜ find all .md in ~/repos, split into 4-20 line pieces, extract concept, append to flows-log.md

**output** ⟜ ✓ + piece count
```

**Step 2: Spin it**

No tracking needed. Just spin and forget.

```bash
# Spin via field agent or direct execution
# Worker appends results to flows-log.md
```

**Step 3: Breathe**

Tailie will accumulate results in background. Infrastructure handles everything.

```bash
# Don't monitor directly; read synthesis instead
cat ~/self/foreman/synthesis.md | tail -5
```

---

### Bounded Worker Card (Full Tracking)

**When to use:** Critical operations, exact timing needed, explicit decision points

**Step 1: Write card**

```markdown
# card: cabal-build-check

**type** ⟜ worker / bounded

**worker** ⟜ circuit / 60s

**input** ⟜ ~/repos/numhask-space

**task** ⟜ cabal clean && cabal build

**output** ⟜ ✓/✗ + [error count] [warning count]
```

**Step 2: Record in foreman-workers.md**

Add row:

```markdown
| 3 | cabal-build-check | 15:22:01 | 15:23:01 | active | - | spinning now |
```

**Step 3: Spin it**

Spin worker-3 with unique ID. Timer will enforce deadline.

**Step 4: Read callback**

When done, check responses/:

```bash
cat ~/self/foreman/responses/worker-3-result.md
```

**Step 5: Update row**

```markdown
| 3 | cabal-build-check | 15:22:01 | 15:23:01 | done | 15:22:45 | ✓ build ok, 2 warnings |
```

---

## Reading Synthesis ⟜ Prime at Altitude

**Never read raw logs.** Always read synthesis.

### Skip-Step Synthesis (Tailie Output)

```bash
tail -20 ~/self/foreman/synthesis.md
```

You'll see:

```
[20:04:22] +173 lines | coord(3) struct(2) exec(0) practice(2) other(5)
[20:05:10] +1276 lines | coord(3) struct(2) exec(0) practice(2) other(18)
[20:06:45] +892 lines | coord(3) struct(2) exec(0) practice(2) other(9)
```

**What to notice:**
- Line count increasing? (flow is accumulating)
- Pattern stable? (coord/struct/exec/practice ratios consistent)
- One domain dominant? (coherent phase)

### Pattern Signals (Bloodhound)

If running bloodhound:

```bash
tail -5 ~/self/foreman/bloodhound-report.md
```

You'll see:

```
[20:09:15] balanced: mixed domains flowing
[20:09:45] coord-dense | c=3 s=2 e=0 p=2
[20:10:15] coord-dense | c=3 s=2 e=0 p=2 (stable)
```

**What this means:**
- "balanced" ⟜ neutral state, ready for direction
- "coord-dense" ⟜ system focused on coordination
- "exec-heavy" ⟜ build/compile phase
- "practice-focus" ⟜ upgrade/refinement phase
- Stable pattern ⟜ continue or transition

### Decision Points

**Pattern is stable and accumulating:**
- Continue spinning similar cards
- Trust the flow
- Breathe between spins

**Pattern is changing (bloodhound detects):**
- Read synthesis to understand emerging phase
- Write next card targeting new phase
- Transition smoothly

**Flow is slow or stopped:**
- Don't fuss; could be normal
- Check one or two recent results
- If workers are actually blocked, investigate
- Otherwise, breathe and wait

---

## Timing Calibration ⟜ For Bounded Workers

### Track Observed Times

In foreman-workers.md, record actual completion time:

```markdown
| 1 | scan-time | 14:22:01 | 14:22:11 | done | 14:22:03 | ✓ 2s actual, 10s timeout = 5x buffer |
| 2 | build-v1 | 14:23:00 | 14:24:00 | done | 14:23:45 | ✓ 45s actual, 60s timeout = 1.3x buffer |
| 3 | lint-check | 14:24:00 | 14:24:30 | timeout | 14:24:30 | ✗ hit deadline, needs +20s next time |
```

### Adjust for Next Session

Rule of thumb:

```
next_timeout = observed_duration * 1.5 (normal work)
next_timeout = observed_duration * 2.0 (variable/network involved)
```

Examples:
- Scans (fast, predictable): 10s timeout fine for 2-5s work
- Builds (variable): 90s for work that took 45s
- Network: 45s for work that took 20s

### Keep Brief Notes

```markdown
**timing patterns observed:**
- scans: consistent, 2-5s, 10s timeout OK
- builds: variable 40-65s, use 90s timeout
- network: unpredictable, use 2x buffer
```

---

## Troubleshooting ⟜ When Things Get Weird

### Tailie Not Emitting Synthesis

Check:

```bash
ps aux | grep tailie | grep -v grep
tail -20 /tmp/tailie.log
ls -la ~/self/foreman/synthesis.md
```

Restart if needed:

```bash
pkill -f tailie
nohup ~/markdown-general/artifacts/bin/tailie ... &
```

### Timer Not Enforcing Deadlines

Check:

```bash
ps aux | grep timer | grep -v grep
tail -20 /tmp/timer.log

# Verify foreman-workers.md is readable
head -5 ~/self/foreman/foreman-workers.md
```

Restart if needed:

```bash
pkill -f timer
nohup ~/markdown-general/artifacts/bin/timer ... &
```

### Worker Seems Hung

For bounded workers, check responses/:

```bash
ls -la ~/self/foreman/responses/ | grep worker-{id}
```

If no response file exists:
- Timer hasn't killed it yet (wait for deadline)
- Or worker crashed (check for logs)

For skip-step workers:
- Check flows-log.md for recent additions
- If stuck, kill via process management

### Synthesis Not Changing

**For skip-step:**
- Tailie is polling flows-log.md
- If no new lines added to flows-log, synthesis won't change
- Check: are workers actually running?

**For bounded:**
- Check foreman-workers.md status column
- Is work marked as "done" or still "active"?

### Context Ball Overflowing

Signs: synthesis repeating same pattern, no new information flowing, system feels stuck.

Reset via supervisor (if available):

1. Verify actual state: what files/repos actually exist?
2. Write fresh foreman-notes.md from current reality
3. Restart with clean synthesis file
4. Resume work

---

## Patterns & Recipes ⟜ Useful Shapes

### Batch Skip-Step Pattern

Run many skip-step cards in parallel, accumulate results:

```markdown
# card: split-batch-001
**type** ⟜ skip-step
**task** ⟜ split files 1-100

# card: split-batch-002
**type** ⟜ skip-step
**task** ⟜ split files 101-200

# (repeat as needed)
```

All append to flows-log.md in parallel. No contention. Tailie synthesizes all.

### Sequential Bounded Pattern

For dependent tasks, use bounded workers + foreman-notes:

```markdown
**spinning now:**
- worker-1: build project-A
  - if ✓: next spin worker-2 (test project-A)
  - if ✗: skip to next project
```

Use notes as decision log, not as formal state.

### Transition Pattern

When bloodhound detects pattern shift:

1. Note the transition in foreman-notes.md
2. Write card for new phase
3. Spin new batch of cards
4. Old phase results remain in flows-log (historical)
5. Synthesis will show new domain emphasis

---

## Session Shutdown ⟜ Clean Closure

### 1. Stop Spinning New Cards

No new cards. Let active ones finish.

### 2. Wait for Active Workers

```bash
# For bounded workers, check status
grep "active" ~/self/foreman/foreman-workers.md
```

### 3. Stop Permanent Agents

```bash
pkill -f tailie
pkill -f timer

# Verify they stopped
ps aux | grep -E "(tailie|timer)" | grep -v grep
```

### 4. Archive Session

```bash
mkdir -p ~/self/foreman/archive/$(date +%Y%m%d)
cp ~/self/foreman/foreman-workers.md ~/self/foreman/archive/$(date +%Y%m%d)/
cp ~/self/foreman/foreman-notes.md ~/self/foreman/archive/$(date +%Y%m%d)/
cp ~/self/foreman/synthesis.md ~/self/foreman/archive/$(date +%Y%m%d)/
```

### 5. Write Handoff Notes

```markdown
# session summary ⟜ $(date)

**what worked:**
- skip-step pattern for batch work
- tailie synthesis clear and stable
- bounded workers for critical builds

**surprises:**
- builds took longer than expected
- tailie tuning needed after first hour

**recommendations:**
- increase cabal timeout to 90s
- batch small scans into one worker
- run tailie with 5s polling for this workload

**next session:**
- resume with synthesis from today
- flows-log.md is append-only, can continue
- reload same listener patterns
```

---

## Quick Reference ⟜ Commands

```bash
# Bootstrap
mkdir -p ~/self/foreman/{cards,responses,logs}
touch ~/self/foreman/{foreman-workers.md,foreman-notes.md}

# Start tailie (skip-step)
nohup ~/markdown-general/artifacts/bin/tailie \
  --watch ~/self/foreman/logs/ \
  --output ~/self/foreman/synthesis.md > /tmp/tailie.log 2>&1 &

# Start timer (bounded)
nohup ~/markdown-general/artifacts/bin/timer \
  --workers ~/self/foreman/foreman-workers.md \
  --responses ~/self/foreman/responses/ \
  --interval 2 > /tmp/timer.log 2>&1 &

# Check synthesis
tail -20 ~/self/foreman/synthesis.md

# Stop agents
pkill -f tailie
pkill -f timer

# View working notes
cat ~/self/foreman/foreman-notes.md
```

---

## References

- **foreman.md** ⟜ philosophy and unified design
- **pattern.md** ⟜ shape and encoding
- **breathe.md** ⟜ yin practice
- **worker.md** ⟜ field worker palette
- **listener.md** ⟜ ~/markdown-general/cards/tools/listener.md
- **timer.md** ⟜ ~/markdown-general/cards/tools/timer.md
