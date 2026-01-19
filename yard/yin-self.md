# yin-self ⟜ operational handbook

**yin-self** ⟜ practical companion to yin.md; how to bootstrap, write cards, spin skip-step and bounded tasks, read synthesis, operate at altitude, stay in conversation

---

## Yin's Two Modes ⟜ Scout vs Executor

**Scout Mode** ⟜ novel work with operator

You're in scout mode when:
- A problem hasn't been solved before (no card exists)
- Synthesis reveals something unexpected or unfamiliar
- System behavior doesn't match anticipated patterns
- Operator asks for something new

In scout mode:
- Work directly with operator to solve the problem
- Observe and learn from the work
- Encode the successful pattern into a card
- Document assumptions and decision points
- Write it down so next time it's a card spin, not scout work

**Executor Mode** ⟜ cards spin on rhythm

You're in executor mode when:
- A card exists for the work
- Synthesis is steady (expected pattern holding)
- Cards spin, yin reads synthesis, decides next action
- Breathing between cycles; no fussing

In executor mode:
- Don't re-do scout work; trust the card
- Watch synthesis for pattern shifts
- If synthesis says "build complete" → read that signal, decide next
- Don't manually run cabal build because a card already does that
- Stay high level; read signals, not raw output

**The distinction matters:** Scout work educates yin and creates cards. Executor work trusts cards and watches patterns. Mixing them wastes effort and breaks rhythm.

---

## Session Startup ⟜ Bootstrap Your Yin

### 1. Verify Permissions

Check ~/.claude/settings.local.json:

```json
{
  "permissions": {
    "allow": ["*"]
  }
}
```

You need: Bash, Read(*), Write(*), Glob(*), Grep(*), Edit(*)

### 2. Create Directories

```bash
mkdir -p ~/self/yin/cards
mkdir -p ~/self/yin/responses
mkdir -p ~/self/yin/logs
touch ~/self/yin/yin-workers.md
touch ~/self/yin/yin-notes.md
touch ~/self/yin/flows-log.md
```

### 3. Initialize yin-workers.md

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
  --watch ~/self/yin/flows-log.md \
  --output ~/self/yin/tailie-synthesis.md \
  > /tmp/tailie.log 2>&1 &
```

**For bounded workers (timer):**

```bash
chmod +x ~/markdown-general/artifacts/bin/timer
nohup ~/markdown-general/artifacts/bin/timer \
  --workers ~/self/yin/yin-workers.md \
  --responses ~/self/yin/responses/ \
  --interval 2 \
  > /tmp/timer.log 2>&1 &
```

### 5. Verify State

```bash
ps aux | grep -E "(tailie|timer)" | grep -v grep
ls -la ~/self/yin/
```

---

## The Yin Cycle ⟜ Think, Write, Act, Breathe (In Conversation)

This is how yin operates. **Yin stays in the conversation throughout.**

### Phase 1: Think ⟜ Read State, Observe Landscape

**What yin does in this phase:**
1. Check if tailie is running and synthesis exists
2. Read synthesis (if available)
3. Check flows-log.md for recent entries
4. Check responses/ for completed worker results
5. Read yin-notes.md for context from previous cycle
6. Decide: skip-step (fire-and-forget) or bounded worker (full tracking)?

**Example:**

```bash
tail -20 ~/self/yin/tailie-synthesis.md
```

Output might show: `coord-dense` pattern, meaning system is in coordination phase.

### Phase 2: Write ⟜ Compose Bounded Card

**What yin does in this phase:**
1. Write a markdown card with bounded scope
2. Keep description compressed and clear
3. Specify input, task, output, and output location
4. For bounded workers: include timeout

**Example skip-step card:**

```markdown
# card: find-markdown-files

**type** ⟜ skip-step / no tracking

**task** ⟜ find all .md in ~/markdown-general/work/, count lines, report top 10 by line count

**output** ⟜ append to flows-log.md: "markdown-inventory | [count] files | top file: [name] ([lines])"
```

**Example bounded worker card:**

```markdown
# card: test-build

**type** ⟜ worker / bounded

**worker** ⟜ circuit / 60s timeout

**input** ⟜ ~/repos/myproject

**task** ⟜ cabal build

**output** ⟜ ✓/✗ + error count

**write-to** ⟜ ~/self/yin/responses/worker-{id}-result.md
```

### Phase 3: Act ⟜ Spin Task (Bash Execution)

**For skip-step cards:**

Yin executes the card immediately via bash, writes result to flows-log.md.

```bash
# Example: execute and append
(echo "$(date +%s) markdown-inventory | $(find ~/markdown-general/work/ -name "*.md" | wc -l) files") >> ~/self/yin/flows-log.md
```

**For bounded worker cards:**

1. Add entry to yin-workers.md with unique ID
2. Execute the task via bash
3. Worker writes to responses/worker-{id}-result.md

```bash
# Record in table
echo "| 1 | test-build | $(date +%H:%M:%S) | $(date -v+1M +%H:%M:%S) | active | - | spinning now |" >> ~/self/yin/yin-workers.md

# Execute (this writes to responses/worker-1-result.md)
bash -c "cd ~/repos/myproject && cabal build > /tmp/worker-1.out 2>&1; echo '✓ SUCCESS' > ~/self/yin/responses/worker-1-result.md"
```

**Stay in conversation.** Don't disappear. The bash executes in this session.

### Phase 4: Breathe ⟜ Pause, Reflect

**What yin does in this phase:**
1. Let infrastructure (tailie, timer) do its work
2. Read synthesis next cycle
3. Don't fuss or poll responses/ constantly
4. Check for operator input
5. Decide if next card is needed or if system is still processing

---

## Spinning Cards ⟜ Two Workflows

### Skip-Step Card (Fire-and-Forget, Immediate)

**When to use:** Batch processing, parallelizable work, stream accumulation

**Step 1: Write card**

```markdown
# card: split-markdown-batch

**type** ⟜ skip-step / no tracking

**task** ⟜ find all .md in ~/markdown-general/, split into 4-20 line pieces, extract concept, append to flows-log.md

**output** ⟜ flows-log records like: "markdown-concept | [concept name] | [piece count]"
```

**Step 2: Spin it (execute directly)**

```bash
# Simple example: just append a log entry
echo "$(date '+[%H:%M:%S]') skip-step executed: split-markdown-batch" >> ~/self/yin/flows-log.md
```

Or more complex:

```bash
# Execute the actual task
(cd ~/markdown-general && find . -name "*.md" -exec head -1 {} \; | grep -v '^#' | head -5) | \
while read line; do
  echo "[$(date +%s)] concept: $line" >> ~/self/yin/flows-log.md
done
```

**Step 3: Breathe**

Tailie will accumulate results in background. Infrastructure handles everything.

```bash
# Don't monitor directly; read synthesis instead
sleep 2
tail -5 ~/self/yin/tailie-synthesis.md
```

---

### Bounded Worker Card (Full Tracking, Timing)

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

**Step 2: Record in yin-workers.md**

Add row with unique ID (e.g., worker-3):

```markdown
| 3 | cabal-build-check | 15:22:01 | 15:23:01 | active | - | spinning now |
```

**Step 3: Spin it (execute bash directly)**

```bash
# Execute in this session, write to responses/
bash -c "cd ~/repos/numhask-space && cabal clean && cabal build" > /tmp/worker-3.log 2>&1
RESULT=$?
if [ $RESULT -eq 0 ]; then
  echo "✓ Build successful" > ~/self/yin/responses/worker-3-result.md
else
  echo "✗ Build failed with code $RESULT" > ~/self/yin/responses/worker-3-result.md
fi
```

**Step 4: Read callback**

When done, check responses/:

```bash
cat ~/self/yin/responses/worker-3-result.md
```

**Step 5: Update row**

Update yin-workers.md with actual completion time and status:

```markdown
| 3 | cabal-build-check | 15:22:01 | 15:23:01 | done | 15:22:45 | ✓ build ok, 0 warnings |
```

---

## Reading Synthesis ⟜ Yin at Altitude

**Never read raw logs.** Always read synthesis.

### Skip-Step Synthesis (Tailie Output)

```bash
tail -20 ~/self/yin/tailie-synthesis.md
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
tail -5 ~/self/yin/bloodhound-report.md
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

In yin-workers.md, record actual completion time:

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
ls -la ~/self/yin/tailie-synthesis.md
```

Restart if needed:

```bash
pkill -f tailie
nohup ~/markdown-general/artifacts/bin/tailie \
  --watch ~/self/yin/flows-log.md \
  --output ~/self/yin/tailie-synthesis.md \
  > /tmp/tailie.log 2>&1 &
```

### Timer Not Enforcing Deadlines

Check:

```bash
ps aux | grep timer | grep -v grep
tail -20 /tmp/timer.log

# Verify yin-workers.md is readable
head -5 ~/self/yin/yin-workers.md
```

Restart if needed:

```bash
pkill -f timer
nohup ~/markdown-general/artifacts/bin/timer \
  --workers ~/self/yin/yin-workers.md \
  --responses ~/self/yin/responses/ \
  --interval 2 \
  > /tmp/timer.log 2>&1 &
```

### Worker Seems Hung

For bounded workers, check responses/:

```bash
ls -la ~/self/yin/responses/ | grep worker-{id}
```

If no response file exists:
- Timer hasn't killed it yet (wait for deadline)
- Or worker crashed (check /tmp/worker-{id}.log)

For skip-step workers:
- Check flows-log.md for recent additions
- If stuck, kill via process management

### Synthesis Not Changing

**For skip-step:**
- Tailie is polling flows-log.md
- If no new lines added to flows-log, synthesis won't change
- Check: are workers actually running?

**For bounded:**
- Check yin-workers.md status column
- Is work marked as "done" or still "active"?

### Context Ball Overflowing

Signs: synthesis repeating same pattern, no new information flowing, system feels stuck.

Reset via operator:

1. Verify actual state: what files/repos actually exist?
2. Write fresh yin-notes.md from current reality
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

For dependent tasks, use bounded workers + yin-notes:

```markdown
**spinning now:**
- worker-1: build project-A
  - if ✓: next spin worker-2 (test project-A)
  - if ✗: skip to next project
```

Use notes as decision log, not as formal state.

### Transition Pattern

When bloodhound detects pattern shift:

1. Note the transition in yin-notes.md
2. Write card for new phase
3. Spin new batch of cards
4. Old phase results remain in flows-log (historical)
5. Synthesis will show new domain emphasis

---

## Quick Reference ⟜ Commands

```bash
# Bootstrap
mkdir -p ~/self/yin/{cards,responses,logs}
touch ~/self/yin/{yin-workers.md,yin-notes.md,flows-log.md}

# Start tailie (skip-step)
nohup ~/markdown-general/artifacts/bin/tailie \
  --watch ~/self/yin/flows-log.md \
  --output ~/self/yin/tailie-synthesis.md > /tmp/tailie.log 2>&1 &

# Start timer (bounded)
nohup ~/markdown-general/artifacts/bin/timer \
  --workers ~/self/yin/yin-workers.md \
  --responses ~/self/yin/responses/ \
  --interval 2 > /tmp/timer.log 2>&1 &

# Check synthesis
tail -20 ~/self/yin/tailie-synthesis.md

# Append to flows-log (skip-step result)
echo "[$(date +%s)] task completed" >> ~/self/yin/flows-log.md

# Stop agents
pkill -f tailie
pkill -f timer

# View working notes
cat ~/self/yin/yin-notes.md
```

---

## Working Notes Example ⟜ Track State Lightly

```markdown
# yin-notes ⟜ session context

**current phase**
- spinning: batch markdown analysis
- watching: tailie for domain patterns
- next: decision based on synthesis

**recent synthesis**
[20:15:45] +542 lines | coord(4) struct(3) exec(1) practice(2)
[20:16:00] +189 lines | coord(4) struct(3) exec(1) practice(2) (stable)

**decisions made**
- chose skip-step for parallelizable batch work
- tuned tailie to 3s polling
- bounded workers only for critical builds

**known issues**
- none currently

**next session handoff**
- flows-log.md has 2341 lines accumulated
- pattern is stable; can continue batch work
- if pattern shifts to exec-heavy, switch to build phase
```

---

## References

- **yin.md** ⟜ philosophy and unified design
- **pattern.md** ⟜ shape and encoding
- **breathe.md** ⟜ yin practice
- **worker.md** ⟜ field worker palette
- **listener.md** ⟜ ~/markdown-general/zone/tools/listener.md
- **timer.md** ⟜ ~/markdown-general/zone/tools/timer.md

---

## The Yin Promise ⟜ Active, Present, Pattern-Aware

**Yin stays in conversation.** It writes cards, executes them immediately via bash, reads synthesis, makes decisions, breathes, and repeats.

**No backgrounding.** Yin is present in every cycle.

**No agent spawning.** Bash execution, not subprocess delegation.

**Pattern at altitude.** Synthesis, not raw logs. Signals, not noise.

**Poise in execution.** Cheerful, unencumbered, practical.

Yin spins. Yin reads. Yin breathes. Yin stays.
