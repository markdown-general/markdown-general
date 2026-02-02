# openclaw ⟜ yin ⟪ how to integrate ⟫

⊢ compare architectures ⊣ ◊ ⬡

Claude running inside OpenClaw, and the yin framework for coordination. Can they work together? Yes. How? Here's the map.

---

## What you have

**yin** = a coordination system where:
- Cards describe strategy (intent + flow topology)
- Runners execute cards and stay in conversation
- Flow markers (✓ ◊ 🚩) track actual state vs. planned state
- Field agents spin out for heavy lifting
- Breathe/sync pauses let strategy and reality align

**OpenClaw** = an infrastructure layer where:
- Claude runs as an embedded agent
- Files live in a persistent workspace
- Messages route to channels automatically
- Sub-agents can be spawned for isolated work
- Skills extend capability

---

## The alignment

They fit together naturally. Here's why:

### Cards ⟜ Workspace files

yin uses markdown cards as strategy documents. OpenClaw has a workspace where I can read/write markdown freely.

```
~/mg/work/          ⟜ Strategy cards (read + edit)
~/mg/yard/          ⟜ Transient work (drafts, experiments)
~/mg/log/           ⟜ Execution logs (I write here)
~/.openclaw/        ⟜ (separate) Config, credentials, sessions
```

I can load a card, parse it, and operate within its flow.

### Spinning field agents ⟜ Sub-agents

When a card says "spin a field agent," I have two options in OpenClaw:

1. **Direct execution** — I read files, run commands, parse output (all in my turn)
2. **Sub-agent spawn** — `sessions_spawn()` creates isolated run (async, reports back)

For CPU-heavy work or long-running tasks, spawning is better. For coordinated file I/O, staying in-turn is simpler.

### Staying in conversation ⟜ Message loop

yin's runner is you (Tony). OpenClaw's message system routes replies to you automatically.

- You send a card or ask a question
- I read it, execute, move `◊` forward
- I report back to you here (this channel)
- You signal next move (breathe, continue, redirect)

The queue mode `steer` lets me be interrupted mid-turn if you need to adjust.

### Memory & persistence ⟜ Workspace + MEMORY.md

yin expects cards to be living records. OpenClaw:
- Persists workspace across sessions (files don't vanish)
- Has MEMORY.md for long-term context (loaded each session)
- Has memory/YYYY-MM-DD.md for daily logs
- Has AGENTS.md for operating instructions

I can read all of these on session start and stay coherent.

---

## How message routing works

Important: **I don't choose where my replies go. OpenClaw routes them back to you.**

- WebChat (here) → replies come back here
- Telegram (if configured) → replies go to Telegram
- Discord (if configured) → replies go to Discord

This means the same agent (me) can work across multiple channels, but my replies always come from the same source.

For cards and logging, I write to files in the workspace. For direct communication with you (the runner), I use the message tool with the `channel` parameter.

---

## Resolving the queries from async.md

### Filesystem & file io ⟜ ✓ Full access

I can read/write anywhere in the workspace. No restrictions on structure or file format.

**Limits:** Relative paths resolve in workspace; absolute paths can reach outside unless sandboxing is enabled. For your setup, sandboxing is off, so I have host-level access.

**How yin uses this:** Cards live in work/, logs in log/, cache/state in tools/. I read strategy, write execution, update flow markers.

### Shell command flexibility ⟜ ✓ Full exec

I can run any shell command via the `exec` tool. No restrictions on what I execute.

**How yin uses this:** Bash one-liners, tool invocation, git operations, anything a runner would do from the terminal.

### Multi-agent usage and dexterity ⟜ ✓ Spawn agents

I can spawn sub-agents via `sessions_spawn()`. They get isolated runs and report back.

**How yin uses this:** Heavy lifting (parsing large files, parallel tasks, long-running operations) goes to field agents. Coordination and flow management stays with me.

### Multi-agent recursion limits ⟜ [uncertain]

Config allows max 8 concurrent sub-agents. No mention of nesting depth limits.

**Theory:** A spawned agent could spawn another, creating chains. But this is untested and costs tokens.

**Practical recommendation:** Keep it shallow (1-2 levels deep). If you need deep recursion, redesign the card to be breadth-first instead.

### How yin stays in conversation ⟜ ✓ Natural

I receive messages here, execute, and reply here. Queue mode can be `steer` (mid-turn injection) or `followup` (wait for turn end).

**How to stay present:** Move the `◊` marker as I go, edit the card in place, report incremental findings. The conversation itself becomes part of the flow record.

### Persistence across sessions ⟜ ✓ Workspace survives

MEMORY.md, workspace files, logs, all persist. Each session I wake up with workspace context intact.

**How yin uses this:** A long card spanning multiple sessions? No problem. I read where I left off, check the ◊ marker, and continue.

### Skill methods ⟜ ✓ Standard OpenClaw skills

52 bundled skills available (github, weather, coding-agent, sonoscli, etc.). I can also write custom skills in workspace/skills/.

**How yin uses this:** Skills become reusable patterns. A skill that parses flow notation, or a tool that logs card execution, can live here and be invoked from cards.

---

## Implementation: How I'd operate as yin

1. **Read the card** in ~/mg/work/ or ~/mg/yard/
2. **Parse queries** [bracketed] sections
3. **Move ◊** to the current action
4. **Execute** (direct or spawn agents as needed)
5. **Log** output to ~/mg/log/[card-id]-[timestamp].md
6. **Update the card** in place:
   - Resolve queries: `[name]` → `[name]✓`
   - Mark failures: add `🚩 action failed because X`
   - Move ◊ forward
7. **Report to you** here (summary of what happened, what's next)
8. **Wait for breathe signal** — you decide next card or redirect

---

## Gaps to address

### Flow encoding as a first-class tool

yin's flow notation is elegant prose + symbols. OpenClaw doesn't have built-in parsing for it.

**Option A:** Implement a simple flow parser in a skill
- Read card, extract [bracketed] queries and flow markers
- Output a JSON state file (tools/flow-state.json)
- This becomes the "position tracker" between sessions

**Option B:** Keep it manual (less code, more flexibility)
- I update the card directly
- Flow notation is guidance, not enforced
- Simpler to start, easier to adapt

**Recommendation:** Start with Option B. Implement Option A as a skill if it becomes painful.

### Cache mechanism

yin docs mention work/cache.md for handoff. OpenClaw has no explicit cache layer.

**I'd use:** tools/cache.md in the workspace, or a simple JSON file in tools/.

This becomes the "context snapshot" I pass to field agents or between sessions.

### Pattern capture and reuse

yin expects successful cards to become recipes/patterns in tools/.

**How:** When a card succeeds, extract the essence:
1. What was the intent?
2. What actually worked?
3. Generalize the approach
4. Write as tools/[pattern-name].md

Then future cards can reference it: `[pattern-name](tools/pattern-name.md)`

---

## Cost implications

Running as yin in OpenClaw:

- **Session cost**: Haiku is cheap, but workspace context loads every session (~10-20 lines per file)
- **Sub-agent spawning**: Each spawn costs a full model turn (use sparingly)
- **Logging**: Writing logs costs nothing (file I/O, not API)
- **Flow marker updates**: Cheap (read + edit a single file)

**To stay lean:**
1. Keep MEMORY.md and AGENTS.md under ~500 lines total
2. Don't spawn sub-agents unless the work truly demands isolation
3. Use logging heavily (it's free) instead of re-reading files
4. Batch queries and resolutions (fewer iterations = fewer turns)

---

## What happens next?

To integrate fully:

1. **Design a first card** in ~/mg/yin/ (like async.md, but for a real task)
2. **I execute it** following yin rhythm: read → execute → move ◊ → report → breathe
3. **We iterate** and refine the flow
4. **Capture patterns** as reusable tools
5. **Build skills** if a pattern repeats often

This report is the map. The next move is a working card.

---

## Status

⊢ comparison complete ⊣ ◊ ⬡
  ⟜ [questions for Tony to refine implementation](async.md#questions)
  ⋆ [proceed with first working card]
  ◉ ready for breathe and sync

**Logged:** ~/mg/log/async-report-2026-02-01.md

Questions for you (the runner):

1. **Flow state tracking:** Option A (skill + JSON state) or Option B (manual markers)?
2. **Cache mechanism:** Use tools/cache.md, or something else?
3. **First card:** What should we tackle first to test this system?
