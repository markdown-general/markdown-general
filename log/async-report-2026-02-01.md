# async report ⟜ 2026-02-01

**Card:** yin/async.md  
**Started:** 2026-02-01 18:05 AEDT  
**Status:** ◊ reading and mapping

## Phase 1: Reading OpenClaw Docs

### Files read:
- `/opt/homebrew/lib/node_modules/openclaw/docs/concepts/agent.md`
- `/opt/homebrew/lib/node_modules/openclaw/docs/concepts/multi-agent.md`
- `/opt/homebrew/lib/node_modules/openclaw/docs/concepts/agent-workspace.md`

### Initial findings (raw, will synthesize later):

**Filesystem & File IO:**
- Default cwd = workspace (`~/.openclaw/workspace` or override)
- Tools: read/write/edit available
- Absolute paths can escape workspace UNLESS sandboxed
- Workspace is treated as "home" + memory (like mg's work/)
- Bootstrap files injected on session start: AGENTS.md, SOUL.md, USER.md, etc.
- Can have per-agent workspaces with multi-agent routing

**Shell command flexibility:**
- `exec` tool runs shell commands
- Optional sandboxing via `agents.defaults.sandbox`
- Per-agent sandbox config possible (separate per-agent docker containers)
- Outputs can be captured/logged

**Multi-agent usage:**
- `sessions_spawn()` spawns isolated sub-agent runs
- Multiple agents = separate workspaces + auth profiles + session stores
- Routing via `bindings` (channel/accountId/peer match)
- Agent-to-agent messaging can be enabled (must be explicit + allowlisted)
- Each agent has own `agentDir` at `~/.openclaw/agents/<agentId>/agent`

**Recursion/nesting:**
- Sessions can spawn sub-agents
- Sub-agent sessions stored separately
- No explicit recursion limit mentioned yet — need to test

**Conversation persistence:**
- Sessions stored as JSONL at `~/.openclaw/agents/<agentId>/sessions/<SessionId>.jsonl`
- Queue mode can be `steer` (inject messages mid-turn) or `followup`/`collect` (hold until turn ends)
- Block streaming available (chunks replies)
- Agent context injected at session start (including workspace files)

**Session state & memory:**
- `AGENTS.md` = operating instructions + memory guidance
- `MEMORY.md` optional = curated long-term (only load in main session, not shared)
- `memory/YYYY-MM-DD.md` = daily logs
- `cache.md` mentioned in mg docs (not yet found in OpenClaw)
- Bootstrap file trimming at ~20k chars default

**Skills and extensibility:**
- Skills loaded from 3 locations: bundled, managed (`~/.openclaw/skills`), workspace (`workspace/skills`)
- Per-agent skills possible (workspace overrides bundled)
- Gated by config/env
- Can write custom skills (format TBD, need to read SKILL.md examples)

## Phase 2: Channel Routing & Config Analysis

### Files read:
- `/opt/homebrew/lib/node_modules/openclaw/docs/concepts/channel-routing.md`
- `~/.openclaw/openclaw.json` (my runtime config)

### Key findings:

**Message routing (how I stay in conversation):**
- Replies automatically route to source channel (deterministic, not my choice)
- Session keys shape: `agent:<agentId>:<channel>:peer:<peerId>` for groups/channels
- Direct messages collapse to agent main: `agent:main:main`
- Can configure bindings to route inbound to different agents
- Queue mode: `steer` injects mid-turn, `followup`/`collect` holds until turn ends
- WebChat provides cross-channel view for one agent

**Capability limits (from config):**
- Max 4 concurrent agents
- Max 8 concurrent sub-agents
- Primary model: Haiku
- Workspace: `/Users/tonyday567/.openclaw/workspace`
- 52 bundled skills available (bird, github, weather, coding-agent, etc.)

**Message tool insights:**
- `message action=send` with `channel` parameter for targeted sends
- `message action=broadcast` for multi-agent output
- Supports effect, emoji, reactions on supported platforms
- Silent reply mode: NO_REPLY

## Phase 3: Mapping to Yin Concepts

Now I'll synthesize how OpenClaw maps to yin's model of work:

### yin ⟜ OpenClaw Bridge

| yin concept | OpenClaw equivalent | Notes |
|---|---|---|
| **Flow markers** `◊` ✓ 🚩 ⟜ | Session context + workspace files | No built-in flow encoding; I'd implement in cards/logs |
| **Card strategy** | work/ markdown files in workspace | I can read/write cards freely |
| **Field agent spinning** | `sessions_spawn()` for sub-agents | Max 8 concurrent sub-agents |
| **Logging output** | Files in workspace (log/, yard/, etc.) | Full read/write to workspace |
| **Staying in conversation** | Message queue + WebChat main session | Queue mode `steer` for mid-turn injection |
| **Persistent memory** | AGENTS.md + MEMORY.md + memory/YYYY-MM-DD.md | Workspace files persist across sessions |
| **Moving ◊ marker** | Manual edits to card file | I'd update flow markers in the card as I go |
| **Breathing/sync** | Sessions with user as runner | Natural via message exchange |
| **Pattern capture** | tools/ recipes in workspace | Can read/write executable cards |
| **Resolving queries** | [bracket] notation in cards | I'd update brackets to [name]✓ as resolved |

### Gaps / Uncertainties:

1. **Flow encoding performance** — How to track ◊ position efficiently without excess I/O?
   - Option A: edit card file in place (slow, one edit per move)
   - Option B: use separate flow-state.json (mirrors yin logs/)
   - Option C: track in message context (volatile, lost between sessions)
   - [uncertain](async.md#flow-encoding)

2. **Sub-agent recursion depth** — Can a spawned agent spawn another? No hard limit found.
   - Testing needed (but costs tokens)
   - [recursion-depth-untested](async.md#recursion)

3. **Skill format for yin operations** — How to package yin helpers (flow utils, card parsers)?
   - Can write shell scripts in tools/
   - Can write JS skills in skills/
   - [skill-format-unclear](async.md#skills)

4. **Cache.md handoff** — mg uses work/cache.md; is there an OpenClaw equivalent?
   - No explicit cache layer in OpenClaw
   - Could use tools/cache.md or tools.json
   - [cache-mechanism](async.md#cache)

### What works cleanly:

✓ **File I/O** — Full read/write to workspace  
✓ **Shell execution** — exec tool works  
✓ **Multi-agent routing** — Sessions can spawn agents  
✓ **Persistence** — Workspace + MEMORY.md survive sessions  
✓ **Message channels** — Message tool for multi-channel sends  
✓ **Skills** — Can load custom skills from workspace/skills/  

---

## Next: Build comparison table + write summary

_Phase 3 synthesis complete. Moving to deliverable._
