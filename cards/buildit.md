# card: buildit ⟜ permanent filewatch with live feedback loop

**type** ⟜ flow / permanent

**purpose** ⟜ watch project directory; spin builds on file change; use ghcid for fast feedback

**effects** ⟜ reads: src/*.hs + cabal file, writes: ~/repos/[repo]/dist-newstyle/ + ghcid.txt, spawns: ghcid process, network: no

---

## Activation

Buildit starts when:
- Lockit has completed
- Operator decides to enter active development (documented in yin-notes)

Usually spun once per project; remains active through all subsequent work.

---

## Core Loop

### Phase 1: Initial Build

Spin: action-cabal-build

- Full cabal build all; capture all output
- Capture to `/tmp/cabal-build-[repo].log`
- Output to flows-log:
  ```
  buildit-initial-build | [repo] | [✓/✗] | [error-count] [warning-count]
  ```

### Phase 2: Ghcid Startup (If Build Succeeds)

If initial build is clean:

Spin: action-ghcid-start

- Start: `ghcid --command="cabal repl" --outputfile=ghcid.txt`
- In target repo directory
- Store PID for later reference
- Output to flows-log:
  ```
  buildit-ghcid-start | [repo] | [PID] | ghcid ready
  ```

### Phase 3: Filewatch Loop

**While ghcid is running:**

- Watch for changes in src/, lib/, app/ directories
- On file change:
  - Tail ghcid.txt (fast feedback, 2-5s)
  - Check for errors in ghcid output
  - If error: log to flows-log, tailie picks up
  - If compile succeeds: log clear status

Output example:
```
buildit-ghcid-watch | [repo] | [file-changed] | [status: ✓/✗] | [error-count if any]
```

### Phase 4: Revert to Full Build (On Non-Compile Error)

If ghcid shows errors but they're not compile errors (e.g., import issues, pragma problems):

Spin: action-cabal-build (full)

- Do not rely on ghcid's error; full rebuild
- Capture output
- Log result
- Stay in full-build mode until clean

---

## Termination

Buildit stops when:

- Operator requests stop (documented in yin-notes)
- Work scope changes significantly
- Repo is moved to main branch (preparation for verifyit)

Spin: action-ghcid-stop

```bash
pkill -f "ghcid.*[repo]"
```

Output to flows-log:
```
buildit-ghcid-stop | [repo] | stopped
```

---

## Key Behaviors

**Avoid build spam:**
- Ghcid remains running; yin watches ghcid.txt
- Don't spin `action-cabal-build` every 2 seconds
- Only full rebuild on actual errors or state changes

**Fast feedback:**
- Operator or yin reads ghcid.txt tail
- 2-5 second turnaround on file change
- Enables rapid error -> fix -> verify cycle

**Synthesis integration:**
- Buildit cards log to flows-log
- Tailie picks up domain signals (exec-heavy when building)
- Bloodhound detects patterns (build phase)

---

## Calls to Action Cards

- action-cabal-build (initial, and on non-compile errors)
- action-ghcid-start (if initial build succeeds)
- action-ghcid-stop (on termination)

---

## Typical Usage Pattern

```
lockit completes
  ↓
operator approves and logs in yin-notes: "Starting development"
  ↓
yin spins buildit
  ↓
action-cabal-build runs; captures initial state
  ↓
if ✓: action-ghcid-start (ghcid watches)
  ↓
operator edits code
  ↓
yin/operator tails ghcid.txt; sees errors
  ↓
operator fixes; ghcid recompiles (2-5s)
  ↓
repeat until satisfied
  ↓
when ready: operator logs in yin-notes, buildit can stop or keep running
```

---

## Notes

Buildit is the engine for rapid development feedback. It's meant to stay running, ask for builds when needed (via synthesis signals), and enable the operator to see consequences of edits immediately.

