# repl-session-lifecycle ⟜ logged, instrumented ghci sessions

Full lifecycle for querying a Haskell repl with clean interfaces. Start session → query cleanly → watch ticker → stop session → review log and degradation patterns.

## lifecycle

### Start a session

```bash
repl-session-start /path/to/haskell/project [session-name]
```

This:
- Initializes communication files (`/tmp/ghci-*.txt`)
- Starts `cabal repl` in the project directory
- Begins logging to `~/mg/yin/logs/session-TIMESTAMP-NAME.log`
- Prints the log path and PID
- Stores session state in `/tmp/ghci-session.env`

Example:
```bash
repl-session-start ~/repos/hyp1 type-wrangling
# Output: Session started. Log at: ~/mg/yin/logs/session-20250128-143022-type-wrangling.log
```

Wait for GHCi startup (5-10 seconds for typical project).

### Query the repl

```bash
repl-query ":t Something"
repl-query ":k Maybe"
repl-query ":i Functor"
```

This:
- Writes query to `/tmp/ghci-in.txt`
- Polls for response (2-second timeout)
- Prints clean response (no noise)
- Logs query + response to session log

Example:
```bash
repl-query ":t fmap"
# Output: fmap :: Functor f => (a -> b) -> f a -> f b
```

### Watch the ticker (optional)

In another terminal:

```bash
repl-ticker
# Refreshes every 1 second, shows last 15 lines of ghci output
```

Or with custom line count:

```bash
repl-ticker 25
```

Useful for seeing GHCi activity without bash scrollage. Just watch it refresh.

### Stop the session

```bash
repl-session-stop
```

This:
- Kills the cabal repl process
- Logs session end timestamp
- Runs degradation check (warns about issues)
- Saves full log for review

## session files

**Logs:**
- `~/mg/yin/logs/session-TIMESTAMP-NAME.log` — full transcript (startup, queries, responses, stderr)

**Communication (temp):**
- `/tmp/ghci-in.txt` — queries (appended by repl-query)
- `/tmp/ghci-out.txt` — responses (appended by cabal repl)
- `/tmp/ghci-err.txt` — stderr (warnings, type errors)

**State (temp):**
- `/tmp/ghci-session.env` — session environment (paths, PIDs)
- `/tmp/ghci-repl.pid` — repl process ID

## degradation detection

When you `repl-session-stop`, a check runs:

```
=== Degradation Analysis ===
✓ GHCi startup: 12 lines (typical: 10-15)
✓ No build errors
✓ Compilation count normal
✓ No query failures
```

Or warnings if issues detected:

```
⚠ Multiple compilations detected: 5
  If doing many queries, consider:
    - Use ghcid for live reloading
    - Or persist your session (don't kill/restart)
```

This card gets consulted when:
- Switching projects (make sure cabal.project exists, `cabal build` first)
- Performance degrades (did we break something? check the log)
- Many repl queries (ghcid might be faster)

## workflow

Typical session:

```bash
# 1. Start
repl-session-start ~/repos/hyp1 exploring

# 2. (optional) In another terminal, watch
repl-ticker

# 3. Query
repl-query ":t run"
repl-query ":k Hyp"
repl-query ":i Functor"

# 4. Stop
repl-session-stop

# 5. Review log
cat ~/mg/yin/logs/session-20250128-143022-exploring.log
```

## implementation notes

**Logging** — `repl-session-start` uses `tee` to capture both stdout/stderr to the session log in real-time. Every query sent via `repl-query` also appends to the log.

**Query** — `repl-query` polls the output file (same observer pattern as before). Responses are filtered for cleanliness (removes prompts, extra whitespace).

**Ticker** — `repl-ticker` is a simple loop that clears the screen and shows the last N lines of ghci output, refreshing every 1 second. Runs independently.

**Degradation** — `check-repl-degradation.sh` analyzes the session log for:
- Startup latency (normal vs slow)
- Build errors (suggests `cabal build` first)
- Repeated compilations (suggests ghcid or session persistence)
- Query timeouts (infrastructure issue)

## next

1. Start session: `repl-session-start ~/repos/hyp1`
2. Wait for GHCi prompt
3. Try: `repl-query ":t fmap"`
4. Watch: `repl-ticker` in another terminal
5. Stop: `repl-session-stop`
6. Review: `cat ~/mg/yin/logs/session-*.log`

All four times you've wired this, you've had the same basic pattern. This card bakes in the learning.

Once this works cleanly, we wire it into Haskell (startRepl, queryRepl functions), and you'll have type wrangling at the speed of thought.
