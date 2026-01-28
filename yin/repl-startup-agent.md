# repl-startup-agent ⟜ spawn cabal repl with file-based stdio

Spawn a real `cabal repl` process in a Haskell project. Wire its stdin to `/tmp/ghci-in.txt`, stdout to `/tmp/ghci-out.txt`, stderr to `/tmp/ghci-err.txt`. Process stays alive and responds to queries.

## what it does

1. Initialize: clear the three communication files
2. Change to project directory
3. Spawn `cabal repl` with input piped from `tail -f /tmp/ghci-in.txt`
4. Redirect stdout → `/tmp/ghci-out.txt`
5. Redirect stderr → `/tmp/ghci-err.txt`
6. Stay running until killed

## assumptions

- You are in or specify a Haskell project with cabal
- `cabal repl` works in that project (cabal.project, .cabal files present)
- `/tmp/` is writable
- GHC and cabal are in PATH

## execution

```bash
cd /Users/tonyday567/mg/yin

# Start repl in a project
./repl-startup.sh /path/to/haskell/project > /tmp/repl-startup.log 2>&1 &
REPL_PID=$!

# Give it time to boot (GHC startup is slow)
sleep 5

# Now observer can send queries
./observer.sh

# When done
kill $REPL_PID
```

Or use current directory:

```bash
./repl-startup.sh . > /tmp/repl-startup.log 2>&1 &
```

## files

**Input:**
- `/tmp/ghci-in.txt` — where observer writes queries (fed to cabal repl stdin via `tail -f`)

**Output:**
- `/tmp/ghci-out.txt` — where cabal repl stdout appears
- `/tmp/ghci-err.txt` — where cabal repl stderr appears (GHCi banner, warnings, errors)

**State:**
- `/tmp/ghci-repl.pid` — stores the PID for cleanup

## signals

- **Started**: first line of log is "repl-startup: spawning cabal repl"
- **PID logged**: log shows `PID: <number>`
- **Process alive**: `ps <PID>` still shows cabal repl
- **Startup complete**: output files exist, cabal has printed its banner to `-out.txt`
- **Query processed**: observer detects response in output file within 2 seconds

## startup latency

GHC startup is slow (5-10 seconds typical). Wait before sending first query. Check `/tmp/ghci-out.txt` for GHCi prompt before running observer.

## next

Once repl-startup is running:
1. Wait for GHCi prompt in output file
2. Spin observer agent (same as with mock)
3. Same queries, same expected patterns
4. Protocol is proven with real process

The interface is identical to mock-repl. Only the backend changed.
