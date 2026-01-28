# yin/ ⟜ Haskell repl design and patterns

Working design for file-based GHCi protocol. Separates concerns: spawning, querying, observing.

## How it works

**File-based message passing for repl queries:**

1. **Write** queries to `/tmp/ghci-in.txt`
2. **Wait** for processing (50-500ms)
3. **Read** responses from `/tmp/ghci-out.txt`
4. **Search** for expected patterns (type sigs, kind info, etc.)
5. **Extract** answers and continue

No synchronization overhead—no markers, locks, or handshakes. The protocol works with any backend that processes queries and writes output. Verified with mock-repl (test double) and real cabal repl.

## Cards

### Design & Patterns

**repl-user.md** ⟜ query pattern
- How to send a query and get a response
- Write to `/tmp/ghci-in.txt`, wait, read `/tmp/ghci-out.txt`, search for pattern
- No synchronization complexity needed

**drift-ghci-echo.md** ⟜ piped input behavior
- Real cabal repl doesn't echo piped commands (unlike terminal REPL)
- Protocol handles this: search for response pattern, not command echo
- Shows how file-based approach is robust to backend differences

### Implementation

**repl-startup.md** ⟜ spawn cabal repl with file stdio
- Opens `/tmp/ghci-*.txt` for process stdin/stdout/stderr
- Returns ProcessHandle for monitoring
- Process persists until killed
- Ready for queries via file-based protocol

**cabal-init-repl.md** ⟜ scaffold a repl library project
- Creates ~/repos/repl/ with minimal dependencies (base + process)
- Setup for developing repl functions
- Clean build, ready to add startRepl/queryRepl

### Testing

**repl-mock.md** ⟜ simulate GHCi for testing
- Reads queries from `/tmp/ghci-in.txt`
- Responds with type/kind/info lookups (from knowledge base)
- Proves the protocol works before touching real cabal repl

## Development Notes

**repl-notes/** ⟜ research and design exploration
- `process-createProcess.md` — System.Process API study
- `async-key.md` — async/race design for timeouts and composition

These are not currently implemented but document the path forward for:
- Haskell library wrapping the protocol
- Concurrent query handling with timeouts
- Error recovery patterns

## Next

1. **Implement startRepl** — wire repl-startup pattern into Haskell
2. **Implement queryRepl** — query/response with timeout handling (use async/race)
3. **Wire into larger system** — type wrangling, agentic exploration

Proven pattern → working library → integrated tooling.
