# haskell-repl-wip ⟜ status and next steps

Haskell Repl library has `startRepl` and `queryRepl` implemented, but hitting a threading/file-locking issue that needs architectural rework.

## what works

- **startRepl**: Spawns cabal repl, pipes stdin through a background thread, writes stdout/stderr to files
- **queryRepl**: Appends queries, polls for responses, extracts matching lines
- **Build**: Clean, no type errors
- **Communication files**: Created and flowing correctly

## what doesn't work yet

File-locking contention:
- inputThread continuously reads /tmp/ghci-in.txt (polling every 50ms)
- queryRepl also reads /tmp/ghci-in.txt repeatedly
- Lazy IO + concurrent reads cause locks and timeouts
- Responses DO appear in output file (verified), but queryRepl can't reliably read them

## why bash version works

The bash repl-session-start/repl-query flow sidesteps this:
- `tail -f /tmp/ghci-in.txt` handles stdin piping (kernel-level, not Haskell threading)
- repl-query is a simple append + poll pattern
- No contention because separate processes, not threads

## next steps for Haskell version

Three options:

**Option 1:** Use channels instead of file polling
- Replace inputThread file-polling with a queue
- Pass queries through a channel instead of file
- Less general, but eliminates contention

**Option 2:** Use strict file reading with locks
- Switch to `withFile` with proper locking
- Serialize access to input file
- More robust but slower

**Option 3:** Use pty library
- Spawn process with pseudo-terminal instead of pipes
- More complex but might handle buffering better
- Industry standard for REPL-like tools

## recommendation for now

Use the bash interface for testing:
```bash
repl-session-start ~/repos/repl
repl-query ":t fmap"
```

This works perfectly and proves the protocol. The Haskell library can be refined later once we have more experience with the actual performance patterns.

## commits so far

- `eb807d6` — startRepl initial implementation
- `9a84dc1` — queryRepl implementation
- (attempted: Haskell test, hit file-locking issue)

Keep the library. Use bash for now. Circle back when we have time for architectural choices.
