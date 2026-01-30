# repl-startup ⟜ spawn cabal repl process with file-based stdio

Spawn `cabal repl` in a Haskell project. Point stdin, stdout, stderr at temp files. Process runs and stays alive.

## result

- `ProcessHandle` — the live process
- Three files for communication:
  - `/tmp/ghci-in.txt` — queries written here arrive at cabal repl stdin
  - `/tmp/ghci-out.txt` — cabal repl stdout appears here
  - `/tmp/ghci-err.txt` — cabal repl stderr appears here

## requirements

- Haskell project with working `cabal repl`
- GHCi ready to accept queries
- `/tmp/` writable

## function signature

```haskell
startRepl :: FilePath -> IO ProcessHandle
```

Takes a project directory, spawns the process, returns a handle for monitoring. Process persists until killed.

## implementation

Uses `System.Process`:
- Spawn `cabal repl` in target project directory
- Open temp files for stdin, stdout, stderr
- Wire process handles to those files
- Return `ProcessHandle` for monitoring
- Process persists until explicitly killed

## signals

- Process alive: `getProcessExitCode` returns `Nothing`
- Process dead: `getProcessExitCode` returns `Just code`
- Output in files: read them anytime

## next

Once running, observe queries written to `/tmp/ghci-in.txt` arriving at stdin, and responses in `/tmp/ghci-out.txt`. Protocol works with any reader respecting file-based message passing.
