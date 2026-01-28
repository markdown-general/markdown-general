# repl-startup ⟜ spawn cabal repl process with file-based stdio

Spawn `cabal repl` in a Haskell project. Point stdin, stdout, stderr at temp files. Process runs and stays alive.

## what you get

- `ProcessHandle` — the live process
- Three files as ground truth:
  - `/tmp/ghci-in.txt` — queries written here arrive at cabal repl stdin
  - `/tmp/ghci-out.txt` — cabal repl stdout appears here
  - `/tmp/ghci-err.txt` — cabal repl stderr appears here

## assumptions

- You are in (or specifying) a Haskell project where `cabal repl` works
- GHCi is ready to accept queries
- `/tmp/` is writable

## Haskell signature

```haskell
startRepl :: FilePath -> IO ProcessHandle
```

Takes a project directory. Spawns process. Returns handle. Doesn't wait for queries. Just starts it.

## implementation notes

Use `System.Process`:
- `createProcess` with `shell "cd <dir> && cabal repl"`
- Set `std_in = UseHandle` (open `/tmp/ghci-in.txt` in ReadMode)
- Set `std_out = UseHandle` (open `/tmp/ghci-out.txt` in WriteMode)
- Set `std_err = UseHandle` (open `/tmp/ghci-err.txt` in WriteMode)
- Use `LineBuffering` so output appears immediately
- Close the handles after passing to process (process now owns them)

## signals

- Process alive: `getProcessExitCode` returns `Nothing`
- Process dead: `getProcessExitCode` returns `Just code`
- Output in files: read them anytime

## next

Once running, other agents write queries (repl-query-response card) and read answers.
