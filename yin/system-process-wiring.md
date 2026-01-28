# system-process-wiring ⟜ intended usage for startRepl

We need to spawn `cabal repl` in a project and wire its stdin, stdout, stderr to files. This card documents what we want, then we explore System.Process to see how to build it.

## the contract

```haskell
startRepl :: FilePath -> IO ProcessHandle
```

Takes a project directory. Spawns `cabal repl` there. Returns a handle to the running process. That's it.

**Side effects:**
- Initializes `/tmp/ghci-in.txt`, `/tmp/ghci-out.txt`, `/tmp/ghci-err.txt` (empty)
- Spawns process with stdin fed from `tail -f /tmp/ghci-in.txt`
- Process writes stdout to `/tmp/ghci-out.txt`
- Process writes stderr to `/tmp/ghci-err.txt`
- Process stays alive until killed

**Usage:**
```haskell
main = do
  h <- startRepl "/path/to/project"
  -- Now observers can write queries to /tmp/ghci-in.txt
  -- And read responses from /tmp/ghci-out.txt
  -- Process handle h can be checked with getProcessExitCode
```

## questions to explore

1. What's the exact signature of `createProcess`? (arguments, return type, monad)
2. How do we wire a file handle to stdin? (UseHandle? CreatePipe? Which mode for input/output?)
3. What does "file handle" mean? (System.IO.Handle? Do we open the file first?)
4. How do we keep the process alive in the background? (fork? return immediately?)
5. What's the difference between `createProcess` and `createProcess_`?
6. How do we handle the `tail -f` idiom? (shell command with pipes?)

## bash reference

Our working implementation:
```bash
(tail -f /tmp/ghci-in.txt 2>/dev/null) | cabal repl > /tmp/ghci-out.txt 2> /tmp/ghci-err.txt &
```

This:
- Starts `tail -f` reading the input file (blocks until content, then streams new lines)
- Pipes that to `cabal repl` (which reads stdin, produces output)
- Captures stdout → `/tmp/ghci-out.txt`
- Captures stderr → `/tmp/ghci-err.txt`
- Runs in background (`&`)

In Haskell, we need to express this without shell.

## next step

Query GHCi:
- `:i createProcess`
- `:i ProcessHandle`
- `:i UseHandle`
- `:t openFile`
- `:i System.Process` (what's exported?)

Get real types, see what's available, then write the function.
