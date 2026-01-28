# mock-repl-agent ⟜ simulate GHCi with file-based query/response

Spawned as a background agent. Reads queries from `/tmp/ghci-in.txt`, looks them up in a knowledge base (Hyp.hs concepts), writes responses to `/tmp/ghci-out.txt`. Stays alive, polling for new queries.

## what it does

- Initialize: clear input and output files
- Loop (50ms polling):
  - Read `/tmp/ghci-in.txt`
  - Detect new queries (lines we haven't seen)
  - Look up response in knowledge base
  - Append to `/tmp/ghci-out.txt` with prompt echo and response
  - Continue

## assumptions

- `/tmp/` is writable
- Queries are one per line
- Knowledge base covers the test cases (fmap, Maybe, run, base, pipe, Hyp)
- Observer will send queries as plain text (`:t fmap`, `:k Maybe`, etc.)

## files

**Reads:**
- `/tmp/ghci-in.txt` — query file (created empty on start)

**Writes:**
- `/tmp/ghci-out.txt` — response file (created empty on start)

## execution

```bash
/tmp/mock-repl.sh > /tmp/mock-repl.log 2>&1 &
```

Runs in background indefinitely. Kill with:

```bash
kill <PID>
```

## signals

- **Agent started**: first line of log is "mock-repl starting"
- **Query processed**: log shows `[processed] <query>`
- **Files exist**: both `/tmp/ghci-in.txt` and `/tmp/ghci-out.txt` present after first write
- **Agent alive**: process still in `ps` output

## knowledge base

```
:t fmap  →  fmap :: Functor f => (a -> b) -> f a -> f b
:k Maybe  →  Maybe :: * -> *
:t run  →  run :: Hyp a a -> a
:t base  →  base :: a -> Hyp a a
:t pipe  →  pipe :: Hyp b a -> b
:k Hyp  →  Hyp :: * -> * -> *
import*  →  Loaded.
other  →  error: not in scope: <query>
```

## next

Once spinning, observer sends queries. Watch `/tmp/ghci-out.txt` grow with responses. All queries should appear with matching responses within 2 seconds.

This agent proves the file-based protocol works before we wire real cabal repl.
