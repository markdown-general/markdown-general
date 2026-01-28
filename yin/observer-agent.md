# observer-agent ⟜ send queries and validate responses

Spawned as a background agent. Sends five test queries to `/tmp/ghci-in.txt`, polls `/tmp/ghci-out.txt` for responses, validates patterns, reports results.

## what it does

- For each test query in sequence:
  - Write query to `/tmp/ghci-in.txt`
  - Poll `/tmp/ghci-out.txt` (up to 20 attempts, 100ms each = 2 seconds)
  - Check for query echo **and** expected pattern in output
  - Log result (✓ or ✗)
- After all queries:
  - Report final count: N passed, M failed
  - Exit 0 if all passed, exit 1 if any failed

## assumptions

- mock-repl agent is already running (or real cabal repl via repl-startup)
- Files exist at `/tmp/ghci-in.txt` and `/tmp/ghci-out.txt`
- Responses appear within 2 seconds
- Expected patterns are exact substrings

## test queries

```
:t fmap  →  expect: "fmap ::"
:k Maybe  →  expect: "* -> *"
:t run  →  expect: "run ::"
:t base  →  expect: "base ::"
:t pipe  →  expect: "pipe ::"
```

## execution

```bash
/tmp/observer.sh > /tmp/observer.log 2>&1
```

Runs to completion. Exit code indicates pass/fail.

## signals

- **Starting**: log shows "observer starting"
- **Query sent**: log shows "Sending: <query>"
- **Response found**: log shows "✓ Got response with: <pattern>"
- **Timeout**: log shows "✗ Timeout"
- **Complete**: final line shows "Results: N passed, M failed"
- **Exit code**: 0 = all passed, 1 = some failed

## behavior

Simple polling loop. No state machine, no synchronization primitives. Just:
1. Write query
2. Sleep 100ms
3. Read entire output file
4. Search for query echo + pattern
5. Move to next query

Decoupled from what's on the other end. Works with mock-repl. Works with real cabal repl.

## next

Run this after spinning mock-repl. When all tests pass, swap mock-repl for real cabal repl (via repl-startup card) and run observer again. Same agent, different backend.
