# test-repl-protocol ⟜ verify file-based query/response works

Test harness for the repl communication pattern. Spins mock-repl and observer agents, verifies five queries succeed, confirms protocol is sound before wiring real cabal repl.

## what it does

1. Clean `/tmp/ghci-*.txt` files
2. Start mock-repl agent in background
3. Wait 300ms for initialization
4. Start observer agent (runs to completion)
5. Let observer run its test loop
6. Report logs and file state
7. Kill mock-repl when done

## files involved

**Agents:**
- `mock-repl.sh` — simulates GHCi (read queries, write responses)
- `observer.sh` — sends queries and validates responses

**Cards:**
- `mock-repl-agent.md` — documents what mock-repl does
- `observer-agent.md` — documents what observer does

**Communication files:**
- `/tmp/ghci-in.txt` — input (queries)
- `/tmp/ghci-out.txt` — output (responses)
- `/tmp/ghci-err.txt` — errors (created by repl-startup, unused by mock)

**Logs:**
- `/tmp/mock-repl.log` — agent activity
- `/tmp/observer.log` — test results

## execution

```bash
cd /Users/tonyday567/mg/yin

# Clean slate
rm -f /tmp/ghci-*.txt

# Start mock-repl
./mock-repl.sh > /tmp/mock-repl.log 2>&1 &
MOCK_PID=$!

# Give it a moment
sleep 0.3

# Run observer (blocks until done)
./observer.sh > /tmp/observer.log 2>&1

# Inspect results
cat /tmp/observer.log
cat /tmp/ghci-in.txt
cat /tmp/ghci-out.txt

# Clean up
kill $MOCK_PID
```

## expected output

**Observer log:**
```
observer starting

Sending: :t fmap
  ✓ Got response with: fmap ::
Sending: :k Maybe
  ✓ Got response with: * -> *
...
Results: 5 passed, 0 failed
```

**ghci-in.txt:**
```
:t fmap
:k Maybe
:t run
:t base
:t pipe
```

**ghci-out.txt:**
```
Prelude> :t fmap
fmap :: Functor f => (a -> b) -> f a -> f b

Prelude> :k Maybe
Maybe :: * -> *
...
```

## signals

- **Protocol works**: observer shows all 5 queries with ✓ response
- **Protocol stalled**: observer times out on first query (check mock-repl.log for errors)
- **File buffering**: responses appear in ghci-out.txt within 200ms of query

## next

Once this passes:
1. Replace mock-repl with real cabal repl (via repl-startup card)
2. Run observer again with same files
3. Confirm same test queries succeed with actual Haskell process

The protocol is the contract. The implementation (mock vs. real) is just a swap.
