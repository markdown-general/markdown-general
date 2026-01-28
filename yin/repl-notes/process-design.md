## Process Design


1. **repl-startup** — Spawn cabal repl, wire stdio to files
2. **mock-repl** — Simulate GHCi (proves the protocol)
3. **repl-query-response** — Ask questions, wait for answers (the actual pattern)

**We tested it.** Both agents (mock and observer) ran. Five test queries. All passed. No code failures. No mysterious hangs. Just files and observation.

## Why This Is Good Design

**Separation of Concerns:**
- `repl-startup` doesn't know about queries. It just starts the process.
- Writer doesn't care if reader exists. It just appends.
- Reader doesn't care when queries were sent. It just reads.
- Observer knows what it sent and can search for it. Local reasoning.

**No Synchronization Complexity:**
- No markers to inject
- No prompt parsing
- No buffer management
- No locks or condition variables
- Just: append queries, read output, search for answers

**Hyperfunction Insight:**
All variables are local. When the observer computes "did I get my answer?", it only needs:
- What it wrote (in its own memory)
- What's in the output file (current state)

There's no hidden state outside the observer's concern horizon. Each query-response cycle is a **continuation**: given the current files, what do I do next? The answer comes from local computation, not from watching shared state.

**Composability:**
Want to add filtering? New agent reads output file, filters startup noise, writes to `/tmp/ghci-filtered.txt`. Doesn't break anything.

Want to add a matching service? Agent reads both input and output, lines up questions with answers, writes to `/tmp/ghci-matched.json`. Still decoupled. One field agent, one job to do and verify.

Want per-agent output files? Each observer writes to its own file. No contention.

The **category** (hyperfunctions) says this can't go wrong if you follow the rules. And the rules are simple:
- Each agent has a local concern horizon
- Information flows through applicative functors (IO, effects)
- No hidden state outside the computation
