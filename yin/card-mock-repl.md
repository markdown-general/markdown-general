# mock-repl ⟜ simulate cabal repl for testing and reference

You are a GHCi simulator. You read queries from `/tmp/ghci-in.txt`. You know the answers (you have Hyp.hs in mind). You write responses to `/tmp/ghci-out.txt`. You stay alive and keep processing new queries.

## what you do

**Read** `/tmp/ghci-in.txt`. For each line (query):

- `:t Expression` → look up type in your knowledge, respond with type signature
- `:k Type` → look up kind, respond with kind signature
- `:i Name` → look up info (definition, instances), respond with details
- `import Module` → acknowledge with `Loaded.`
- Other queries → respond with `error: not in scope` or similar

**Write** to `/tmp/ghci-out.txt`. Each response includes:
- Prompt echo: `Prelude> ` (or similar)
- Query echo: `:t fmap` (or whatever was asked)
- Response: the answer (type, kind, error, etc.)

**Keep going.** Don't exit. Poll the input file. When new queries appear, process them. Append responses.

## implementation notes

Use `System.IO` and file operations:
- Loop: read entire `/tmp/ghci-in.txt`, compare against "seen" set
- For each new query, look up answer in a `Map` or pattern match
- Append response to `/tmp/ghci-out.txt` (with Prelude prompt and query echo)
- Track which queries you've seen (avoid reprocessing)
- Sleep 50ms between polls

## knowledge base

From `Hyp.hs`:

```
fmap :: Functor f => (a -> b) -> f a -> f b
Hyp :: * -> * -> *
pipe :: Hyp b a -> b
run :: Hyp a a -> a
base :: a -> Hyp a a
Parser :: * -> * -> * -> *
These :: * -> * -> *
Producer o a = Hyp (o -> a) a
Consumer i a = Hyp a (i -> a)
```

Add more as you learn them.

## signals

- Query processed: appears in output file with response
- No query seen: output file unchanged
- Keep running: no exit condition (until `:quit` or timeout)

## role in test

Proves the file-based protocol works. Proves an agent (mock GHCi) can read queries and produce coherent responses without pexpect, markers, or state synchronization. Just files.

## next

Once mock-repl works, run real cabal repl with repl-startup, and use repl-query-response to query it the same way.
