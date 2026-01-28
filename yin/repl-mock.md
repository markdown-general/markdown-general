# mock-repl ⟜ test double simulating GHCi

Simulates a GHCi REPL for testing the query protocol before running real cabal repl.

## behavior

- Reads queries from `/tmp/ghci-in.txt`
- Looks up answers in a knowledge base (Hyp.hs types, for example)
- Writes responses to `/tmp/ghci-out.txt`
- Continues processing until killed

For each query:
- `:t Expression` → respond with type signature
- `:k Type` → respond with kind signature
- `:i Name` → respond with info (definition, instances)
- `import Module` → acknowledge with `Loaded.`
- Unknown → respond with `error: not in scope`

Each response includes prompt, query, and answer. Responses are appended to output file.

## purpose

Proves the file-based protocol works before running real cabal repl. Mock reads queries, responds from a knowledge base, and continues running. Uses the same communication files as the real backend (only the responder changes).

## next

Once mock-repl works, swap it with real cabal repl via repl-startup. Same queries, same protocol, different backend.
