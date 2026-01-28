haskell-build ⟜ default method to see ghc compiler output
 ⟜ cabal build is a time-expensive operation in a tight build loop and we prefer a shared loop (see build-log-request) and then a cabal repl. Before either can be possible, you need a cabal build.

**instruction**
Try in order

1. **build-log-check** ⟜ if build-log-request is already running (instant, already cached)
2. **cabal-repl** ⟜ if you want interactive check (fast, safe)
3. **cabal-build** ⟜ if you need authoritative answer (slow, complete)
