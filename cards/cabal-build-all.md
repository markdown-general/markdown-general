cabal-build-all ⟜ authoritative compilation with clean cache

**input** ⟜ repo with .cabal file

**output** ⟜ [✓/✗] + build log

**instruction**
cd [repo]
cabal clean && cabal build all 2>&1
