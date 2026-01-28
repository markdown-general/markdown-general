action-ormolu ⟜ format Haskell source files

**input** ⟜ repo state (depends on tool)

**output** ⟜ [✓/✗] + results

**instruction**
ormolu --mode inplace $(git ls-files '*.hs'); reformat all Haskell source
