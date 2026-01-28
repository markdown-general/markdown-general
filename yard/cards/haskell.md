haskell ⟜ environment, standards, and refactoring principles

## Dependency Refactoring

**Never swap dependencies at once.**

The mistake: Replace old dependency + rewrite code simultaneously. When it fails, you can't tell if it's a dependency resolution error, import error, or type error.

Correct way: Incremental substitution.

```
1. Add new lib to build-depends (KEEP old lib)
2. cabal build (verify both compile)
3. Rewrite code to use new lib (imports both)
4. cabal build (narrow down errors to new lib only)
5. Switch imports (old to new per-module)
6. Remove old lib from build-depends
7. cabal build (final clean build)
```

Reason: Separate concerns (dependency ≠ code), isolate failures, debug clearly.

## GHC Versions

Test against last 3 stable GHC versions + latest:
- ghc ==9.10.2
- ghc ==9.12.2
- ghc ==9.14.1

tested-with in .cabal must match CI matrix.

## Language Extensions

Standard extensions (set in .cabal common stanza):
- OverloadedStrings (text handling)
- OverloadedLabels (graphics API)
- DataKinds (type-level programming)

Avoid without purpose:
- TemplateHaskell (unless required)
- PatternSynonyms (unless essential)
- UndecidableInstances (maintenance burden)

## When to Read

Read this before:
- big refactors (understand incremental approach, version compatibility)
- library migrations (follow dependency substitution pattern)
- setting up new package (GHC defaults, extensions)
- changing language pragmas (check against practices)

Skip if:
- linting (hlint-check sufficient)
- formatting (ormolu-format sufficient)
- small targeted fixes (ghcid-loop sufficient)
