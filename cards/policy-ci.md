# card: policy-ci ⟜ CI strategy and tested-with alignment

**type** ⟜ policy / setup (operator + captain agreement)

**purpose** ⟜ define what "tested" means, align tested-with with CI matrix, establish defaults

**scope** ⟜ all Haskell development cycles in this project

---

## Problem: CI/tested-with Incoherence

**Test discovery:** huihua.cabal declared `tested-with: 9.6.7, 9.8.4, 9.10.2, 9.12.2` but GitHub Actions CI was not testing 9.14.1, resulting in surprise bounds failures.

**Root cause:** No enforcement that tested-with matches CI matrix. These are defined separately and can drift.

---

## Policy: Default tested-with Versions

**Standard:** Test against last 3 stable GHC versions + latest

### Current Recommended Defaults (as of 2026-01-15)

```
tested-with:
  ghc ==9.10.2
  ghc ==9.12.2
  ghc ==9.14.1
```

**Rationale:**
- 9.10.2: LTS-22 (still widely used)
- 9.12.2: LTS-23 (recent stable)
- 9.14.1: Current latest (enables modern language features)

**Update cadence:** Review quarterly. When new LTS major version released, bump oldest version off; add new one.

### How to Update Defaults

1. Check [GHC releases](https://www.haskell.org/ghc/download_ghc_9_14_1.html)
2. Identify last 3 stable releases
3. Update this policy card with new defaults
4. Update GitHub Actions CI matrix to match
5. For any in-development repo, cite this policy in lockit

---

## Policy: CI Matrix Definition

**GitHub Actions workflow must test:**
- All versions in tested-with
- All major platforms: ubuntu-latest, macos-latest, windows-latest
- Exception: windows can be single-GHC if resource constraints (document in repo)

### Template: .github/workflows/haskell-ci.yml

```yaml
strategy:
  matrix:
    ghc: [9.10.2, 9.12.2, 9.14.1]
    os: [ubuntu-latest, macos-latest, windows-latest]
```

This ensures:
- `tested-with` in cabal file = versions tested in CI
- CI matrix auto-defined from cabal file (or policy defaults)
- No drift

---

## Verification: Cross-Check at lockit

**New lockit audit phase:**

When operator runs lockit on any repo:
1. Extract tested-with from .cabal file
2. Compare against active GitHub Actions CI matrix
3. If mismatch → flag for operator review
4. Log to flows-log: `lockit-ci-alignment-check | [repo] | tested-with: [versions] | ci-matrix: [versions] | ✓/⚠`

If mismatch found:
- Option A: Update .cabal tested-with to match CI (recommended)
- Option B: Update CI to match .cabal
- Option C: Document deliberate difference (e.g., "testing only on Linux for performance")

---

## Reference: Standard Cabal Configuration

### Default tested-with stanza

Add to every new package:

```cabal
tested-with:
  ghc ==9.10.2
  ghc ==9.12.2
  ghc ==9.14.1
```

### Default language & extensions stanza

```cabal
default-language:
  GHC2024

default-extensions:
  DataKinds
  DerivingStrategies
  DisambiguateRecordFields
  ExplicitNamespaces
  GADTs
  LambdaCase
  MonoLocalBinds
  RoleAnnotations
```

For GHC < 9.10, fallback to GHC2021 with hand-coded extensions.

### Default ghc-options stanza

```cabal
ghc-options:
  -Wall
  -Wcompat
  -Widentities
  -Wincomplete-record-updates
  -Wincomplete-uni-patterns
  -Wpartial-fields
  -Wredundant-constraints
```

---

## Captain Responsibility

**Captain defines and maintains this policy.**

- Review quarterly (or when GHC stable release happens)
- Update tested-with defaults in this card
- Communicate changes to team
- Ensure CI templates reflect policy

**CI drift reports to captain:** If any repo's tested-with diverges from policy without documented reason, flag in team sync.

---

## Operator Responsibility

**Operator ensures alignment at lockit:**

1. Check repo's tested-with against policy-ci defaults
2. If outdated, propose update to match current policy
3. Log decision in yin-notes
4. Ensure CI matrix is then verified

**If you don't update tested-with, you accept existing CI matrix. That's fine if deliberate, but must be logged.**

---

## Yin Responsibility

**Yin cross-checks during development:**

- In lockit audit: compare tested-with vs CI matrix
- In verifyit: ensure CI actually ran on all declared versions
- Flag mismatches to flows-log

---

## Notes

This policy exists because:
- tested-with is a promise to users (we tested against these versions)
- CI matrix is a promise to code (we actually run these versions)
- When they drift, one promise is broken

By aligning them at lockit and maintaining this policy, we keep both promises honest.

**Historical note:** GHC 9.14 was missing from huihua CI despite being in code, causing surprise breakage on first run. This policy prevents that.

---

**Policy version:** 1.0 (2026-01-15)
**Last reviewed:** 2026-01-15
**Next review:** 2026-04-15 (or when GHC 9.16 released)
