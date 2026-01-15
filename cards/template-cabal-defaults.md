# card: template-cabal-defaults ⟜ standard stanzas and boilerplate

**type** ⟜ template / reference

**purpose** ⟜ provide copy-paste defaults for new packages to ensure consistency

**scope** ⟜ all new Haskell packages in this project

---

## Quick Start

When creating a new package, use these stanzas as a starting point.

---

## File Header & Metadata

```cabal
cabal-version: 3.4
name: [package-name]
version: 0.1.0.0
license: BSD-3-Clause
copyright: (c) 2026, [author name]
author: [author name]
maintainer: [email]
homepage: https://github.com/[user]/[package]#readme
bug-reports: https://github.com/[user]/[package]/issues
synopsis: [one-line description]
description: [longer description, typically 2-3 sentences]
build-type: Simple
category: [category]
```

---

## Source Repository

```cabal
source-repository head
  type: git
  location: https://github.com/[user]/[package]
```

---

## Version Matrix & Language

```cabal
tested-with:
  ghc ==9.10.2
  ghc ==9.12.2
  ghc ==9.14.1
```

**See:** policy-ci for rationale and update cadence.

---

## Common Stanzas

### GHC Options (Warnings)

```cabal
common ghc-options-stanza
  ghc-options:
    -Wall
    -Wcompat
    -Widentities
    -Wincomplete-record-updates
    -Wincomplete-uni-patterns
    -Wpartial-fields
    -Wredundant-constraints
```

### Language & Extensions (GHC2024)

```cabal
common ghc2024-stanza
  if impl(ghc >=9.10)
    default-language:
      GHC2024
  else
    default-language:
      GHC2021
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

**Why this approach:** GHC2024 is only available in GHC 9.10+. For older versions, we manually add the extensions that GHC2024 includes.

---

## Library Section

```cabal
library
  import: ghc-options-stanza
  import: ghc2024-stanza
  hs-source-dirs: src
  exposed-modules:
    [Module.Name]
    [Module.Another]
  other-modules:
    [Module.Internal]
  build-depends:
    base >=4.14 && <5,
    [package] >=[version] && <[next-major],
    [more-deps],
```

### Dependency Bounds Pattern

For each build-depend:
- **Lower bound:** minimum version you've tested against
- **Upper bound:** next major version (exclude breaking changes)

Example:
```
base >=4.14 && <5,          -- GHC 9.0 base to before GHC 10
containers >=0.6 && <0.7,  -- tested with 0.6.x, exclude 0.7
text >=2.0 && <2.2,        -- tested with 2.0.x and 2.1.x
```

**Rule of thumb:** Prefer `< next-major` bounds unless you know package is stable across majors.

---

## Test Suite

### Unit Tests

```cabal
test-suite tests
  import: ghc2024-stanza
  type: exitcode-stdio-1.0
  main-is: Main.hs
  hs-source-dirs: test
  other-modules:
    [Test.Module]
  build-depends:
    base >=4.14 && <5,
    [package],
    hspec >=2.10 && <2.12,
  ghc-options: -threaded -rtsopts -with-rtsopts=-N
```

### Documentation Tests (cabal-docspec)

```cabal
test-suite doctests
  import: ghc2024-stanza
  type: exitcode-stdio-1.0
  main-is: doctests.hs
  hs-source-dirs: test
  build-depends:
    base >=4.14 && <5,
    cabal-docspec >=0.3 && <0.5,
    [package],
  ghc-options: -threaded
```

---

## Benchmarks (Optional)

```cabal
benchmark bench
  import: ghc2024-stanza
  type: exitcode-stdio-1.0
  main-is: Main.hs
  hs-source-dirs: bench
  build-depends:
    base >=4.14 && <5,
    [package],
    criterion >=1.5 && <1.7,
  ghc-options: -threaded -O2
```

---

## Example: Complete Minimal Package

```cabal
cabal-version: 3.4
name: example-lib
version: 0.1.0.0
license: BSD-3-Clause
author: Your Name
maintainer: you@example.com
homepage: https://github.com/you/example-lib#readme
bug-reports: https://github.com/you/example-lib/issues
synopsis: Example library
build-type: Simple

source-repository head
  type: git
  location: https://github.com/you/example-lib

common ghc-options-stanza
  ghc-options:
    -Wall
    -Wcompat
    -Widentities
    -Wincomplete-record-updates
    -Wincomplete-uni-patterns
    -Wpartial-fields
    -Wredundant-constraints

common ghc2024-stanza
  if impl(ghc >=9.10)
    default-language: GHC2024
  else
    default-language: GHC2021
    default-extensions:
      DataKinds
      DerivingStrategies
      DisambiguateRecordFields
      ExplicitNamespaces
      GADTs
      LambdaCase
      MonoLocalBinds
      RoleAnnotations

tested-with:
  ghc ==9.10.2
  ghc ==9.12.2
  ghc ==9.14.1

library
  import: ghc-options-stanza
  import: ghc2024-stanza
  hs-source-dirs: src
  exposed-modules:
    Example
  build-depends:
    base >=4.14 && <5,
    containers >=0.6 && <0.7,

test-suite tests
  import: ghc2024-stanza
  type: exitcode-stdio-1.0
  main-is: Main.hs
  hs-source-dirs: test
  build-depends:
    base >=4.14 && <5,
    example-lib,
    hspec >=2.10 && <2.12,
  ghc-options: -threaded -rtsopts -with-rtsopts=-N
```

---

## Rationale

These defaults ensure:
- **Consistency:** All packages follow same structure
- **Maintainability:** Easy to update across projects (e.g., bump GHC version)
- **Best practices:** Warnings enabled, bounds specified, tests included
- **Modern Haskell:** GHC2024 extensions by default

---

## When to Deviate

- **GHC <9.10:** Use GHC2021 explicitly
- **No tests:** Remove test-suite stanzas
- **Minimal packages:** Omit benchmarks
- **Special requirements:** Document reason in package README

---

**See also:**
- policy-ci (tested-with reasoning)
- cleanit (formatting standards)
- dependendit (bounds management)
