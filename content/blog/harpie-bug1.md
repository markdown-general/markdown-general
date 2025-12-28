---
title: "harpie bug#1"
date: 2025-12-23
status: active
---

# harpie bug#1

Starting with unsafesnatsfix

## harpie core

- [ ] try -ffull-laziness and -fno-pedantic-bottoms
- [ ] try and get a meaningful core -ddump-simpl
- [ ] try lazy, oneShot, inline, noinline
  <https://gitlab.haskell.org/ghc/ghc/-/issues/26127>

## Note \[NOINLINE someNatVal\]

<https://gitlab.haskell.org/ghc/ghc/-/blob/9f614270873135e9a3791085a486b665907a0d07/libraries/ghc-internal/src/GHC/Internal/TypeNats.hs#L114-174>

main GHC bug deconstruction

<https://gitlab.haskell.org/ghc/ghc/-/issues/16586>
<https://gitlab.haskell.org/ghc/ghc/-/issues/19675>

### Note \[Preventing unsafe coercions for singleton types\]

<https://hackage.haskell.org/package/ghc-internal-9.1201.0/docs/src/GHC.Internal.TypeNats.html#natVal>

<https://hackage.haskell.org/package/ghc-internal-9.1201.0/docs/src/GHC.Internal.TypeNats.html#UnsafeSNat>
