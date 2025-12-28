+++
title = "harpie - call for contributors"
author = "Tony Day"
date = 2025-07-31
draft = true
tags = ["haskell","harpie"]
+++

harpie is an old project looking at fresh ideas. It began life as numhask, a play on numpy, as part of a data science effort in the Haskell community. The project split into numhask (what if we had a sensible numerical tower) and array programming (what is the ideal API for polymorphic arrays), into numhask-array and then as harpie.

The project is mostly a hand-crafted API into hyper-rectangular array programming. A recent refactor took on uiua.org as a modern take on an old corner of programming, and has been used as an API reference, coverage test and as a modern usage guide.


'exists.' does not exist
===

The core idea of the library is to utilise vector as a computational stream with control of index record keeping. An ideal type for this would be:

```
(r :: Nat, s :: Array '[r] Nat, Array s a)
```

where:

- r is the array rank, and
- s is the shape of the array

but Haskell can't represent this really without a sigma type. Since the non-existence of `exists.` precludes a consistent algorithmic approach across arrays and modelling array dimensions, we compromise with:

``` haskell
newtype Array (ds :: [Nat]) a
```

representing shape as a Nat list and rank as the shape list length.

This ideal, one that can be seen in usage in uiua design, leads towards algorithms for array element manipulation, for array shape operations, for index computations and rank operators to be exactly the same things, polymorphic across these concerns.

Por que no los dos?
===

To bring back the spirit of algorithmic polymorphism, the library provides an identical interface between list algorithms at value and type level.

At the type-level, https://hackage.haskell.org/package/first-class-families is the workhorse for getting value-level and type-level code to line up conceptually, via defunctionalization.

In terms of the Haskell language, the difference between type-level and value-level coding can be sensed in comparing implementations across Harpie.Array and Harpie.Fixed, which are identical APIs.

The most obvious problematic with type-level coding is that it cannot be encapsulated, really, and (potentially vast) constraint lists need to be propagated to every use site.

Given that we want to use a Vector as the array container, but have no Vector operators at type level, this design forces a separation between array processing algorithms (with the vector library under the hood), and shape algorithms (type-level Nat lists under the hood).

Singleton [Nat]
---

Fixed-shape vector libraries in Haskell often lean into peano numbers as inductive-style types that are conducive to ease of proofs at the type-level, defining a Nat as:

```
data Nat = Zero | Succ Nat
```

So that append can be defined as:

```
append :: Vector n a -> a -> Vector (Succ n) a

```

To quote the google ai:

> In Haskell, typenats is often associated with GHC.TypeLits, which allows you to represent natural numbers at the type level using Peano numerals. Peano numbers are a way to represent natural numbers using only Zero and a successor function (like Succ). This approach allows for type-level arithmetic and manipulation of numbers within the type system. ... Oops, something went wrong.

Cool, but GHC.TypeLits and GHC.TypeNats do not use Peano numbers at all. They use:

```


```










