---
title: "haskell"
date: 2025-12-23
---

## blogs

[Oleg's gists - Archives](https://oleg.fi/gists/archive.html) [Home :: Reasonably Polymorphic](https://reasonablypolymorphic.com/) [Overcoming Software](https://www.parsonsmatt.md/) [Haskell for all](https://www.haskellforall.com/) [GitHub - quchen/articles: Miscellaneous articles. The readme is the table of …](https://github.com/quchen/articles) [Blog — Monday Morning Haskell](https://mmhaskell.com/blog) [GitHub - pigworker/so-pigworker: being the scrapings of my stackoverflow answers](https://github.com/pigworker/so-pigworker) or <https://personal.cis.strath.ac.uk/conor.mcbride/so-pigworker.pdf> [Chris Penner's FP](https://chrispenner.ca/) [Rebecca Skinner - Home](https://rebeccaskinner.net/) <https://snakamura.github.io/log/> <https://haskellweekly.news/>

## haskell classic papers

[Functional Programming with Bananas, Lenses, Envelopes and Barbed Wire](https://maartenfokkinga.github.io/utwente/mmf91m.pdf) [An introduction to Recursion Schemes](http://blog.sumtypeofway.com/an-introduction-to-recursion-schemes/) [Lazy Functional State Threads](https://www.microsoft.com/en-us/research/wp-content/uploads/1994/06/lazy-functional-state-threads.pdf) [Applicative programming with Effects](http://www.staff.city.ac.uk/~ross/papers/Applicative.pdf) [Scrap Your Constructors: Church Encoding Algebraic Types](http://programmable.computer/posts/church_encoding.html) [Trees That Grow](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/11/trees-that-grow.pdf) [Data types a la carte](http://www.cs.ru.nl/~W.Swierstra/Publications/DataTypesALaCarte.pdf) [The Derivative of a Regular Type is its Type of One-Hole Contexts](http://strictlypositive.md/diff.pdf) [A Very General Method of Computing Shortest Paths](http://r6.ca/blog/20110808T035622Z.html) [Clowns to the Left of me, Jokers to the Right](http://strictlypositive.md/CJ.pdf) [The Essence of the Iterator Pattern](http://www.cs.ox.ac.uk/jeremy.gibbons/publications/iterator.pdf) [There is no Fork: an Abstraction for Efficient, Concurrent, and Concise Data Access](https://simonmar.github.io/bib/papers/haxl-icfp14.pdf) [Haskell Papers Ereader](https://github.com/beerendlauwers/haskell-papers-ereader) <https://github.com/danidiaz/variegated-reading-lists/tree/main/FP-reading-lists>

# haskell classic posts

[zippability - pigworker](http://www.reddit.com/r/haskell/comments/2y25ly/language_deriveapplicative/) [A Neighborhood of Infinity: The Mother of all Monads](http://blog.sigfpe.com/2008/12/mother-of-all-monads.html) [Higher-Kinded Data :: Reasonably Polymorphic](https://reasonablypolymorphic.com/blog/higher-kinded-data/) [Defunctionalize the Continuation](https://www.cis.upenn.edu/~plclub/blog/2020-05-15-Defunctionalize-the-Continuation/) [Parse, don’t validate](https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/) [A break from Programming Languages](https://lexi-lambda.github.io/blog/2025/05/29/a-break-from-programming-languages/)

# Haskell Coursework

- <https://www.cis.upenn.edu/~cis194/spring13/lectures.html>
- [The Essence of the Iterator Pattern](http://www.cs.ox.ac.uk/jeremy.gibbons/publications/iterator.pdf)
- <http://www.cs.ru.nl/~W.Swierstra/Publications/DataTypesALaCarte.pdf>
- <https://blog.sumtypeofway.com/posts/introduction-to-recursion-schemes.html>
- [Functortown: Functor and Bifunctor](https://typeclasses.com/functortown/functor-bifunctor)
- <https://hub.darcs.net/olf/haskell_for_mathematicians>

# code bases

## monoid object

<https://gist.github.com/puffnfresh/f7656fcac42107eebf580e4fe1c72ef9>

## yaya

<https://hackage.haskell.md/package/yaya>

## fresnel

[GitHub - fresnel/fresnel: high-powered optics in a small package](https://github.com/fresnel/fresnel)

## sequoia

[GitHub - robrix/sequoia: focalized sequent calculus proof search](https://github.com/robrix/sequoia)

## lr-acts

<https://github.com/AliceRixte/lr-acts/tree/main>

## goal

[GitHub - alex404/goal: The Geometric OptimizAtion Libraries](https://github.com/alex404/goal)

## orthotope

<https://github.com/augustss/orthotope>

# snippets

[The Algebra of ADTs](https://gist.github.com/gregberns/5e9da0c95a9a8d2b6338afe69310b945) [The evolution of a haskell programmer](http://www.willamette.edu/~fruehr/haskell/evolution.html)

# Language Pragmas

## QuantifiedConstraints

<https://ryanglscott.github.io/2018/02/11/how-to-derive-generic-for-some-gadts/>

## ViewPatterns

<https://stackoverflow.com/questions/43838550/using-viewpatterns-and-patternsynonyms-to-simply-pattern-matches> <https://www.reddit.com/r/haskell/comments/6ylqcg/viewpatterns_and_patternsynonyms_enable_easy/?st=1Z141Z3&sh=9c6ce732>

## TypeFamilies

typefam contains an experiment with using typefamilies & deriving via

<https://gitlab.haskell.md/ghc/ghc/-/issues/16478>

<https://www.reddit.com/r/haskell/comments/5zjwym/when_is_undecidableinstances_okay_to_use/> <https://functor.tokyo/blog/2017-04-07-undecidable-instances>

<https://www.reddit.com/r/haskell/comments/l9kr5f/after_action_report_on_ghc2021/>

## DerivingVia

replacing defaultsignatures

<https://ryanglscott.github.io/papers/deriving-via.pdf>

Instance resolution will match the instance head first before considering the context. Once GHC has commited to an instance, it will never backtrack.

# haskell-juju

## lists

<http://www.imn.htwk-leipzig.de/~waldmann/etc/untutorial/list-or-not-list/>

## string

<https://gist.github.com/dino-/28b09c465c756c44b2c91d777408e166> <https://hasufell.github.io/posts/2024-05-07-ultimate-string-guide.html>

## Not in scope: type variable 'a'

turn on ScopedTypeVariables, and add a forall a.

## utf-8

<https://www.channable.com/tech/so-long-surrogatesa>

## fusion

<https://www.stackbuilders.com/blog/ghc-optimization-and-fusion/> <https://teh.id.au/#/posts/2017/06/30/notes-on-fusion/index.html> <https://jyp.github.io/posts/controlled-fusion.html>

## ghc-core

``` bash
ghc-core --no-asm --no-cast -- -dsuppress-var-kinds -dsuppress-type-applications -dsuppress-uniques Foo.hs
```

## reflection

``` haskell
class (forall a . Functor (p a)) => Profunctor p
```

- <https://ryanglscott.github.io/2018/06/22/quantifiedconstraints-and-the-trouble-with-traversable/>

## linear types

[Learn just enough about linear types](https://tek.brick.do/learn-just-enough-about-linear-types-EGnqyrBDQ5Gk)

## state

[Difference between State, ST, IORef, and MVar](http://stackoverflow.com/questions/5545517/difference-between-state-st-ioref-and-mvar)

``` haskell
newtype State s a = State { run :: s → (a,s) }

instance Functor (State s) where
  fmap f (State s) = State $ first f . s

instance Applicative (State s) where
  pure a = State (a,)
  (State f) <*> (State a) = State $ (\(a',(f',s')) -> (f' a',s')) . second f . a
```

## laziness

[laziness gives us transparent streaming](https://www.reddit.com/r/haskell/comments/j3kbge/comment/g7foelq/?utm_source=share&utm_medium=web2x&context=3)

[Why Functional Programming Matters](https://www.cs.kent.ac.uk/people/staff/dat/miranda/whyfp90.pdf)

coinduction: 'Coinductive' does not mean 'necessarily infinite', it means 'defined by how it is deconstructed'

yield

<https://okmij.md/ftp/continuations/PPYield/yield-pp.pdf>

Lazy evaluation is regarded as one of the main reasons why functional programming matters \[1\]. Lazy evaluation lets us write producers and consumers separately, whereas the two are inextricably intertwined in a call-by-value language. This separation allows a modular style of programming, in which a variety of producers, consumers, and transformers can readily be “plugged together.” Lazy evaluation is also an elegant implementation of a form of coroutines, suspending and resuming computations based on the demand for values, giving us memory-efficient, incremental computation ‘for free’ \[2–4\].

### polysemy comparison

[GitHub - polysemy-research/polysemy: :gemini: higher-order, no-boilerplate mo…](https://github.com/polysemy-research/polysemy#readme)

### Done box bump

[tonyday567/box#10 Newest semigroupoids not accepted](https://github.com/tonyday567/box/issues/10)

- [x] filterE & witherE
- [x] basic explanation in readme
- [x] readme audit & cleanup
- [x] haddock + doctest cleanup

### Done box release

- [x] audit box library

- [x] glue and glue\_ can glue be done without recursion

- [x] nail mergers and splits

- [x] product profunctors <https://github.com/tomjaguarpaw/product-profunctors/blob/faf52f0a134099ed8adb6b29ea822d48920e7ac3/Data/Profunctor/Product.hs#L56>

  [product-profunctors: product-profunctors](https://hackage.haskell.md/package/product-profunctors)

- [x] audit tristero version of box

- [x] fix upstream box usage

  - [x] web-rep
  - [x] iqfeed
  - [x] box-csv
  - [x] box-socket

### box-socket

- compact data types [Compact normal forms + linear types = efficient network communication - Tweag](https://www.tweag.io/blog/2017-08-24-linear-types-packed-data/) [Encode state transitions in types using linear types - Tweag](https://www.tweag.io/blog/2017-08-03-linear-typestates/)
- lineE, lineC, lineSocket =\> Box.Socket (why is it unconnected to box-socket?)
- TCP very different to Socket surface

[free-algebras/TCP.hs at master · coot/free-algebras · GitHub](https://github.com/coot/free-algebras/blob/master/examples/src/Network/TCP.hs)

### function instance of box

``` haskell
fromArrow :: (a -> b) -> Emitter ((->) a) b
fromArrow f = Emitter $ Just . f

fromArrow' :: (a -> b) -> Committer ((->) a) a
fromArrow' f = Committer $ const (const True . f)
```

### Done playback

``` haskell
-- | wait until Stamped time before emitting
emitOn ::
  Emitter IO (LocalTime, a) ->
  Emitter IO a
emitOn =
  witherE
    ( \(l, a) -> do
        sleepUntil (localTimeToUTC utc l)
        pure $ Just a
    )

-- | reset the emitter stamps to by in sync with the current time and adjust the speed
--
-- > let e1 = qList (zipWith (\x a -> Stamped (addUTCTime x t) a) [0..5] [0..5])
playback :: Double -> Emitter IO (LocalTime, a) -> IO (Emitter IO (LocalTime, a))
playback speed e = do
  r <- emit e
  case r of
    Nothing -> pure mempty
    Just (l0, _) -> do
      t0 <- getCurrentTime
      let ua = diffLocalTime (utcToLocalTime utc t0) l0
      let delta u = addLocalTime ua $ addLocalTime (toNominalDiffTime (fromNominalDiffTime (diffLocalTime u l0) * speed)) l0
      pure (witherE (\(l, a) -> pure (Just (delta l, a))) e)

-- | simulate a delay from a (Stamped a) Emitter relative to the first timestamp
simulate :: Double -> Emitter IO (LocalTime, a) -> CoEmitter IO a
simulate speed e = Codensity $ \eaction -> do
  e' <- playback speed e
  eaction (emitOn e')
```

### Done box-socket refactor

<https://github.com/coot/free-algebras/blob/master/examples/src/Network/TCP.hs>

[Using websockets with scotty haskell · GitHub](https://gist.github.com/andrevdm/9560b5e31933391694811bf22e25c312)

- [x] box-socket-0.5.1.0

- [x] web-rep-0.12.0.0

- [x] iqfeed

- [x] debug TCP

- [x] Box.Types -\> Box.Socket.Types, Box.Socket -\> Box.Websocket

- [x] IqFeed.Socket

  [Faster Coroutine Pipelines: A Reconstruction](https://rubenpieters.github.io/assets/papers/JFP20-pipes.pdf)

## inlinable

<https://www.reddit.com/r/haskell/comments/cjkc3l/should_i_be_inlining_instance_implementations/>

## delimited continuations

delimited continuation developments: [Lysxia - From delimited continuations to algebraic effects in Haskell](https://blog.poisson.chat/posts/2023-01-02-del-cont-examples.html)

## codata

<https://reasonablypolymorphic.com/blog/review-codata/>

These are all the same thing:

data vs codata finite vs infinite induction vs coinduction least fixed point vs greatest fixed point initial F-algebras vs terminal F-coalgebras construction vs deconstruction

``` haskell-ng
data Bool where
  True :: Bool
  False :: Bool

class CoBool where
  bool :: a -> a -> Bool -> a
  bool t _ False = t
  bool _ t True = t

data Tree t where
  Leaf :: t -> Tree t
  Branch :: Tree t -> Tree t -> Tree t

walk :: Tree t -> (t -> a) -> (a -> a -> a) -> a
walk (Leaf t) mk _ = mk t
walk (Branch l r) mk comb = comb (walk l mk comb) (walk r mk comb)

class CoTree where
  cowalk :: Tree t -> (t -> a) -> (a -> a -> a) -> a
  cowalk (Leaf t) mk _ = mk t
  cowalk (Branch l r) mk comb = comb (walk l mk comb) (walk r mk comb)


data Stream a = a :> Stream a deriving (Functor, Eq, Ord)

ones :: (Multiplicative a) => Stream a
ones = one :> ones

data CoStream a = CoStream {
  head :: a,
  tail :: CoStream a}

coOnes :: Multiplicative a => CoStream a
coOnes = CoStream one coOnes
```

[Deriving Dependently-Typed OOP from First Principles](https://dl.acm.md/doi/pdf/10.1145/3649846)

This covers:

- The expression problem

  Either new producers or consumers are easy to extend, but not both. OOP new producers are easy FP new consumers are easy

- The principal of duality

- start with (co)data and derive its dual fragment by the systematic use of functionalization and defunctionalization.

## scrap your typeclasses

<https://www.haskellforall.com/2012/05/scrap-your-type-classes.html>

<https://h2.jaguarpaw.co.uk/posts/simplifying-typeclasses/>

Reduce the power of typeclasses down to:

Defining a typeclass

``` haskell-ng
class Eq a
```

Defining superclasses of typeclasses

``` haskell-ng
class Eq a => Ord a
```

Defining instances of typeclasses

``` haskell-ng
instance Eq Int
```
