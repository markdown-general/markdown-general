---
title: "theory"
date: 2025-12-23
---

# theory

## Economics

- [ ] audit

## functional economics

- [ ] steve keene modelling
- [ ] mmt textbook
- [ ] <https://www.aeaweb.md/aea/2019conference/program/pdf/14020_paper_etZgfbDr.pdf>
- [ ] <https://modernmoneynetwork.md/sites/default/files/biblio/Pavlina_2007.pdf>
- [ ] fe

### Solow

<https://www3.nd.edu/~esims1/solow_model.pdf>

data Economy = Economy { firms :: \[Firm\], households :: \[Household\] }

produce :: Firm -\> Capital -\> Labor -\> Output

data Household = Household { capital :: Capital, labor :: Labor }

rent :: Household -\> Firm -\> Capital -\> Labor -\> Rent

Output, Labor is use it or lose it.

Capital is produced

data Firm = Firm { prodf :: Capital -\> Labor -\> Output }

prodf varies by firm and over time, is bi-monotonic

defaultFirm alpha = Firm (cobbDouglas alpha)

cobbDouglas :: Double -\> Capital -\> Labor -\> Output cobbDouglas alpha k n = k ^ alpha \* n (1 - alpha)

data Env = Env (Capital -\> Labor -\> Output)

profit :: Env -\> Firm -\> Capital -\> Labor -\> Output profit env firm c l = env c l \* (prodf firm c l) - c - l

## billyblog

<http://bilbo.economicoutlook.net/blog/>

[Understanding central bank operations – Bill Mitchell – Modern Monetary Theory](http://bilbo.economicoutlook.net/blog/?p=9392)

Look up the search terms below and find material.

- [ ] replace the budget constraint with an inflation constraint
- [ ] assessing underutilisation is critical to understanding inflation risks
- [ ] deal with inflation ex-ante
- [ ] a job guarantee increases flexibility to tax anti-politic and raise job quality (versus job scarcity effects)
- [ ] taxes offset demand rather than raise revenue
- [ ] deficit financing <http://neweconomicperspectives.md/2013/12/krugman-helicopters-consolidation.html>

### national accounts

aka flow of funds

<http://bilbo.economicoutlook.net/blog/?p=43652>

<http://bilbo.economicoutlook.net/blog/?p=14153>

## Lerner

<https://www.gc.cuny.edu/CUNY_GC/media/LISCenter/pkrugman/lerner-function-finance.pdf>

## MMT reading

<http://neweconomicperspectives.md/2018/01/answers-from-the-mmters.html?fbclid=IwAR2wJkTT4qmEWEZj1OSpgintISFXpq0zAh9MzEe3MbE4xR5ywl4JvLN-6Vs>

<https://americanaffairsjournal.md/2017/08/make-left-great/>

<https://www.ineteconomics.md/perspectives/blog/growth-with-decarbonization-is-not-an-oxymoron>

<https://niskanencenter.md/wp-content/uploads/2018/04/Final_Free-Market-Welfare-State.pdf>

<https://qz.com/88781/after-crunching-reinhart-and-rogoffs-data-weve-concluded-that-high-debt-does-not-cause-low-growth/>

<https://actuaries.asn.au/Library/FSF10_Paper_Frank%20Ashe.pdf>

<http://neweconomicperspectives.md/2018/10/modern-money-theory-how-i-came-to-mmt-and-what-i-include-in-mmt.html?subscribe=already#blog_subscription-2>

<https://pubs.aeaweb.md/doi/pdfplus/10.1257/jep.31.3.215>

<http://neweconomicperspectives.md/2018/10/modern-money-theory-how-i-came-to-mmt-and-what-i-include-in-mmt.html?subscribe=already#blog_subscription-2>

[Nathan Tankus - The Green New Deal must be about public spending …](https://twitter.com/NathanTankus/status/1176256369344663553)

## One-liners

- money is not scarce
- public sector deficit = private sector surplus
- society has social problems that are not profitable.
- there is no reason to think that the private sector would or should employ everyone who wants to work.
- Lending creates its own deposits.
- The banking system doesn't need to borrow to increase credit.
- The sovereign buying goods creates money, taxes destroy it.
- A sovereign doesn't have to finance spending.
- Government borrowing is a service to the banking industry.
- Functional Finance: finance should be “functional” (to achieve the public purpose), not “sound” (to achieve some arbitrary “balance” between spending and revenues).

## economic schools

Neoclassical = money printing is not ok, and monetary stimulus is better than fiscal stimulus Keynesian = fiscal stimulus is better than monetary stimulus Market Monetarist = money printing is ok MMT = money printing is ok + fiscal stimulus is better than monetary stimulus

Examples:

Monetarist: <https://www.lesswrong.com/posts/tAThqgpJwSueqhvKM/frequently-asked-questions-for-central-banks-undershooting>

Monetary Finance: financing deficits via expansion of the monetary base (aka printing money)

- <https://www.imf.md/external/np/res/seminars/2015/arc/pdf/adair.pdf>

Quantitative Theory of Money

- <https://www.themoneyillusion.com/where-mmt-went-wrong>

## dynamic efficiency

We are over-shooting resource consumption by a very long way.

<https://en.wikipedia.md/wiki/Dynamic_efficiency>

### <http://www.sfu.ca/~poitras/jep_piketty_15.pdf>

### <https://mpra.ub.uni-muenchen.de/60221/>

### <http://piketty.pse.ens.fr/files/Geerolf13.pdf>

### <http://www.rba.gov.au/publications/smp/2017/may/pdf/statement-on-monetary-policy-2017-05.pdf>

### <https://en.wikipedia.md/wiki/Natural_resource_economics>

# Systems Theory

<http://donellameadows.md/wp-content/userfiles/Leverage_Points.pdf>

## Places to Intervene (increasing power)

- Constants, parameteres, numbers
- Negative feedback loops
- Positive feedback loops
- material flows and intersection nodes
- information flows
- The rules of the system (incentives, punishments, constraints)
- The distribution of power over the rules of the system
- The goals of the system
- The mindset or paradigm out of which the system - its goals, power structure, rules, its culture - arises.
- abandoning the paradigm

### <http://productiongap.md/wp-content/uploads/2019/11/Production-Gap-Report-2019.pdf>

## meadows

<https://wtf.tw/ref/meadows.pdf>

<http://donellameadows.md/wp-content/userfiles/Leverage_Points.pdf>

survival, resilience, differentiation, evolution are system-level goals

In the end, it seems that power has less to do with pushing leverage points than it does with strategically, profoundly, madly letting go.

## biology

<https://sci-hub.wf/10.1007/s11692-015-9323-x>

## Category Theory

## Next new CT reading list

<https://www.math3ma.com/blog/what-is-category-theory-anyway>

## Applied Category Theory

[nLab - category theory](http://nlab-pages.s3.us-east-2.amazonaws.com/nlab/show/category+theory)

> Schapira mentions the influence of Grothendieck, but he also writes that “the appearance of categories is… more or less concomitant with the outbreak of structuralism in the human sciences. ~ <https://webusers.imj-prg.fr/~michael.harris/MPRT.pdf>

The structure is defined by the arrows pointing to the object. What about the arrows pointing away from the object?

## <https://www.math3ma.com/categories/algebra>

## basics

- An object is determined by its relationships with other objects.

  objects: sets, groups, spaces relationships: functions, group homomorphisms, arrows

- A category is a set of objects and arrows

- Functors are (structure-preserving) maps between categories

## ACT coursework placemark

- [ ] read through 7 sketches

[Limits and Colimits, Part 1 (Introduction)](https://www.math3ma.com/blog/limits-and-colimits-part-1)

Limits are a way to construct a new object from an existing collection of objects.

Colimits are:

A category is a set of objects

[What is Category Theory Anyway?](https://www.math3ma.com/blog/what-is-category-theory-anyway)

<https://math.mit.edu/~dspivak/teaching/sp18/7Sketches.pdf>

<https://www.math3ma.com/categories/category-theory>

## ACT coursework <span class="tag" tag-name="reading"><span class="smallcaps">reading</span></span>

### John Baez

[Applied Category Theory Course in The Azimuth Project](https://www.azimuthproject.md/azimuth/show/Applied+Category+Theory+Course)

### 7 Sketches <span class="tag" tag-name="reading"><span class="smallcaps">reading</span></span>

structures and coherence

<https://math.mit.edu/~dspivak/teaching/sp18/7Sketches.pdf>

### Bartosz Milewski

[Bartosz Milewski - YouTube](https://www.youtube.com/channel/UC8BtBl8PNgd3vWKtm2yJ7aA) [Optics for the Working Mathematician \|   Bartosz Milewski's Programming Cafe](https://bartoszmilewski.com/2021/09/08/optics-for-the-working-mathematician/) <https://github.com/BartoszMilewski/Publications/blob/master/TheDaoOfFP/4-SumTypes.pdf> [Bartosz: Category theory for programmers](http://bartoszmilewski.com/2014/10/28/category-theory-for-programmers-the-preface/) [category-theory-for-programmers-challenges/101-Category<sub>The</sub>\_Essence<sub>of</sub>\_Compos…](https://github.com/awalterschulze/category-theory-for-programmers-challenges/blob/main/101-Category_The_Essence_of_Composition.md)

Composition: putting blocks together Identity: When are things the same (Structure preservation) Abstraction: hiding detail

But how do we chop things up in the first place?

### Emily

<https://math.jhu.edu/~eriehl/context.pdf>

### math3ma

[Category Theory on Math3ma](https://www.math3ma.com/categories/category-theory)

### classics <span class="tag" tag-name="reading"><span class="smallcaps">reading</span></span>

[Maclane](http://www.mtm.ufsc.br/~ebatista/2016-2/maclanecat.pdf) [Kan - Adjoint Functors](https://www.ams.md/journals/tran/1958-087-02/S0002-9947-1958-0131451-0/S0002-9947-1958-0131451-0.pdf) [Gibbons - Calculating functional programming](http://www.cs.ox.ac.uk/people/jeremy.gibbons/publications/acmmpc-calcfp.pdf) [All kinds of lattices](https://oleg.fi/pdfs/Monotone.pdf)

### blogs

[Adventures in Category Theory - The algebra of types](https://miklos-martin.github.io/learn/fp/category-theory/2018/02/01/adventures-in-category-theory-the-algebra-of-types.html)

## conceptual taxonomy

variegated

[danidiaz/variegated-read](https://github.com/danidiaz/variegated-reading-lists/tree/main/FP-reading-lists)

## Yoneda

The self does not undergo modifications, it is itself a modification ~ Gilles Deleuze

> the basis of standard techniques in topology (classifying spaces) and algebraic geometry (moduli spaces) ~ <https://webusers.imj-prg.fr/~michael.harris/MPRT.pdf>

- Using Yoneda: [Haskell: example of function of type a -\> a, besides the identity - Stack Ove…](https://stackoverflow.com/questions/12230820/haskell-example-of-function-of-type-a-a-besides-the-identity/12230918#12230918)

### `(forall b . (a -> b) -> f b) ~ f a`

`(<$> xs)`

The map is the territory …

An object is completely specified by it's relationaships.

\`forall b. (a -\> b) -\> b `=` a\` is an instance of the Yoneda Lemma

[haskell - Is having a \`(a -\> b) -\> b\` equivalent to having an \`a\`? - Stack Ov…](https://stackoverflow.com/questions/45287954/is-having-a-a-b-b-equivalent-to-having-an-a)

GADTs are nothing more than the Yoneda lemma in disguise.

``` haskell
(forall b . (a -> b) -> f b) ~ f a
```

``` haskell
fw :: (Functor f) => (forall b . (a -> b) -> f b) -> f a
fw f = f id

bw :: (Functor f) => f a -> (forall b . (a -> b) -> f b)
bw x f = fmap f x
```

``` haskell
fw . bw ==>
  \x -> fw (bw x) ==>
  \x -> fw ((\x' f -> bw f x') x) ==>
  \x -> fw (\f -> bw f x)
  \x -> (\f -> bw f x) id
  \x -> bw id x
  \x -> fmap x id
  \x -> fmap x (\x' -> x')
```

``` haskell
bw . fw ==>
  bw (\f -> fw f) ==>
  \x -> bw (x id) ==>
  \x -> fmap fw (\f -> bw f x)
  \x -> (\f -> bw f x) id
  \x -> bw id x
  \x -> fmap x id
  \x -> fmap x (\x' -> x')
```

[Haskell for all: GADTs](https://www.haskellforall.com/2012/06/gadts.html)

[A Neighborhood of Infinity: Reverse Engineering Machines with the Yoneda Lemma](http://blog.sigfpe.com/2006/11/yoneda-lemma.html)

extracting the f a from the machine

``` haskell
uncheck :: (forall b . (a -> b) -> f b) -> f a
uncheck t = t id
```

emulating the machine

``` haskell
check :: (Functor f) => f a -> (forall b . (a -> b) -> f b)
check a f = fmap f a
```

The Yoneda Lemma without category theory

<http://boole.stanford.edu/pub/yon.pdf>

<https://stackoverflow.com/questions/68670779/coyoneda-hasnt-a-higher-rank-type-but-what-type-has-it-actually>

<https://bartoszmilewski.com/2021/04/01/traversals-2-profunctors-and-tambara-modules/>

### Yoneda Perspective

[The Yoneda Perspective](https://www.math3ma.com/blog/the-yoneda-perspective)

- the Yoneda lemma implies all vantage points give all information.
- mathematical objects are completely determined by their relationships to other objects.
- the properties of a mathematical object are more important than its definition: properties are defining characteristics

[Yoneda Intuition from Humble Beginnings · GitHub](https://gist.github.com/Icelandjack/02069708bc75f4284ac625cd0e2ec81f)

## enriched categories

[enriched categories](https://en.wikipedia.md/wiki/Enriched_category)

the hom-set of a category often has additional structure that should be respected

- what are concrete enriched categories?

[Locally Cartesian Closed Categories ~ Xu](https://arxiv.md/pdf/2202.04543.pdf)

## adjunction

[A story of an arrow and a comma - Murat Kasimov](https://iokasimov.github.io/posts/2020/10/arrow-and-comma)

[Kan - Adjoint Functors](https://www.ams.md/journals/tran/1958-087-02/S0002-9947-1958-0131451-0/S0002-9947-1958-0131451-0.pdf)

<https://www.math.harvard.edu/media/lehner.pdf>

[Kan - Adjoint Functors](https://www.ams.md/journals/tran/1958-087-02/S0002-9947-1958-0131451-0/S0002-9947-1958-0131451-0.pdf)

### Free ⊣ Forget

[Adjunctions and Battleship](https://chrispenner.ca/posts/adjunction-battleship) [Free and Forgetful Functors](https://chrispenner.ca/posts/free-forgetful-functors)

<https://www.haskellforall.com/2016/02/from-mathematics-to-map-reduce.html>

f is left adjoint to g means this:

``` haskell
-- f a -> b ≅ a -> g b
-- >>> fw . bw = id
-- >>> bw . fw = id
fw :: (f a -> b) -> (a -> g b)
bw :: (a -> g b) -> (f a -> b)
```

### Exists ⊣ Const ⊣ Forall

Existential quantification:

``` haskell
f x -> a
(∃x. f x) -> a
```

Universal quantification:

``` haskell
a ->      f x
a -> (∀x. f x)
```

natural transformation

``` haskell
type (~>) :: forall k. (k -> Type) -> (k -> Type) -> Type
type f ~> g = (forall x. f x -> g x)
```

``` haskell
type Exists :: forall k. (k -> Type) -> Type
data Exists f where
  Exists :: f __ -> Exists @k f
```

<https://www.reddit.com/r/haskell/comments/jdjdnl/universals_to_the_right_existentials_to_the_left/>

### Notions of Computations as Monoids

[Notions of Computation as Monoids](https://www.fceia.unr.edu.ar/~mauro/pubs/Notions_of_Computation_as_Monoids.pdf)

[iceland<sub>jack</sub>](functorof.md::*iceland_jack)

### Essence of AD

The simple essence of automatic differentiation - Conal

[Essence of AD](https://arxiv.md/pdf/1804.00746.pdf)

### flip dot

<https://twitter.com/rob_rix/status/1408191313590112259>

### differentiation as a functor

The dual of addition is duplication (CPS). This is why there is addition in differentiation.

<https://mobile.twitter.com/MotivicKyle/status/1477350772476022787> chain rule

(a -\> b) -\> (a -\> (a -o b))

-o linear map

local linear approximation

f a + D f a e affine approximates f (a + e) for small e

Therefore, the derivative of a linear function is itself, everywhere.

D id a = id D exl a = exl D exr a = exr

chain rule

D (g . f) = D g (f a) . D f a

D (f fork g) a = D f a fork D g a

D' :: (a -\> b) -\> (a -\> (b,(a -o b)))

### Three

<https://iokasimov.github.io/posts/2020/10/arrow-and-comma>

<https://stackoverflow.com/questions/56726854/do-monad-transformers-generally-speaking-arise-out-of-adjunctions>

What you've described is one way that you can build a monad from "a monad and a pointed functor", which is by using a distributive law.

Mark P Jones and Luc Duponcheel Composing Monads

MaybeT and ReaderT are monad transformers that are built this way. MaybeT pushes the Maybe inside, while ReaderT pulls the function from the environment outside. Similarly WriterT pushes a product inside.

Then you get stuck. For example, StateT doesn't fit this model.

s -\> m (a, s) is not a composition of two simpler monads. (It almost looks like a composition of three of them, but the (,s) there isn't actually writer, it has no monoid.) (Update monads actually are the 3-way composition that State at first glance appears to be.)

What is happening there is that (, s) is "adjoint" to (-\>) s, and so if you sandwich a monad in the middle the result is also a monad. Cont is also based on an adjunction, between (-\> r) and (-\> r), but ContT is built differently.

Finally, Mark also covers a different sort of 'distributive' law, an absorption law where m.n.m embeds into m.n or m.n.m embeds into n.m.

The above list isn't meant to be exhaustive just to indicate that the monad transformer story has some interesting twists and turns.

Generalising monads to arrows ~ hughes

<https://www.cs.tufts.edu/~nr/drop/arrows.pdf>

### monad transformers

<https://stackoverflow.com/questions/24515876/is-there-a-monad-that-doesnt-have-a-corresponding-monad-transformer-except-io>

<https://stackoverflow.com/questions/49322276/adjoint-functors-determine-monad-transformers-but-wheres-lift>

### free monoid construction

<https://www.haskellforall.com/2016/02/from-mathematics-to-map-reduce.html>

f is left adjoint to g means this:

``` haskell
-- f a -> b ≅ a -> g b
-- >>> fw . bw = id
-- >>> bw . fw = id
fw :: (f a -> b) -> (a -> g b)
bw :: (a -> g b) -> (f a -> b)
```

## colimits

### semiotics

<https://gjoncas.github.io/posts/2020-12-26-algebraic-semiotics.html>

## monad

A Monad is a Monoid in the Category of Endofunctors.

### Relative Monads

pigworker example

<https://www.reddit.com/r/haskell/comments/2q5bdb/monads_need_not_be_endofunctors_we_introduce_a/cn36502/?utm_source=reddit&utm_medium=web2x&context=3>

<https://arxiv.md/pdf/1412.7148.pdf>

### Notions of Computation as Monoids

<https://arxiv.md/pdf/1406.4823.pdf>

Composing monads <http://web.cecs.pdx.edu/~mpj/pubs/RR-1004.pdf>

### Free Applicative

<https://oleg.fi/gists/posts/2018-02-21-single-free.html>

<https://www.fceia.unr.edu.ar/~mauro/pubs/Notions_of_Computation_as_Monoids_ext.pdf>

<https://bartoszmilewski.com/2018/02/17/free-monoidal-functors/>

### Free Monads

[What are Free Monads? (Stack Overflow)](http://stackoverflow.com/questions/13352205/what-are-free-monads)

### arrow

arrows like monads are monoids <https://www.pure.ed.ac.uk/ws/portalfiles/portal/22099462/http//www.haskell.md/arrow.pdf>

a monad is 'a monoid in the category of endofunctors', an arrow is 'a monoid in the category of profunctors'

## profunctor

### profuctor optics

[Profunctor Optics](http://www.cs.ox.ac.uk/people/jeremy.gibbons/publications/poptics.pdf)

<https://github.com/hablapps/DontFearTheProfunctorOptics>

<https://github.com/ChrisPenner/proton>

### arrows

<https://stackoverflow.com/questions/38169453/whats-the-relationship-between-profunctors-and-arrows>

### lens

[artyom: lens over tea](http://artyom.me/lens-over-tea-1)

### Category and Profunctor types

- categories and profunctors have the same shape \`k a b\`
- [Deconstructing Lambdas—An Awkward Guide to Programming Without Functions - Yo…](https://www.youtube.com/watch?v=xZmPuz9m2t0)

1.  Cartesian

    - Cartesian is not a Profunctor concept (only non-crossover between categories and profunctors)

    ``` haskell
    class Category k => Cartesian k where
      copy :: k a (a, a)
      consume :: k a ()
      fst' :: k (l,r) l
      snd' :: k (l,r) r
    ```

2.  Strong

    (also called Cartesian in profunctor libraries)

    ``` haskell
    class Category k => Strong k where
      first' :: k a b -> k (a,c) (b,c)
      second' :: k a b -> k (c,a) (c,b)
    ```

    Costrong is related to fix

    ``` haskell
    class Category k => Costrong k where
      unfirst' :: k (a,c) (b,c) -> k a b
      unsecond' :: k (c,a) (c,b) -> k a b
    ```

3.  Choice

    ``` haskell
    class Category k => Choice k where
      left' :: p a b -> p (Either a c) (Either b c)
      right' :: p a b -> p (Either c a) (Either c b)
    ```

    CoChoice is related to recursion

    ``` haskell
    unleft :: p (Either a d) (Either b d) -> p a b
    ```

4.  Closed

    ``` haskell
    class Category k => Closed p where
      closed :: p a b -> p (x -> a) (x -> b)
    ```

5.  Monoidal

    <https://oleg.fi/gists/posts/2017-10-05-monoidal-vs-traversing.html>

## Poly

[Polynomial Functors Course](https://topos.site/poly-course/)

<https://topos.site/poly-book.pdf>

## inference

deduction, induction & abduction:

[peirce](https://ncatlab.md/davidcorfield/files/Peirce200225.pdf)

<https://x.com/locallycompact/status/1943767542536769969>

h = g . f

composition (deduction)

\_ = g . f

Kan extension (induction)

h = g . \_ \_ \>\>\> g = h

lifting (abduction)

h = \_ . f f \>\>\> \_ = h

# Algebra

## boom

[The Boom Hierarchy - Alexander Bunkenburg](http://citeseerx.ist.psu.edu/viewdoc/download;jsessionid=601FB55680BBC2C1A14D136657E4A7ED?doi=10.1.1.49.3252&rep=rep1&type=pdf)

## linear algebra

[Matrix multiplication as composition \| Chapter 4, Essence of linear algebra -…](https://www.youtube.com/watch?v=XkY2DOUCWMU)

## f-algebra

<https://bartoszmilewski.com/2014/09/29/how-to-get-enriched-over-magmas-and-monoids/>

<https://bartoszmilewski.com/2013/06/10/understanding-f-algebras/>

``` haskell
-- * basic F-Algebra
type Algebra f a = f a -> a

newtype Fix f = Fix (f (Fix f))

unFix :: Fix f -> f (Fix f)
unFix (Fix x) = x

cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg = alg . fmap (cata alg) . unFix

```

## Coalgebra explanation

[scala - What does "coalgebra" mean in the context of programming? - Stack Ove…](https://stackoverflow.com/questions/16015020/what-does-coalgebra-mean-in-the-context-of-programming?rq=1)

## starsemiring

<https://byorgey.wordpress.com/2016/04/05/the-network-reliability-problem-and-star-semirings/>

<http://r6.ca/blog/20110808T035622Z.html>

<https://xinitrc.de/blog/2014/02/09/Sucker-for-generality.html>

# Algorithms

<https://twitter.com/sigfpe/status/1531371151737225216>

## MCMC, HMC, NUTS

Hamiltonian Monte Carlo No U-Turn

- HMC uses gradients to improve MCMC mixing
- NUTS is an extension of HMC that adaptively tunes M and ε during burn-in, and adapts L throughout the MCMC run.

## propagators as a reasoning framework

propagation, a computational model built on the engineering idea that the basic computational elements are autonomous machines interconnected by shared cells through which they communicate

<https://courses.csail.mit.edu/6.803/pdf/sussman-white.pdf>

## egg-style equality saturation

term rewriting [{POPL 2021} egg: Fast and Extensible Equality Saturation (full) - YouTube](https://www.youtube.com/watch?v=6cJMI9z2TeU)

## Linear programming with a totally unimodular constraint matrix

[Totally Unimodular Matrices in Linear Programming - Nate Veldt - YouTube](https://www.youtube.com/watch?v=Fmjy74c-R-I)

## general optimization over integral polytopes

<https://arxiv.md/pdf/2102.09994.pdf>

## SMT solvers

[Satisfiability modulo theories - Wikipedia](https://en.wikipedia.md/wiki/Satisfiability_modulo_theories)

## succinct permutations/functions

## dynamic programming

Dijkstra's algorithm for the shortest path problem

## neural networks

<https://bartoszmilewski.com/2024/03/22/neural-networks-pre-lenses-and-triple-tambara-modules/>

## backpropagation and learning

[Backpropogation is Just Steepest Descent with Automatic Differentiation \| Mat…](https://idontgetoutmuch.wordpress.com/2013/10/13/backpropogation-is-just-steepest-descent-with-automatic-differentiation-2/)

[Neural Networks and Automated Differentiation \| Maths, Stats & Functional Pro…](https://idontgetoutmuch.wordpress.com/2013/05/31/neural-networks-and-automated-differentiation-3/)

[Backprop as Functor: A compositional perspective on supervised learning](https://arxiv.md/abs/1711.10455)

## backprop library

[backprop - Home](https://backprop.jle.im/)

[backprop - Manipulating BVars](https://backprop.jle.im/03-manipulating-bvars.html)

## Loopless

[<file:~/org/Papers/Loopless_Functional_Algorithms.pdf>](Papers/Loopless_Functional_Algorithms.pdf)

## Heterogeneous Rose Tree

``` haskell
-- | heterogeneous rose tree
data HTree a b = HTree a [HTree a b] | HLeaf b

toTree :: HTree a b -> Tree (Either a b)
toTree (HTree l xs) = Node (Left l) (fmap toTree xs)
toTree (HLeaf r) = Node (Right r) []

fromTree :: Tree (Either a b) -> HTree a b
fromTree (Node (Right r) []) = HLeaf r
fromTree (Node (Left l) xs) = HTree l (fmap fromTree xs)
fromtree (Node (Right _) _) = error "Leaf with Children"

type Element = HTree Tag ByteString
```

# Coding Theory

## codensity

[continuations-and-reduction-semantics.md · GitHub](https://gist.github.com/lexi-lambda/d97b8187a9b63619af29689e9fa1b880)

- Codensity (Free f)

### delimited continuations

<http://parametricity.net/dropbox/yield.subc.pdf> <https://www.reddit.com/r/haskell/comments/oj5br4/any_good_uses_for_logict_extended_with_shift_and/> <http://comonad.com/reader/2011/free-monads-for-less-3/>

### LogicT

<https://okmij.md/ftp/papers/LogicT.pdf>

### ContT

<https://stackoverflow.com/questions/3322540/how-and-why-does-the-haskell-cont-monad-work> "Suspended computation"

<https://okmij.md/ftp/continuations/#tutorial>

## convolution

What is:

- cauchy product
- day convolution

[convolution - conal](https://arxiv.md/pdf/1903.10677.pdf)

[Convolutions and Semirings - Donnacha Oisín Kidney](https://doisinkidney.com/posts/2017-10-13-convolutions-and-semirings.html)

``` haskell
convolve :: [a] -> [b] -> [[(a,b)]]
convolve xs ys = foldr f [] xs
  where
    f x zs = foldr (g x) id ys ([] : zs)
    g x y a (z:zs) = ((x, y) : z) : a zs
    g x y a [] = [(x, y)] : a []
```

[FUNCTIONAL PEARLS Power Series, Power Serious](https://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.333.3156&rep=rep1&type=pdf)

<https://doisinkidney.com/posts/2017-10-13-convolutions-and-semirings.html>

<https://byorgey.wordpress.com/2008/04/22/list-convolutions/>

<https://arxiv.md/pdf/1903.10677.pdf>

[Generalized Convolution and Efficient Language Recognition](https://arxiv.md/pdf/1903.10677.pdf)

<https://ruor.uottawa.ca/bitstream/10393/36118/3/Khan_Sakif_2017_thesis.pdf>

## quotient

Good explanation of quotient.

[Universal Properties – Math ∩ Programming](https://jeremykun.com/2013/05/24/universal-properties/)

- To 'quotient' something is to decompose it?
- Is to quotient something, to apply a forgetful functor?

[What are quotients?](http://www.cs.nott.ac.uk/~psztxa/publ/defquotients.pdf)[ Good undergrad explanation of quotienting](https://web.maths.unsw.edu.au/~danielch/thesis/zac_murphy.pdf)

> A -\> () is an isomorphism"
>
> In many cases, such as in toposes, the initial object is strict, so that any morphism 𝐴→0 is an isomorphism. What this means is "you can't quotient something to get nothing

## Recursion

> GHC is not very good at recursion, hence fusion involves converting functions on lists to a representation not involving recursion.

### Recursion reading list

<https://www.janis-voigtlaender.eu/papers/AsymptoticImprovementOfComputationsOverFreeMonads.pdf> [Reflection without Remorse](https://okmij.md/ftp/Haskell/zseq.pdf) [when to use CPS vs codensity vs reflection without remorse in Haskell - Stack…](https://stackoverflow.com/questions/45334985/when-to-use-cps-vs-codensity-vs-reflection-without-remorse-in-haskell)

### folds on lists

> the fact that list is such a prevalent data structure in Haskell is really a first-order reflection of the fact that fold is such a fundamental operation in functional programming in general. ~ [Building up to a Point via Adjunctions - School of Haskell \| School of Haskell](https://www.schoolofhaskell.com/user/gbaz/building-up-to-a-point-via-adjunctions)

1.  tl;dr

    lists and folds form a duality.

    ``` haskell
    toList :: forall a. (Foldable t) => t a -> [a]
    toList = foldr (:) []
    ```

2.  Q. What is a list in Haskell?

    1.  Answer 1: A list is a recursive data type

        A list, as commonly understood in Haskell, and as is most things, is a data type defined in [GHC.Types](https://hackage.haskell.md/package/ghc-prim-0.8.0/docs/src/GHC.Types.html), with extra syntactic sugar.

        ``` haskell
        module GHC.Types (
                -- Data types that are built-in syntax
                -- They are defined here, but not explicitly exported
                --
                --    Lists:          []( [], (:) )
                --    Type equality:  (~)( Eq# )
        data [] a = [] | a : [a]
        ```

        Built-in, syntactic sugar:

        ``` haskell
        [a] ==> [] a
        [1,2,3] ==> 1:2:3:[]
        ```

    2.  Answer 2: A list is the Free Monoid object.

        A Binary Tree

        ``` haskell
        data Tree a
          = Leaf a
          | Branch (Tree a) (Tree a)
        ```

        Graft on a unit

        ``` haskell
        data Tree a
          = EmptyTree
| Leaf a
||
          | Branch (Tree a) (Tree a)        ```

        Make the Tree associative:

        - (Leaf a \`Branch\` Leaf b) \`Branch\` Leaf c and Leaf a \`Branch\` (Leaf b \`Branch\` Leaf c) should be isomorphic (represented in the same way).

        - To 'forget' the left/right structure of the tree we force the left side of the branch to be a value rather than another tree branch, so that the whole tree always branches to the right:

        ``` haskell
        data Tree a = EmptyTree | Leaf a | Branch a (Tree a)
        ```

        - `Leaf` a can be represented as Branch a EmptyTree, so we can simplify this to:

        ``` haskell
        data Tree a = EmptyTree | Branch a (Tree a)
        ```

        Substitute:

        - EmptyTree ==\> \[\]
        - Tree ==\> \[\]
        - Branch ==\> :

        ``` haskell
        data [] a = [] | a : [a]
        ```

3.  Q. What is a fold in Haskell?

    1.  Answer 1: Almost everything

        Informally:

        f a -\> b ==\> is a folding, a (usually lossy) reduction to a summary value f a -\> f b ==\> is a mapping (eg `fmap f`) a -\> f a ==\> is a wrapping (eg `pure`)

        wrap: a -\> f a map: f a -\> f b fold: f a -\> b

    2.  Answer 2: Data.Foldable.fold

        [Data.Foldable](https://hackage.haskell.md/package/base-4.16.0.0/docs/Data-Foldable.html#g:7)

        [Foldable Traversable In Prelude - HaskellWiki](https://wiki.haskell.md/Foldable_Traversable_In_Prelude)

        ``` zsh
        app/app.hs:19:8: warning: [-Wcompat-unqualified-imports]
            To ensure compatibility with future core libraries changes
            imports to Data.List should be
            either qualified or have an explicit import list.
           |
        19 | import Data.List
           |        ^^^^^^^^^
        ```

        ``` haskell
        -- The Foldable class represents data structures that can be reduced to a
        -- summary value one element at a time.  Strict left-associative folds are a
        -- good fit for space-efficient reduction, while lazy right-associative folds
        -- are a good fit for corecursive iteration, or for folds that short-circuit
        -- after processing an initial subsequence of the structure's elements.
        ```

        ``` haskell
        fold :: Monoid m => t m -> m
        fold = foldMap id

        foldMap :: Monoid m => (a -> m) -> t a -> m
        foldMap f = foldr (mappend . f) mempty
        ```

    3.  Answer 3: Higher-order, recursive functions

        ``` haskell
        recurse :: forall a b.
            -- binary accumulation function
            (a -> b -> b) ->
            -- initial value of accumulator
            b ->
            -- list to be folded
            [a] ->
            -- result
            b
        ```

        1.  tail recursion

            ``` haskell
            recurseTail :: (b -> a -> b) -> b -> [a] -> b
            recurseTail f = go
              where
                go s [] = s
                go s (x:xs) = go (f s x) xs
            ```

        2.  (co-)recursion

            ``` haskell
            recurse :: (a -> b -> b) -> b -> [a] -> b
            recurse f s0 = go
              where
                go [] = s0
                go (x:xs) = f x (go xs)
            ```

        3.  guarded recursion (?)

            ``` haskell
            recurseGuard :: (a -> b -> Bool) -> (a -> b -> b) -> b -> [a] -> b
            recurseGuard p f s0 = go
              where
                go s [] = s
                go s (x:xs) =
                  let s' = f s x in bool (f x (go s' xs)) s' (p x s)
            ```

        4.  foldr

            ``` haskell
            foldr :: (a -> b -> b) -> b -> [a] -> b
            foldr k z = go
              where
                go [] = z
                go (y:ys) = y `k` go ys
            ```

        5.  foldl'

            ``` haskell
            foldl' :: (b -> a -> b) -> b -> [a] -> b
            foldl' k z0 xs =
              foldr (\(v::a) (fn::b->b) -> oneShot (\(z::b) -> z `seq` fn (k z v))) (id :: b -> b) xs z0
            ```

            - oneShot is a performance annotation (awaiting LinearTypes?)
            - \`seq\` eliminates the laziness in foldr
            - `\k z0 xs = foldr (\v f z -> f (k v z)) id xs z0` folds an accumulating function rather than an accumulating value so that the foldr pattern can be applied.

        6.  first-class, higher-order magic

            ``` haskell

            go acc [] = acc
            go acc (x:xs) = go (f acc x) xs

            (flip arguments and think about b <==> a -> a)

            go [] acc = acc ==> go [] == id

            go (x:xs) acc ==>
            go xs (f x acc) ==>
            (substitute fn ==> go xs into corecursive pattern)
            fn (f acc x) = f x fn acc ==>
            \x fn acc -> fn (f x acc) ==>

            go = foldr (\x fn acc -> fn (f x acc)) id xs acc
            ```

    4.  Answer 4: ⍝ and /

        [Reduce - APL Wiki](https://aplwiki.com/wiki/Reduce#:~:text=Reduce%20(%20%2F%20%2C%20%E2%8C%BF%20)%2C,right%2Dto%2Dleft%20order.)

        Described in Iverson APL (1962)

4.  Associativity & Laziness

    > The natural definition for such a function, sum = fold (+) 0, processes the numbers in the list in right-to-left order. ~ [Hutton 1999](http://www.cs.nott.ac.uk/~pszgmh/fold.pdf)

    foldr & foldl' both traverse a list from left to right. The r\|l suffixes stands for association.

    (:) associates to the right ie

    a:b:c ==\> a : (b : c)

    ``` haskell
    > :i (:)

    type [] :: * -> *
    data [] a = ... | a : [a]
        -- Defined in ‘ghc-prim-0.6.1:GHC.Types’
    infixr 5 :
    ```

    `infixr` is an engineering choice to fit in with laziness. is right-associative because Haskell is lazy.

    ``` haskell
    > head (1:(undefined:[]))
    1

    If (:) was left associative:

    > head ((1:undefined):[])
    [1*** Exception: Prelude.undefined
    ```

    ``` haskell
    head (1:undefined:[]) ==>
    head (1:<thunk>) ==> (weak-head normal form)
    head (x:_) = x ==>
    1
    ```

    foldr (:) \[\] \[1, 2, 3, 4\] = 1 : ⟨foldr (:) \[\] \[2, 3, 4\]⟩

    > In contrast, it is not possible to redefine fold in terms of foldl, due to the fact that foldl is strict in the tail of its list argument but fold is not ~ [Hutton 1999](http://www.cs.nott.ac.uk/~pszgmh/fold.pdf)

5.  Fusion

    1.  list/foldr duality

        > Lists are the primary data structure of functional programming. In lazy languages, such as Haskell, lists also serve in place of traditional control structures ~ [Stream Fusion: From Lists to Streams to Nothing at All](http://citeseer.ist.psu.edu/viewdoc/download;jsessionid=756445B145EFE3978F733AC819D5B628?doi=10.1.1.104.7401&rep=rep1&type=pdf)

        - folding a list is (hopefully) subject to fusion
        - … and so is making a list in the first place

        ``` haskell
        toList :: forall a. t a -> [a]
        toList t = build (\ c n -> foldr c n t)
        ```

        foldr (build s) = s

        > Whatever you do in GHC, you should do it with foldr

        > A series of list appends or monadic binds for many monads performs algorithmically worse when left-associated.
        >
        > – \>\>\> force \$ foldr (+) 0 \[1..\] – **\*** Exception: stack overflow
        >
        > ~ [foldr haddock](https://hackage.haskell.md/package/base-4.16.0.0/docs/src/GHC.Base.html#foldr)

        - Specifically, `foldr/build` fusion ==\> `foldr (build s) = s`

        - GHC is fold-centric (and unfold & stream averse?)

        - eg a `map reduce` in other languages translates as (roughly) `build foldr build foldr` and gets fused to `build foldr`.

        - `augment` is a specialisation of build (for tail-recursion)

        - therefore, just about everything in base is a foldr

          ``` haskell
          map f xs = foldr (a b -> f a : b) [] xs
          filter f xs = foldr (a b -> if f a then a:b else b) [] xs
          reduce f x0 xs = foldr f x0 xs
          ```

        - [Notes on Fusion](https://teh.id.au/#/posts/2017/06/30/notes-on-fusion/index.html)

        - [GHC optimization and fusion](https://markkarpov.com/tutorial/ghc-optimization-and-fusion.html)

        [Implement list fusion using streams instead of foldr/build (#915) · Issues · …](https://gitlab.haskell.md/ghc/ghc/-/issues/915)

    2.  Answer 5: A fold in GHC is foldr "under the hood"

6.  Why are there different kinds of folds?

    Because different kinds of computations are lazy or strict.

    > This gives us a general rule of thumb for using foldl and foldr on lists:
    >
    > - When the accumulation function is strict, use foldl' to consume the list in constant space, since the whole list is going to have to be traversed, anyway.
    >
    > - When the accumulation function is lazy in its second argument, use foldr to do work incrementally to improve streaming and work-saving.
    >
    > - Never use foldl or foldr'; they’re always worse on lists.
    >
    > ~ [lexie lambda](https://github.com/hasura/graphql-engine/pull/2933#discussion_r328821960)

    foldl space leak

    ``` haskell
    foldl (+) 0 [1, 2, 3, 4]
     = foldl (+) (0 + 1) [2, 3, 4]
     = foldl (+) ((0 + 1) + 2) [3, 4]
     = foldl (+) (((0 + 1) + 2) + 3) [4]
     = foldl (+) ((((0 + 1) + 2) + 3) + 4) []
     = ((((0 + 1) + 2) + 3) + 4)
     = (((1 + 2) + 3) + 4)
     = ((3 + 3) + 4)
     = (6 + 4)
     = 10
    ```

    foldl eager evaluation - no space leak

    ``` haskell
    foldl' (+) 0 [1, 2, 3, 4]
     = foldl' (+) 1 [2, 3, 4]
     = foldl' (+) 3 [3, 4]
     = foldl' (+) 6 [4]
     = foldl' (+) 10 []
     = 10
    ```

    foldr WHNF thunk build-up

    ``` haskell
    foldr (+) 0 [1, 2, 3, 4]
    = 1 + foldr (+) 0 [2, 3, 4]
    = 1 + (2 + foldr (+) 0 [3, 4])
    = 1 + (2 + (3 + foldr (+) 0 [4]))
    = 1 + (2 + (3 + (4 + foldr (+) 0 [])))
    = 1 + (2 + (3 + (4 + 0)))
    = 10
    ```

7.  Further readings

    - [Data.Foldable](https://hackage.haskell.md/package/base-4.16.0.0/docs/Data-Foldable.html)
    - [foldl: Composable, streaming, and efficient left folds](https://hackage.haskell.md/package/foldl)
    - [Haskell for all: Composable streaming folds](https://www.haskellforall.com/2013/08/composable-streaming-folds.html)
    - [OverloadedLists](https://ghc.gitlab.haskell.md/ghc/doc/users_guide/exts/overloaded_lists.html)
    - recursion-schemes
      - foldl' is a catamorphism
      - non-element-wise, non-binary recursion
      - fixed point
      - [yaya: Total recursion schemes.](https://hackage.haskell.md/package/yaya)
      - [recursion-schemes: Representing common recursion patterns as higher-order fun…](https://hackage.haskell.md/package/recursion-schemes-5.2.2.2#readme) (note: example is actually tail recursion/foldl')
      - [Functional Programming with Bananas, Lenses, Envelopes and Barbed Wire](https://maartenfokkinga.github.io/utwente/mmf91m.pdf)
    - [F-Algebras \|   Bartosz Milewski's Programming Cafe](https://bartoszmilewski.com/2017/02/28/f-algebras/)

### fusion

<https://jyp.github.io/posts/controlled-fusion.html>

### recursion schemes

<https://github.com/passy/awesome-recursion-schemes>

[Functional Programming with Bananas, Lenses, Envelopes and Barbed Wire](https://maartenfokkinga.github.io/utwente/mmf91m.pdf) [An introduction to Recursion Schemes](http://blog.sumtypeofway.com/an-introduction-to-recursion-schemes/)

<https://blog.sumtypeofway.com/posts/recursion-schemes-part-6.html> <http://dreixel.net/research/pdf/ds.pdf> <https://blog.poisson.chat/posts/2021-01-03-iterative-categories.html>

### memoization

[Memoization in Haskell? - Stack Overflow](https://stackoverflow.com/questions/3208258/memoization-in-haskell/3209189#3209189)

### dlist trick

<https://teh.id.au/build/slides-ba7249f1f4be6343.pdf>

### build

<https://www.reddit.com/r/haskell/comments/9ykkkc/code_yoneda_stuff/>

``` haskell
build :: forall a.
         (forall xx. (a -> xx -> xx) -> xx -> xx)
      -> [a]
build builder = builder @[a] (:) []
```

### corecursion

Corecursive fibonacci numbers from <https://meldingmonads.files.wordpress.com/2009/06/corecqueues.pdf>

``` haskell
data Tree a b
= Leaf a
| Branch b (Tree a b) (Tree a b)
deriving (Eq, Show)
fib :: Int → Tree Int Int
fib n = fibs !! (n − 1)
where
fibs = Leaf 0 : Leaf 0 : zipWith3 Branch [1 . .] fibs (tail fibs)
sternBrocot :: Tree a (Ratio Integer )
sternBrocot = loop 0 1 1 0
where loop a b x y
= Branch (m % n) (loop a b m n) (loop m n x y)
where m = a + x
n = b + y
```

## Search

### Search Concepts

- climber
- exhaustive methods
  - depth first
  - breadth first
  - balanced depth/breadth
  - pruning exhaustive O(exp) \>\>\> pruning O(x<sup>n</sup>)
- tsp
  - <http://users.cs.cf.ac.uk/C.L.Mumford/howard/FarthestInsertion.html>
  - <https://en.wikipedia.md/wiki/Christofides_algorithm>
  - <https://github.com/boechat107/tsp_furthest_insertion_haskell/blob/master/second_version/KDtree.hs>
  - <https://github.com/timerg/HaskellForTSP/blob/master/src/TSP/NN.hs>
  - <https://en.wikipedia.md/wiki/2-opt>
- pattern recognition <http://users.isr.ist.utl.pt/~wurmd/Livros/school/Bishop%20-%20Pattern%20Recognition%20And%20Machine%20Learning%20-%20Springer%20%202006.pdf>

### local-search

[Control.Search.Local.Strategies](https://hackage.haskell.md/package/local-search-0.0.7/docs/Control-Search-Local-Strategies.html)

<https://etheses.whiterose.ac.uk/4847/1/Richard%20Senington's%20thesis.pdf>

ephemeral language is very similar to this.

neighbourhood is generated from a seed (and not a distance metric)

### Conal

<https://www.youtube.com/watch?v=Ns3DxUeCvRg>

- function optimisation - "best" element of a set
  - differentiation
  - gradient descent
- find objective function from a set (meta-function optimisationx)
- objective function defined using input/output pairs

important elements of an api

- function representation
- can we differentiate (implementation detail)
- binary sequential, parallel conditional composition
- not multi-dimensional arrays

Values

- precision
- simplicity
- generality

### Algebras for Combinatorial Search

<https://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.107.5318&rep=rep1&type=pdf>

### Jules

<https://arxiv.md/pdf/1406.2058.pdf>

### Algebras for Weighted Search

[Algebras for Weighted Search](https://dl.acm.md/doi/pdf/10.1145/3473577)

graphs are endomorphisms in the Kleisli category of the free semimodule monad.

### backtracking

[LogicT](http://okmij.md/ftp/Computation/LogicT.pdf)

### pattern recognition

<http://users.isr.ist.utl.pt/~wurmd/Livros/school/Bishop%20-%20Pattern%20Recognition%20And%20Machine%20Learning%20-%20Springer%20%202006.pdf>

### blogs

[Why Functional Programming Matters by John Hughes at Functional Conf 2016 - Y…](https://www.youtube.com/watch?v=XrNdvWqxBvA) [A Purely Functional Typed Approach to Trainable Models (Part 1) · in Code](https://blog.jle.im/entry/purely-functional-typed-models-1.html) [Backpropogation is Just Steepest Descent with Automatic Differentiation \| Mat…](https://idontgetoutmuch.wordpress.com/2013/10/13/backpropogation-is-just-steepest-descent-with-automatic-differentiation-2/) [GitHub - mikeizbicki/HLearn: Homomorphic machine learning](https://github.com/mikeizbicki/HLearn) [Machine Learning in Haskell · tonyday567](https://tonyday567.github.io/posts/learning/) [Algebras for Weighted Search](https://dl.acm.md/doi/pdf/10.1145/3473577) [Probabilistic programming with continuations – Jules Hedges](https://julesh.com/2020/08/15/probabilistic-programming-with-continuations/) [BOB Summer 2019 - Conal Elliott, A Functional Reboot for Deep Learning - YouTube](https://www.youtube.com/watch?v=Ns3DxUeCvRg)

## fix

[articles/fix.md at master · quchen/articles · GitHub](https://github.com/quchen/articles/blob/master/fix.md) [Rebecca Skinner - The Fixed Point](https://rebeccaskinner.net/posts/2021-06-09-getting-to-the-fixed-point.html#part2) [Deriving via Free and Cofree : haskell](https://www.reddit.com/r/haskell/comments/mf8588/deriving_via_free_and_cofree/)

Self-recursive tree helper function.

``` haskell
newtype Fix f = Fix (f (Fix f))
```

``` haskell
Cofree f a = Free (Compose ((,) a) f) void
Free f a = Cofree (Compose (Either a) f) ()
Fix f = Cofree f () = Free f Void
```

Left adjoints 'Free f' preserve initial objects 'Void' Right adjoints 'Cofree f' preserve terminal objects '()'

`Fix f` is both the initial and terminal algebra

``` haskell
newtype Fix f = Product (forall r. (f r -> r) -> r)
```

### Greatest fix point

<https://www.reddit.com/r/haskell/comments/5l3jzc/fusion_and_streaming/> <https://homepages.inf.ed.ac.uk/wadler/papers/free-rectypes/free-rectypes.txt>

### morphisms

<https://ipfs.io/ipfs/QmTppu1VDAQWsdiyVSZX6qb8PErdpwzNP2oKfEhcgaBvWR/guide-to-morphisms.pdf>

using free instead of fix

<https://chrispenner.ca/posts/asts-with-fix-and-free>

<https://jyp.github.io/posts/free-structures.html>

Explanation of Free

If a data type can be written as the fixpoint of some functor f:

data Fix f = In (f (Fix f))

then its Church encoding is as follows:

type Church f = ∀α. (f α → α) → α

## code design

### functor-oriented design

<https://www.reddit.com/r/haskell/comments/75fo8k/functor_oriented_programming/do5vqa5/>

<http://r6.ca/blog/20171010T001746Z.html>

<https://stackoverflow.com/questions/20296936/is-it-possible-to-create-a-functor-instance-for-sorted-binary-trees-in-haskell>

### category design

There's a big idea in haskell - that the solution to the big-ball-of-mud that is modern software creation is a category theory design pattern.

One beginning of this thread is a Don Stewart [editorial](http://stackoverflow.com/questions/27852709/enterprise-patterns-with-functional-programming/27860072#27860072). To paraphrase:

- almost all designs fall into the 'compiler' or 'interpreter' pattern, using a model of the data and functions on that data. That is,

  1.  data: problem domains are represented as algebraic structures (ADTs, GADTs)
  2.  functions: software architectures are about mapping from one algebra to another.

- Our algebraic data types are the best way of capturing structures.

- Scalable software architectures = this "category theory" design pattern.

Following this down various rabbit holes:

[Understanding Algebras](https://www.fpcomplete.com/user/bartosz/understanding-algebras)

[The Functor design pattern](http://www.haskellforall.com/2012/09/the-functor-design-pattern.html)

[The Category design pattern](http://www.haskellforall.com/2012/08/the-category-design-pattern.html)

[Scalable program architectures](http://www.haskellforall.com/2014/04/scalable-program-architectures.html)

[Modular program design](http://stackoverflow.com/questions/13007123/modular-program-design-combining-monad-transformers-in-monad-agnostic-function)

[Design patterns in haskell](http://blog.ezyang.com/2010/05/design-patterns-in-haskel/)

Kmett reddit [aside](http://www.reddit.com/r/haskell/comments/2f9w0p/is_it_practical_to_write_a_strong_chess_engine_in/):

One trick I tend to use when I want to write a thing but Haskell isn't fast enough to make it go is to write it in Haskell in a different way: Make an EDSL to express the problem space, turn my domain specific problem into a language problem, then write a compiler for my domain that does every domain specific optimization I can think of and have it spit out efficient code to something like LLVM/CUDA, etc.

The benefit of that model is you can know it can eventually hit whatever speed limit that is achievable in any language and you still get to think and code in Haskell.

The cost of that model is of course that it is a very very deep embedding, and you then need to write a compiler to write a program where if you're willing to accept whatever you get a more direct shallow encoding of the domain as a library / program in haskell would do just fine.

more links:

[NCD conferences: architecture patterns](https://vimeo.com/180287057)

[Functional programming design patterns by Scott Wlaschin](https://vimeo.com/113588389)

[Large scale design in haskell](http://stackoverflow.com/questions/3077866/large-scale-design-in-haskell/3077912#3077912)

### Handle pattern

[jaspervdj - Haskell Design Patterns: The Handle Pattern](https://jaspervdj.be/posts/2018-03-08-handle-pattern.html)

### Three Layer Haskell Cake

[Three Layer Haskell Cake](https://www.parsonsmatt.md/2018/03/22/three_layer_haskell_cake.html)

### Invert your mocks

[Invert Your Mocks!](https://www.parsonsmatt.md/2017/07/27/inverted_mocking.html)

# Probability

## maximum entropy

<https://twitter.com/brandondamos/status/1496496179907022849>

## Stochastic Volatility

[Stochastic Volatility \| Maths, Stats & Functional Programming](https://idontgetoutmuch.wordpress.com/2015/03/11/stochastic-volatility/)

<https://faculty.chicagobooth.edu/nicholas.polson/research/papers/lopes-polson-2010.pdf>

## Prop

Since Cont/Codensity is also Prob this may well also pump out a nice approach to bayesian analysis.

<https://julesh.com/2020/08/15/probabilistic-programming-with-continuations/>

## Bayes

> Bayesian statistics is the other part of calculus, solving really tricky integrals.

[Interview with Will Kurt on his latest book: Bayesian Statistics The Fun Way](https://notamonadtutorial.com/interview-with-will-kurt-on-his-latest-book-bayesian-statistics-the-fun-way-63ce8aee32ed)

[Course](http://hedibert.md/wp-content/uploads/2013/12/UPCcourse-handouts.pdf)

\[\[<https://www.countbayesie.com/>\]\[Count Bayesie - A Probability Blog \]\] Bayesian Updates Compose Optically

<https://arxiv.md/pdf/2006.01631.pdf>

## Probabilistic Programming

[Functional Programming for Modular Bayesian Inference](http://denotational.co.uk/publications/scibior-kammar-ghahramani-funcitonal-programming-for-modular-bayesian-inference.pdf)

[Practical Probabilistic Programming with Monads](http://mlg.eng.cam.ac.uk/pub/pdf/SciGhaGor15.pdf)

[GitHub - tweag/monad-bayes: A library for probabilistic programming in Haskell.](https://github.com/tweag/monad-bayes)

[continuation == probability](https://julesh.com/2020/08/15/probabilistic-programming-with-continuations)

## Measure Theory

<http://www.stat.umn.edu/geyer/8501/measure.pdf>

### Inference

[Embedded DSLs for Bayesian Modelling and Inference: a Retrospective · jtobin.io](https://jtobin.io/embedded-dsls-bayes)

MCMC is the de-facto standard way to perform inference on Bayesian models

> it’s very questionable whether the fancy MCMC algo du jour is really any better than some combination of Metropolis-Hastings (even plain Metropolis), Gibbs (or its approximate variant, slice sampling), or nested sampling in anything outside of favourably-engineered examples
>
> [Gibbs Sampling in R, Haskell, Jags and Stan](https://idontgetoutmuch.wordpress.com/2014/04/09/gibbs-sampling-in-r-haskell-jags-and-stan/)

### Hamiltonian Monte Carlo

[MCMC using Hamiltonian dynamics](https://arxiv.md/pdf/1206.1901.pdf)

## stan

[The fundamental abstractions underlying BUGS and Stan as probabilistic progra…](https://statmodeling.stat.columbia.edu/2017/09/07/fundamental-abstractions-underlying-bugs-stan-probabilistic-programming-languages/)

[Getting more out of Stan: some ideas from the Haskell bindings \| Zenodo](https://zenodo.md/record/1465992#.XktdSZMzau5)

# Footnotes

\[fn:1\]
