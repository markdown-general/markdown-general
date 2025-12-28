+++
title = "lowercase haskell"
author = "Tony Day"
date = 2020-12-15
lastmod = 2022-01-01
draft = false
tags = ["haskell"]
+++

Type-level trickery, UpperCase Haskell, is the showy rock-star of
Haskell and all power to the Type. I like a good Type as much as anyone,
but what keeps me using the language is the work-a-day value-level
coding tool-kit - lower case haskell.

Let me explain.

In noodling around with
[stan](https://hackage.haskell.org/package/stan), a wonderful tool
developed with obvious care and attention by the talented
[kowainik](https://kowainik.github.io/) troupe, I created
[hcount](https://hackage.haskell.org/package/hcount), a cheap and
cheerful attempt to count symbol usage in Haskell projects. Please try
it out and let me know how it runs.

Here's my personal top 10:

```haskell
base      $            828
local     x            696
base      fromLabel    613
base      .            536
local     a            510
numhask   *            510
base      &            499
lens      .~           483
base      <$>          479
base      <>           422
```

You can tell so much about a coder from their top 10 list. I'm an
unreconstructed `$` user after a decade of Haskell hacking: you write
from right to left, plonk down a `$`, add the next thing, think some
more, rinse and repeat. I use `<$>` extensively because of this style: I
plonk down a `$` and then realise I am moving up a functorial level so
wrap it to a `<$>`.

The very next refactor I have planned, however, is to replace each and
every `<$>` with `fmap`. The flow I get from subbing `<$>` for `$` is
almost always interrupted with having to then bracket everything on the
right because of the change in fixity.

I have made some efforts to move to a more dotty approach and I suspect
my `.` usage has risen of late. I also, somewhat unusually I suspect,
commonly write left to right using `&`, the flipped `$`, especially when
using lens. But I never use `>>>` (the flipped `.`) probably because of
it's garish look.

I'm an unrepentant user of
[OverloadedLabels](https://ghc.gitlab.haskell.org/ghc/doc/users%5Fguide/exts/overloaded%5Flabels.html),
hence the `fromLabel` (which ghc inserts on my behalf) and `.~` usage. I
tend towards single letter, anonymous local names and hope that my logic
function blocks and Typing habits are simple enough to justify this
(they're probably not). I'm a mathy coder given `*` is right up there
(`+` is 14th). I love semigroup and look for it constantly.


## lower-case haskell {#lower-case-haskell}

```haskell
base      fromLabel    613
base      pure         200
numhask   zero         176
base      mempty       173
numhask   one          168
protolude show         143
numhask   fromIntegral 140
protolude bool         105
base      fmap          97
base      maybe         90
```

Once I filtered out the operators I was immediately struck by just how
much love and respect I have for lower case haskell (except for
`fromIntegral` which always grates given how loooong and boringly
obvious it is). Why the love?


## [pure](https://hackage.haskell.org/package/base/docs/Prelude.html#v:pure) {#pure}

I came late to the Haskell party, started up the typical learning curve
of Monoid-Functor-Applicative-Monad and never quite made it to the top.
Monads (or MoNaDs as they used to spell it) are just so 90s, so late Gen
X - early Millennial. They remind me of that other 90s sickly-child that
is Radiohead; if they saved rock and roll (twice!) then how come you
can't dance to it?

Wading through other peoples' library monad wrappers ("i wanna have
control"), part of the mess was the metaphysical-linguistic confusion of
`return`. Where am I that I have to now return, I would ask ("i don't
belong here"). The tutorials told me I was somewhere ("what the hell am
i doing here"), somewhere special ("i wish i was special"), but now I
need to go back to somewhere else ("but I'm a creeeeep!").

Don't @ me on this; I understand monads. I hate on 'em cause I don't
like 'em.

Looking through my lower-range usage, I have one use of `=<<`, one of
`>=>`, a single `>>=` and can't recall the signature for any of them.

Meanwhile `pure` means simple; it's only a value, gently lifted and
placed into an Applicative. Rock on AMP!


## [bool](https://hackage.haskell.org/package/base/docs/Data-Bool.html#v:bool) {#bool}

When I first learnt about this gem, this functional eliminator, I had an
epiphany: if I used `bool` I would never ever again have to decide how
to indent an `if`-`then`-`else` statement. I'd never even have to recall
whether haskell has an else clause (of course it does because without it
it's a partial but I'm forgetful and often dull).

Most of all, what happens when I code with `bool` is I stay in the flow
compared with having to use my `if`-`then`-`else` fingers or construct a
`case`-`blah`-`of`-`x->etc` monolith. I switch from _homo categoralis_,
master of domain and codomain alike, to _homo narratavis_, teller of
post-modern stories and (sometimes) bender of truthiness.

A nice extra of `bool` usage is cheap refactoring. You can cut and paste
a bool statement, no matter the complexity, and stand a very good chance
it will work in a new context, without moving brackets or indents or
redesigning a case statement.

The only problem is nested `bool`'s start to get a bit hairy (though not
as bad as nested if-then-else's). More than 2 layers of bool and it's
time for guards.


## [maybe](https://hackage.haskell.org/package/base/docs/Prelude.html#v:maybe) & [either](https://hackage.haskell.org/package/base/docs/Prelude.html#v:either) {#maybe-and-either}

It took me years to discover `maybe` and `either`. I knew they were
there but I didn't naturally reach for them. And then one day, neck deep
in some transformer stack, looking up whether I needed to run my
runEitherT before or after I execute my execMaybe, deleted the whole
stack in anger and never looked back.

| Consider:  |                                        |
|---|---|
|------------|----------------------------------------|
| throwing   | <kbd>maybe (throw SomeException)</kbd> |
| defaulting | <kbd>either (const def)</kbd>          |
| maybeing   | <kbd>either (const Nothing) Just</kbd> |
| eithering  | <kbd>maybe (Left "error") Right</kbd>  |
there is an economy of compositional movement you don't get anywhere else.


## [zero](https://hackage.haskell.org/package/numhask/docs/NumHask-Algebra-Additive.html#v:zero) & [one](https://hackage.haskell.org/package/numhask/docs/NumHask-Algebra-Multiplicative.html#v:one) {#zero-and-one}

In plain Haskell, there is no `zero` and there is no `one` - our
ancestors weep at the loss. Sure we have the literate `0` & `1` which
desugars to `fromInteger 0` & `fromInteger 1` but these are pale shadows
of the twin unital gods of arithmetic.

Consider these examples:

```haskell
-- what signum would look like if we had a zero and one
sign = bool (negate one) one . (>= zero)

-- conversion of boolean to a number to construct an identity matrix
ident = tabulate (bool zero one . isDiag)

-- applicative standard deviation
(\ss s -> (ss - s ** (one + one)) ** (one / (one + one))) <$> (** (one + one)) <*> id

-- boolean conversion to a number
bool zero one
```

In each, there is a sense of intent, of unital usage to shift domain,
rather than the _just another magical number_ feel that comes from using
`0` or `1`.


## [first](https://hackage.haskell.org/package/base/docs/Data-Bifunctor.html#v:first) & [second](https://hackage.haskell.org/package/base/docs/Data-Bifunctor.html#v:second) {#first-and-second}

Another pair of terse, vital tools in my kit that are not even in
prelude. They were trapped in the unfortunate arrows abstraction for a
long while but find a nice home in bifunctors.

Consider adding commas to a number and fixing the decimal points:

```haskell
addcomma n x =
  uncurry (<>) .
  second (take n) .
  first (reverse . intercalate "," . chunksOf 3 . reverse) .
  Text.breakOn "." $
  x
```

How do you write that without `first` & `second` (or bimap)? Only by
busting the composition into components and exponentiating complexity.

Amazingly and somewhat mysteriously, they work with both tuples and
Either, so you can refactor between the two.

I track two things at once so much in my code that
[`<<*>>`](https://hackage.haskell.org/package/bifunctors/docs/Data-Biapplicative.html#v:),
the biapplicative version of spaceship,is in my top 20. I bet others do
too.


## [fmap](https://hackage.haskell.org/package/base/docs/Data-Functor.html#v:fmap) {#fmap}

For the most quint-essential function in all of haskelldom, `fmap` has
the worst documented explanation. It starts with reference to
-XApplicativeDo (scoring 0% proliferation on a recent
[GHC2021](https://mail.haskell.org/pipermail/ghc-steering-committee/2020-November/001876.html)
post), sugars the very definition being explained into do notation, and
then talks self-referentially about an implied Functor constraint, when
fmap is the sole operator of Functor. Never get a committee of fish to
explain water.

But it's the best named, especially in comparison to the other maps in
the other languages. This is the functor-map (personally, I pronounce it
`f'n'map`) because, unlike where you're from, we have other ways to map.
There is the
[bimap](https://hackage.haskell.org/package/base/docs/Data-Bifunctor.html#v:bimap)
of bifunctors, the
[dimap](https://hackage.haskell.org/package/profunctors/docs/Data-Profunctor.html)
of profunctors (with lmap and rmap), the hippy
[contramap](https://hackage.haskell.org/package/contravariant-1.4.1/docs/Data-Functor-Contravariant.html#v:contramap),
to say nothing of the various monomorphic maps such as
[Map.map](https://hackage.haskell.org/package/containers/docs/Data-Map-Strict.html#v:map).
And when others say map we instead often see traversing, lifting,
sequencing, aligning or zipping. Haskell has 50 different names for
mapping.


## [const](https://hackage.haskell.org/package/base/docs/Prelude.html#v:const) & [id](https://hackage.haskell.org/package/base/docs/Prelude.html#v:id) {#const-and-id}

I was surprised that neither `id` nor `const` made the top 10 (11th and
24th). I think that are more common in early code but get eventually
factored out as polish occurs (eg <kbd>maybe def id</kbd> becomes
<kbd>fromMaybe def</kbd>). These two, and their upper class cousins `Identity`
and `Const` are what's most noticeably missing in other
language constructs.


## A prediction {#a-prediction}

There's so much more I could that can be said from this simple, cheesy
analysis. For instance, my heavy usage of `reverse` (19th most common)
is no doubt a code smell. I get lazy and use lists where I should be
using
[Seq](https://hackage.haskell.org/package/containers/docs/Data-Sequence.html#t:Seq).

Instead, I'll hazard an adjunctive prediction. Starting from a position
of [terrible](https://avi-d-coder.github.io/post/dear%5Fhaskell/),
Haskell tooling has now moved into a zone of
[getting
there](https://www.reddit.com/r/haskell/comments/feptnt/is%5Fhaskell%5Ftooling%5Flacking/). With the gap between ghc hacking and front-line tools having
considerably shrunk, so much so that I can interface with the beast,
then expect the Haskell user interface to evolve at speed.

And this may be a catalyst for ubiquitous adoption,, lower case haskell
may yet have it's day to shine. If Haskell begins to turn GHC towards
it's own behaviours and standards, and the community starts to apply
it's categorical sharpness on the problem domain of software
development. Watch this space and remember to upgrade your GUI's!
