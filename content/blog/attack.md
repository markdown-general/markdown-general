+++
title = "Haskell and the elegant attack."
author = "Tony Day"
date = 2021-06-05
lastmod = 2022-01-01
draft = false
tags = ["haskell"]
+++

Welcome back to the Happy Haskell HAckers Tour. As we approach our next
stop, let me firstly apologise for the recent detours. Our bus driver,
Yoneda, mixed up the maps and started following the GHC After
Dark[^after] script. But now we're back on track, and coming up on your
left is our monument to the Haskell `literals`:

```text
-- | motto
--
-- We may need to think more about this (SPJ)
--
motto :: String
motto = "avoid success at all costs"
```

For a long time, through the darkness of pointfree, the incompatibility
of lhs, and the golden age of DerivingVia, this `motto` has guided
Haskell language design. If you peer closely, you'll notice some scratchings
of the original work. These are various attempts at interpretation
involving commas and parenthesis, by our bright but cheeky novitiates,
but we've done our best to restore the work to its original intent.

Archivist retrieval digs have recently confirmed the original context of
these words, and now believe it is not a `motto` at all, but part of a
larger computation[^trans], one that has been recovered[^onenote] from a
lost functional pearl[^lits]

```haskell
-- | The literals
--
-- Ok, ship it (SPJ)
--
literals :: [String]
literals =
  mconcat $
    fmap List.unwords <$>
    (combinations <$>
     [1 .. 5] <*>
     pure
     (List.words
      "avoid success at all costs"
     ))
```

Whilst we do not have time to cover all of the technical details[^tech],
what has previously been described as the Haskell `motto`, is, in fact,
simply a seed string that generates a lexicographically ordered list of
aphorisms (the `literals`).

Note how the seed value is embedded directly in the computation and
cannot be changed. One can imagine inserting another `motto` into the
computation, but this would, literally, not be Haskell.

Note also that any combination disrespecting the lexicographic ordering
of the seed literal is considered heresy, and will regardless be
unavailable to the compiler. Problematic examples that have caused
confusion in the past include "success avoid" and "costs avoid success."

But enough with the theory, if a volunteer would like to step up and
spin the literal combinator, that big button marked `random-1.2.0`,
let's meet the Haskell literals.

```text
-- >>> literal 5
-- avoid success
```

Or, as Rudyard Kipling (an early adopter of category theory) wrote on
the steps of Wimbledon, "meet with triumph and disaster, and treat those
two impostors just the same (or at least, up to isomorphism)."

Quoting from the lost pearl, "maybe we should avoid labels such as
success being placed on language design. Either they will turn out to be
correct, in which case the label is redundant, or they will eventually
need replacement, in which case the label becomes an impediment to
adopting better ways."

```text
-- >>> literal 1
-- success
```

My favourite literal, and one of the `singletons`. The singletons are
best understood as interjections[^bang], so an accurate translation is
actually `Success!`.

In the lost pearl, it reads literally as "joy of haskell." Some other
interpretations include "I can't believe it compiles!", "Huzzah!",
"Poggers! Let's goooo!" and, the dryer, "With Haskell, if you can get it
to compile, it usually runs."

```text
-- >>> literal 0
-- avoid
```

Or `Avoid!`, the head of the literals. To quote the
ancients[^alphabeta], "In a functional language with lazy evaluation, we
can go a step further and eliminate the recursive definitions ... in
favour of a more modular solution." A more modern version is "a bug can
only exist if it's representable." When laziness is inherent and exposed
in language design, we can all get to the point quicker and have some
down time.

Avoid!


## The Elegant Attack Proclamation {#the-elegant-attack-proclamation}

```text
-- >>> literal 30
-- avoid success at all costs
```

Now Yoneda and I could sit here all day, watching the combinator
spin[^spin] its way through `Hask -> Hask`, but that's our day job, and some of
you may be getting bored, so let's skip to the last of the `literals`.

In a recent enunciation[^elegant], `literal 30` was described in these
terms:

> Haskell embodies a radical and elegant attack on the entire enterprise
> of writing software.

The elegance comes from how the literals combine to form this emerging
narrative. For example, this is not the elegant attack but embodies it:

> Haskell is a non-commercial (avoid all costs[^l20]), volunteer army
> (success at all costs[^l29]) who would like coders (forall
> coders.[^l3]) everywhere (avoid success at[^l15]) to have rich
> (success at all[^l21]) lives, with less tedium (Avoid!) and the best
> (Success!) tools.

Please, a round of applause, and spare change if you have it, for our
real-life combinatorial choreographers of language design, who juggle
and balance the `literals` day in and day out, in their quest for
software perfection.

And until next time, when we visit the GHC foundry, where tar balls are
rendered and frozen, and libraries machined to isomorphic perfection,
Yoneda and I will leave you be. Feel free to spin the literal combinator
some more, and ponder the Haskell `literals` and the elegant attack they
may formulate.

Finally, `literal 7` (avoid all)[^apology]

---

[^heinlein]: As, Heinlein, one of our early adopters of `literal 15`,
scribbled on the side of a punch card, "specialisation is for insects."

[^l3]: `literal 3: All!` Also written `forall coders.`

[^l15]: `literal 15: Avoid success at` Haskell is a general purpose
language. Although we are quite good at parsing, do not box us in.
Success at a particular endeavour does not necessarily mean we should
specialise[^heinlein], but instead maybe work on our weaknesses.

[^l20]: `literal 20: Avoid all costs` Haskell, as a project, is poor and
shall remain so. A price point of zero ensures wide adoption and a
collegiate approach to the craft of coding.

[^l21]: `literal 21: Success at all` We would like coders to have
balanced lives, with less boiler-plate, and for teams of coders to use
and embrace diversity, so that success may be shared widely.

[^l29]: `literal 29: Success at all costs` For all that they are, this
bunch of misfits is pretty focused on the tasks of writing software.
They are not in the mix to take some small slice of existing commercial
arrangements.

[^poverty]: Debate continues as to whether `literal 20` necessarily
requires volunteers to also take a vow of poverty.

[^alphabeta]: R.S. Bird; John Hughes (1987). The alpha-beta algorithm:
An exercise in program transformation.

[^after]: Unfortunately, the GHC After Dark tour is solidly booked due
to unprecedented demand.

[^onenote]: Found in a recursively-defined Windows backup directory, of
all places.

[^trans]: The code is not the exact original, but has had some doctrinal
modification, including the qualified List obligation, the applicative
noise injunction, the trailing operator cascade and, of course, monadic
purification. The use of `[String]` may grate the modern ear, but the
old interface is retained to allow interaction with the other Haskell
List String mysteries.

[^bang]: An interjection or exclamation in English is similar to
BangPatterns, but with StrictData also turned on, that can be used to
interupt concurrent conversation.

[^tech]: Just as an aside, to quote from the lost pearl: "For the
combinations operation, we use a functional algorithm similar in spirit
to Knuth's algorithm R. The imperative version is also known as Knuth's
revolving-door algorithm, but in the functional equivalent the door does
not so much revolve as remain in a fix point in relation to the grey
code generated, saving the computation costs of actually revolving the
door.""

[^spin]: or remain a fix point in the natural transformation of category
theory to software design, if you prefer.

[^elegant]: The Elegant Attack Proclamation can be found at
[haskell.foundation](https://haskell.foundation), just above "Learn
about Haskell."

[^apology]: For passengers who have specifically complained, again,
apologies. For those complaining that they did not sign up for any tour
business, this is, in reality, not a bus tour but an allusion to one. A
sketch to set a satirical scene in which to portray the craft of
software design. In my defence, the use of allusion and metaphor used to
be stock in trade for a functional programmer back in the day. If you
read any old functional pearl you will find an author playing with the
English language alongside their functional language expressions. Put
the fun back in function I say but, regardless, you are free to click
away at any time. I won't name names, but I understand that, for
specific cultures, satire is seen as equivalent to sarcasm. 40 years of
SNL has robbed this culture of seeing any joy or point to parody, and
that is sad. Treating subject matter whimsically is not equivalent to
treating it as a joke. To the passenger who alluded to doxing Yoneda and
I, on the main charge of sloppy writing, if I understand the complaint,
and described uncertainty surrounding the fictional status of the piece
as unbearable, I say good day ma'am, but your ride is over.

[^lits]: [GitHub - tonyday567/lits: The Haskell literals](https://github.com/tonyday567/lits)
