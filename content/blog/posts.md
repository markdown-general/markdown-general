---
title: "Machine Learning in Haskell"
date: 2025-12-23
---

# Machine Learning in Haskell

Much advocacy of Haskell, in general, boils down to type-safety and elimination of bugs. How boring. My personal experience is that bugs are trickier in Haskell and I can write bad code in an extraordinary variety of ways.

I don't code in Haskell to help eliminate bugs. That seems a goal unlikely to produce joy in my code. I write Haskell primarily because in enables me to name things a bit better.

## What is Learning?

As an example, I came across this quote within the first few pages of a popular online course on machine learning:

> A computer program is said to learn from experience E with respect to some task T and some performance measure P, if its performance on T, as measured by P, improves with experience E. ~ Tom Mitchell

Here's how I unpacked this into types:

``` haskell
-- | learning is to use experience to
-- change the performance of a task.
newtype Learn f e p =
  Learn {
    change ::
        (Foldable f) =>
        Experience f e ->
        Task e p ->
        Task e p }
```

A common approach to newtypes is to label the unwrapping as a reversal of the newtype (such as `unlearn` or `unLearn`) but this misses an important naming opportunity: to use a Learn is to change.

``` haskell
-- | An experience is a set of e's
newtype Experience f e =
  Experience { set :: f e }
```

``` haskell
-- | A task takes an e and measures
-- performance.
newtype Task e p =
  Task { measure :: e -> p }
  deriving (Profunctor)
```

This could be thought of (and coded) as an ordinary function with type (e -\> p). The advantage of using a newtype, however, is to highlight the two ways that a task can change:

- changing the way a task is performed (the `e`).
- changing the way a task is measured (the `p`).

Acting on a learning, you can change the way a task is performed, such as changing parameters of a function, or change the way you view what performance is.

To change the way a task is performed, taking experience into account, is to make progress. Progress is made one step at a time.

``` haskell
-- | To progress, is to transduce
-- a Task, via a carrier
newtype Progress e p =
  Progress {
    step :: e -> Task e p -> Task e p
  }
```

Putting these above types together in a useful way, to learn, is then to repeatedly apply a progressive step to a set of experiences.

``` haskell
-- | to learn, is to make Progress
-- from an Experience.
learn :: Progress e p -> Learn f e p
learn p =
  Learn $
  \(Experience e) task ->
    foldr (step p) task e
```

To `machine learn`, we change the way a `machine` does a task, based on learning, measuring whether performance improves, and adopt the new way to do a task if it does.

``` haskell
-- | to improve, given a way to change
-- a task by experience,
-- you need to choose the better
-- performance way over an experience
-- set.
improve ::
  ( Ord (f p),
    Foldable f,
    Functor f) =>
  Progress e p ->
  Experience f e ->
  Task e p ->
  Task e p
improve progress es task =
  bool task task' (p' > p)
  where
    task' =
      change (learn progress) es task
    p = measure task <$> set es
    p' = measure task' <$> set es
```

The constraints contained in the above code are mostly based on suggestions from ghc to get a compile, and I didn't think about them much in drafting. Type constraints provide useful guide-rails for future development of these ideas.

## Machine Learning in Haskell?

In the above example of improving we are left with an `Ord (f p)` constraint - that the performance over an experience set is ordinal. If we narrow the constraint to a `Num (f p)`, in other words, assume that the size of the `f p` is meaningful, then we have wandered in to the traditional numerical methods domain, and can, for example, start to think about gradients to speed up our improvement.

By insisting on useful and concrete names (aka types), Haskell supplies a constrained domain we can further explore. For example, are there machine learning examples that don't fit the candidate types and constraints?

Naming things is hard, and other ways of coding tend to avoid the task. Types bring me joy and I shall keep them, thank you very much, and continue to curate collections of them, for fun and profit. In this example, I now have as good a definition of machine learning as you'll find out in the wild, and my progress on machine learning will be more comprehensive, faster and safer than bashing away in python.

And yet, to a first degree, machine learning in Haskell does not exist, swamped by the easiness of dynamic types. If the promises being made by ML are to be eventually honoured, however, it may need less code bashing and better naming.

## Haskell is Diverse

My trans daughter came out to me at a Haskell workshop. At the time, I thought I was tech support but, in retrospect, she was locating a safe space for both of us to digest the news. I helped with installs, practised my pronouns and had a blast.

It can be very difficult to see the diversity in our community. We are sparse; there are maybe 200k active GHC installs in the world, but we are also flexible, global and accessible. Maybe 5% of those installs are due to [tidal](https://github.com/tidalcycles/Tidal), and more to other projects like it, that didn't quite fit anywhere else, and the opportunities they provide to create art (or code as art) on a budget.

Our family now has three Haskell coders (they are best bred). One, that I mentioned, is now a harsh noise artist. I watch code on the screen with a good set of ear plugs, looking for useful refactorings and recursions to follow up on, in less frenetic contexts. Audiences heckle applicatives "oh no, its less than, star, greater than." Another is a roboticist in training. I've witnessed the state of robotics software and I'm worried for the robots more than anything. As the trans-human attain rights, one of the first will be a GHC install in their cores. And, me, I do tech support.

> We recognize that the Haskell community, echoing the technology industry more generally, skews white and male. We see it as our duty and honour to spread the joy of Haskell widely and to broaden the patterns of participation, in the hopes that, one day, we will no longer be askew. ~ [GRC](https://haskell.foundation/guidelines-for-respectful-communication/)

Not a bad definition of tech support.

This will come as no surprise to insiders, but within the very small circle of Haskell coders I know, the community is renown as a safe space for the gender queer and neuro-diverse. We don't like to brag, but tech support in Haskell has improved of late. Diversity is visible and being amplified.

Within this context of community and respect for diversity, there has been some recent active defence against gender and race baiting on fp slack. Some of our community has left, and some of us never arrived because it has never been a particularly safe space for them. Recent posts are trolling, exclusionary and cheap, and I hope we can find a better spot to chat about technicals.

Can we please think about this communication channel in light of the GCR? Given our makeup, these memetic viral loads coming in can pack a punch, and we need to develop strong and automatic immune responses.

## Haskell and the elegant attack.

Welcome back to the Happy Haskell HAckers Tour. As we approach our next stop, let me firstly apologise for the recent detours. Our bus driver, Yoneda, mixed up the maps and started following the GHC After Dark\[^after\] script. But now we're back on track, and coming up on your left is our monument to the Haskell `literals`:

``` example
-- | motto
--
-- We may need to think more about this (SPJ)
--
motto :: String
motto = "avoid success at all costs"
```

For a long time, through the darkness of pointfree, the incompatibility of lhs, and the golden age of DerivingVia, this `motto` has guided Haskell language design. If you peer closely, you'll notice some scratchings of the original work. These are various attempts at interpretation involving commas and parenthesis, by our bright but cheeky novitiates, but we've done our best to restore the work to its original intent.

Archivist retrieval digs have recently confirmed the original context of these words, and now believe it is not a `motto` at all, but part of a larger computation\[^trans\], one that has been recovered\[^onenote\] from a lost functional pearl\[^lits\]

``` haskell
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

Whilst we do not have time to cover all of the technical details\[^tech\], what has previously been described as the Haskell `motto`, is, in fact, simply a seed string that generates a lexicographically ordered list of aphorisms (the `literals`).

Note how the seed value is embedded directly in the computation and cannot be changed. One can imagine inserting another `motto` into the computation, but this would, literally, not be Haskell.

Note also that any combination disrespecting the lexicographic ordering of the seed literal is considered heresy, and will regardless be unavailable to the compiler. Problematic examples that have caused confusion in the past include "success avoid" and "costs avoid success."

But enough with the theory, if a volunteer would like to step up and spin the literal combinator, that big button marked `random-1.2.0`, let's meet the Haskell literals.

``` example
-- >>> literal 5
-- avoid success
```

Or, as Rudyard Kipling (an early adopter of category theory) wrote on the steps of Wimbledon, "meet with triumph and disaster, and treat those two impostors just the same (or at least, up to isomorphism)."

Quoting from the lost pearl, "maybe we should avoid labels such as success being placed on language design. Either they will turn out to be correct, in which case the label is redundant, or they will eventually need replacement, in which case the label becomes an impediment to adopting better ways."

``` example
-- >>> literal 1
-- success
```

My favourite literal, and one of the `singletons`. The singletons are best understood as interjections\[^bang\], so an accurate translation is actually `Success!`.

In the lost pearl, it reads literally as "joy of haskell." Some other interpretations include "I can't believe it compiles!", "Huzzah!", "Poggers! Let's goooo!" and, the dryer, "With Haskell, if you can get it to compile, it usually runs."

``` example
-- >>> literal 0
-- avoid
```

Or `Avoid!`, the head of the literals. To quote the ancients\[^alphabeta\], "In a functional language with lazy evaluation, we can go a step further and eliminate the recursive definitions … in favour of a more modular solution." A more modern version is "a bug can only exist if it's representable." When laziness is inherent and exposed in language design, we can all get to the point quicker and have some down time.

Avoid!

## The Elegant Attack Proclamation

``` example
-- >>> literal 30
-- avoid success at all costs
```

Now Yoneda and I could sit here all day, watching the combinator spin\[^spin\] its way through `Hask -> Hask`, but that's our day job, and some of you may be getting bored, so let's skip to the last of the `literals`.

In a recent enunciation\[^elegant\], `literal 30` was described in these terms:

> Haskell embodies a radical and elegant attack on the entire enterprise of writing software.

The elegance comes from how the literals combine to form this emerging narrative. For example, this is not the elegant attack but embodies it:

> Haskell is a non-commercial (avoid all costs\[^l20\]), volunteer army (success at all costs\[^l29\]) who would like coders (forall coders.\[^l3\]) everywhere (avoid success at\[^l15\]) to have rich (success at all\[^l21\]) lives, with less tedium (Avoid!) and the best (Success!) tools.

Please, a round of applause, and spare change if you have it, for our real-life combinatorial choreographers of language design, who juggle and balance the `literals` day in and day out, in their quest for software perfection.

And until next time, when we visit the GHC foundry, where tar balls are rendered and frozen, and libraries machined to isomorphic perfection, Yoneda and I will leave you be. Feel free to spin the literal combinator some more, and ponder the Haskell `literals` and the elegant attack they may formulate.

Finally, `literal 7` (avoid all)\[^apology\]

------------------------------------------------------------------------

\[^heinlein\]: As, Heinlein, one of our early adopters of `literal 15`, scribbled on the side of a punch card, "specialisation is for insects."

\[^l3\]: `literal 3: All!` Also written `forall coders.`

\[^l15\]: `literal 15: Avoid success at` Haskell is a general purpose language. Although we are quite good at parsing, do not box us in. Success at a particular endeavour does not necessarily mean we should specialise\[^heinlein\], but instead maybe work on our weaknesses.

\[^l20\]: `literal 20: Avoid all costs` Haskell, as a project, is poor and shall remain so. A price point of zero ensures wide adoption and a collegiate approach to the craft of coding.

\[^l21\]: `literal 21: Success at all` We would like coders to have balanced lives, with less boiler-plate, and for teams of coders to use and embrace diversity, so that success may be shared widely.

\[^l29\]: `literal 29: Success at all costs` For all that they are, this bunch of misfits is pretty focused on the tasks of writing software. They are not in the mix to take some small slice of existing commercial arrangements.

\[^poverty\]: Debate continues as to whether `literal 20` necessarily requires volunteers to also take a vow of poverty.

\[^alphabeta\]: R.S. Bird; John Hughes (1987). The alpha-beta algorithm: An exercise in program transformation.

\[^after\]: Unfortunately, the GHC After Dark tour is solidly booked due to unprecedented demand.

\[^onenote\]: Found in a recursively-defined Windows backup directory, of all places.

\[^trans\]: The code is not the exact original, but has had some doctrinal modification, including the qualified List obligation, the applicative noise injunction, the trailing operator cascade and, of course, monadic purification. The use of `[String]` may grate the modern ear, but the old interface is retained to allow interaction with the other Haskell List String mysteries.

\[^bang\]: An interjection or exclamation in English is similar to BangPatterns, but with StrictData also turned on, that can be used to interupt concurrent conversation.

\[^tech\]: Just as an aside, to quote from the lost pearl: "For the combinations operation, we use a functional algorithm similar in spirit to Knuth's algorithm R. The imperative version is also known as Knuth's revolving-door algorithm, but in the functional equivalent the door does not so much revolve as remain in a fix point in relation to the grey code generated, saving the computation costs of actually revolving the door.""

\[^spin\]: or remain a fix point in the natural transformation of category theory to software design, if you prefer.

\[^elegant\]: The Elegant Attack Proclamation can be found at [haskell.foundation](https://haskell.foundation), just above "Learn about Haskell."

\[^apology\]: For passengers who have specifically complained, again, apologies. For those complaining that they did not sign up for any tour business, this is, in reality, not a bus tour but an allusion to one. A sketch to set a satirical scene in which to portray the craft of software design. In my defence, the use of allusion and metaphor used to be stock in trade for a functional programmer back in the day. If you read any old functional pearl you will find an author playing with the English language alongside their functional language expressions. Put the fun back in function I say but, regardless, you are free to click away at any time. I won't name names, but I understand that, for specific cultures, satire is seen as equivalent to sarcasm. 40 years of SNL has robbed this culture of seeing any joy or point to parody, and that is sad. Treating subject matter whimsically is not equivalent to treating it as a joke. To the passenger who alluded to doxing Yoneda and I, on the main charge of sloppy writing, if I understand the complaint, and described uncertainty surrounding the fictional status of the piece as unbearable, I say good day ma'am, but your ride is over.

\[^lits\]: [GitHub - tonyday567/lits: The Haskell literals](https://github.com/tonyday567/lits)

## Coder Climate Action

I'd be interested in the zero-carbon path that the software industry anticipates, if there was such a thing. A professional guild of coders may be concerned for the havoc their energy consumptions are unleashing on the world. Unfortunately, coding is a new profession, with no guild or central craft, having sprung up quite recently due to the unreasonable success of a few mathematical logicians.

Instead, the people and the profession are cogs in a wider corporate machine. Our climate action plans are written for us; by sales most likely.

## Coder climate action

What might a coder call to climate action look like?

It would firstly recognise and seek to own the energy pathways that coders lay out, and add up just how much of a problem the industry is, in relation to everyone else. The most popular number is around 6% of total energy usage\[^wiki\]

\[^wiki\]: <https://en.wikipedia.md/wiki/IT_energy_management#cite_note-1>

This is a number from Gartner 2007, however, uncalculated since. The number, if you include household computation and allow for global growth in computation energy cost might be around 10%. The `computation` complex that includes software development, computers, the internet and media streams is about the same size as Aviation. As big a problem as planes.

And yet, if you look through the IEA\[^iea\] reports and models, the industry is classified ambiguously, as brown goods and what is value-added services. Software energy usage patterns are hidden within the fabric of the matrix, someone else's problems to solve.

\[^iea\]: <https://www.iea.md/reports/world-energy-model/documentation#world-energy-model>

## Industry privilege

This makes software a bit special because it is, in reality, a coherent project and industry, one of the few that can move the dial. And we've just seen the first ever reduction in human energy usage, the first of many perhaps, driven by software facilitating pandemic-response behavioural change. And at a speed no-one alive imagined possible. Software is talking a bow and leading the way.

This combination makes the industry powerful indeed. It can, for example, participating and celebrate the outrageous energy consumption plans of Bitcoin, some Utopian vanity project. Name another big energy vampire prepared to flaunt and moon us in the here and now. Big Software, as part of the Big Tech posse, will not only eat your world; but burn it too, because we can.

Helped by this invisible power source, the sum total of all green commitments across the entire computational complex is a bug.

Take Google\[^sustain\]. There's not a single commitment to actually reducing energy footprint in any way. When you commit to buying renewable energy rather than carbon, you help build confidence in the new technology at the margin, but you also push prices and people around so that the overall excess energy consumption just shifts somewhere else. Beyond this, they offer machine learning to the masses, helping cities and forests by providing ever more data to them, and branded machines who are learning how to help, via consumption of massive heterogeneous data sets (MHDSs).

\[^sustain\]: <https://sustainability.google/commitments/#leading-at-google>

## Welcome to the Sunlight Age.

Since Software doesn't seem to be on the field, they may not understand recent evolutions in potential climate action pathways, worked on by our best and brightest. Just like mathematical logic, solar PV and wind to a lesser extent has been unreasonably successful. So much so that, across the scenarios a consistent line is forming, a milestone in development, where energy usage will no longer be a problem. We will turn off the meter readers and allow anyone access to energy if they want it. Not because it will be free but because a metered system costs too much versus the societal base price.

Welcome to the Sunlight Age! That's where we want to get to and if we get to there, huzzah! Just like computation before it, the renewable endgame will close off another epoch - the one we are living in right now.

## The Great Carbon Squeeze

When people right now talk about trusting the science behind the climate crisis what they're really speaking to is our measurements of size, scope and the morose certainty of it all. The knowledge that this is happening at such scale gives it reliability, and extra oomph.

What will our era be like? Let's assume for a moment that our best and brightest are working on climate response pathways, and not trying to understand MHDSs. If you place trust in the pathways they map and evolve, between us and the Sunlight Age lies a gap. The gap is a shrinking carbon supply - the distance between total energy demanded and renewable energy available. There is an energy usage squeeze coming, beyond our experiences, as the fossil fuel economy is shut down.

So whether restricted by government decree, or by market forces, or famine and pestilence, or food security, a reliable prediction is that global energy usage is going down a lot. Trust the certainty of our Gaian measurements; the obvious consequences and likely action and response.

Within the Great Carbon Squeeze, how does the Software industry reduce energy use and be most helpful? As much fun as they will be, nanotech, quantum computers and other thermodynamic adventures towards the the von Neumann–Landauer bound will be more likely Sunlight Age past-times. We may have to cobble together something from what we have at hand.

We have Moore's law, in the physical infrastructure world, but it's looking shaky. Bottlenecks along multiple production paths and recent shortages signal wide systemic malaise. An entrepreneurial engine run out of puff.

## MHDSs

Is that it? We're as good as you get and you'll just have to live with less computation? From Walport's tour de force survey\[^walport\] of the intersection between thermodynamics and computation, here's a hint of another path:

> quantum computers as currently understood could only reduce the costs (and increase the speed) of an extremely limited set of possible computations, like factoring products of prime numbers. More importantly, many of the major challenges facing modern computing, both in terms of speed and thermodynamic costs, do not arise in the operation of the CPU (which is where quantum computation might be helpful), but rather in the i/o. In applications ranging from widely popularized applications of deep learning to search engines to simple back-office database management, modern computation exploits massive, heterogeneous data sets

Across a wide range of computations; management reporting, AI media selection, data mining and machine learnt business logic, MHDSs congregate. And in every case, you can liken them to a form of computational composting.

From inside looking out, the informational content in MHDS's is incredibly sparse. The likelihood that any particular piece of data in them matters, that it will ever effect the world, is vanishingly small. Machine learning tools like python and SQL run big sieves repeatedly through this low-nutrient soup in the hope of constructing a meal.

## Big State not Big Data

My main Machine Learning interface suggestion looks a bit like this:

``` haskell
-- | extracts information from lots of Stuff.
--
-- DEPRECATED: Current usage tends towards inputing lists (or streams) of Stuff
-- that are somewhat less than infinite in size, but only just.
--
-- Do not use this function if energy cost is at all an issue, or if you are working on dynamic or somewhat closed systems, where some of the Stuff happened far away and long ago.
unsafeExtract :: [Stuff] -> Summary Stuff

-- | safely traverse some Stuff to extract information.
--
-- Enforces on-the-fly compression of lots of Stuff into a hopefully snappier Summary.
safeExtract :: (Traversable T) => T Stuff -> (Stuff -> Summary Stuff -> Summary Stuff) -> Summary Stuff
```

If you muddle through an MHDS it becomes obvious that vast oceans of data have been collected shabbily and in haste. A record of historical summary has long been seen as a weakness in data science. An MHDS is low-level memory stripped of context and purpose. Machine learning needs to go live, becoming Summary State updates for other computations happening around them. It wouldn't need as much data, would be much faster and more timely, and may prove more useful.

In this world, MHDSs would undergo `compression` at source; take these large volume but ephemeral data streams and capture a current Summary, with the wider context and purpose to the best you can, so that the Summary can be improved over time. Let the rest go down the Sink.

The coding professions and craft have only a short window to recognise they're on the wrong side of the carbon divide, are laggards as a profession in real climate action, and could even be the bad guys. Saving all the Stuff, assuming near-infinite computational capacity and confidence in high fidelity reproduction of the past maybe a good approach in the Sunlight Age, but we're not there yet, and we may have to be more clever. We need to count our cycles and conserve our capacities, in the coming great carbon squeeze that we can now just feel.

\[^walport\]: Wolpert, David (2019). The stochastic thermodynamics of computation. Journal of Physics A: Mathematical and Theoretical, (), –. <doi:10.1088/1751-8121/ab0850>

\[^adjoint\]: From a category theory point of view, the Stuff has been cleaved by a left adjoint to Information, forgetting Meaning and Context.

## Consider the Ellipse

In the course of [chart-svg](https://hackage.haskell.md/package/chart-svg) development, I've considered the ellipse. Quite a bit. It's a weekender project run amok trampling on best laid plans, but life is art and I code in Haskell for art sake as much as commerce. Here's what I saw.

## Consider an angle

[numhask](https://hackage.haskell.md/package/numhask-0.7.1.0/docs/NumHask-Analysis-Metric.html#v:ray) contains a type class, Direction, which interfaces between Cartesian or spatial position and polar\[^jesuit\] co-ordinates, busting this information into direction and magnitude.

The best way to illustrate the Direction class is is via the Point instance in [numhask-space](https://hackage.haskell.md/package/numhask-space-0.7.1.0/docs/NumHask-Space-Point.html)\[^numhask-space\]:

``` haskell
instance (TrigField a) => Direction (Point a) a where
  angle (Point x y) = atan2 y x
  ray x = Point (cos x) (sin x)
```

Nothing new here, except for shepherding these into a class. In base, these are available for the Complex type and are called [phase](https://hackage.haskell.md/package/base-4.14.1.0/docs/src/Data.Complex.html#phase) and [cis](https://hackage.haskell.md/package/base-4.14.1.0/docs/src/Data.Complex.html#cis), a naming convention whose exact provenance is unknown to me, though I do admit to having gone through a cis phase. In linear, these are rendered as [angle](https://hackage.haskell.md/package/linear-1.21.5/docs/Linear-V2.html#v:angle) and [unangle](https://hackage.haskell.md/package/linear-1.21.5/docs/Linear-V2.html#v:unangle) in typical kmettian brutalism.

Checking in with wiki, an [angle](https://en.wikipedia.md/wiki/Angle), is formed by two rays.

![](file:///images/Two_rays_and_one_vertex.png)

This specification of Direction locks in a frame of reference where the ray 2 of the wiki diagram refers to `Point one zero` (aka along the positive x-axis). The decision as to whether positive y means up (sane people) or down (SVG) then spills over to angle specification, so that a positive angle means clockwise in SVG but is anti-clockwise in grade-school\[^purdy\] textbooks.

## Consider a circle.

Topologically, an ellipse is a circle. The unit circle is a circumference, a continuous set of points in a two dimensional frame with magnitude one.

Armed with ray we can approximate a unit circle as a list of points:

``` haskell
-- xs is a hundred Doubles forming a grid over [0, 2 * pi]
-- representing radians
xs = grid OuterPos (Range 0 (2 * pi)) 100 :: [Double]
unitCircle = ray <$> xs
```

and here's a picture:

![](file:///images/circle.svg)

## Consider the ellipse

In SVG land, an ellipse is then a circle that is:

- scaled by a Point (ie scaled across the x and y axis)
- rotated by an angle
- translated by a Point

For example, an ellipse with a major radius of `2`, a minor radius of `1`, rotated by `pi/4` from the x-axis, and centered on `Point 1 1` looks like:

``` haskell
rotate a b = norm b .* ray (a + angle b)
(Point 1 1 +) . rotate (pi/4) . ((Point 2 1) *) . ray <$> xs
```

![](file:///images/ellipse.svg)

## Position, position, position

The SVG standards choose to define an ellipse segment in terms of the (x,y) co-ordinates of the segment ends - sometimes called endpoint-based in the standards. This contrasts with sane engineering (taking advantage of angle maths, say) and the standards acknowledge this potential, and [define](https://www.w3.md/TR/SVG/implnote.html#ArcConversionCenterToEndpoint) conversion between angle and position pov's:

![](file:///images/b23.png)

In contrast, cleaning up the code tapped out above and adding some naming arrives at a competitive standard to this:

``` haskell
ellipse centroid radii major theta =
  rotate major (radii * ray theta) + centroid
```

Much neater.

The factory must grow, but nothing prepares you for the cthulhuian emergence that is then [B.2.4](https://www.w3.md/TR/SVG/implnote.html#ArcConversionEndpointToCenter):

![](file:///images/b24.png)

## Meanwhile …

the same computation expressed in Haskell …

``` haskell
theta_ centroid radii major pos =
  angle . (/radii) . rotate (-major) . (- centroid) $ pos
```

To find the angle, the ellipse transformations each have reversals that you apply to position (who would have thunk it). No trig bleeding out all over the spec polluting the essence of the computation.

Take another look at rotate:

``` haskell
rotate a b = norm b .* ray (a + angle b)
```

I see a heavy-hitting fusion recipe for turning trig calcs into addition, easily understood and able to be recreated in most languages.

The code, as it stands, has a good chance of actually doing a `sin` and `cos` calc exactly once (linear types might cement this guarantee in) whatever the complexity of it's context. The simplicity noted by mathematicians working on ellipses using polar co-ordinates directly translates into blindingly fast code, and this looks like the blueprint.

But that's not all! `atan` infinity bugs are confined to `ray` (`/radii` will also be problematic). `norm` has the square root aka "magnitude must be positive" bugs well located. No corner cases, no if buts or maybes.

No x or y subscripts in plain sight, at all. We can extend to 3D either cylindrically or spherically within this class without any api change. No matrix notation messing with our diverse notions of what they mean. No flag variables.

Let's make Haskell the lingua franca of the standards world.

\[^numhask-space\]: numhask-space is what you get if you extract all the maths needed for a typical chart library, and tried to make some sense of it. If you dig through d3, matplotlib, plotly, ggplot, svg even, you will see bespoke histogram collection, linear space grid calculation (adjusted for human sensibility) and linear algebra frameworks. It seems ad-hoc, but once you follow the threads and get to a [Space](https://hackage.haskell.md/package/numhask-space-0.7.1.0/docs/NumHask-Space.html) type, it coheres nicely.

\[^jesuit\]: polar co-ordination, separation of concerns really, was a Jesuit invention - see [greg](https://en.wikipedia.md/wiki/Gr%C3%A9goire_de_Saint-Vincent) et al - invented in a context of imminent colonisation plans and race for global domination.

\[^purdy\]: Ms Purdy taught me how to draw a 45 degree angle on a graph, and it's in the first quadrant, not the fourth. She was never wrong, and shot down aircraft in the blitz, so we didn't argue.

## Active Haskell Projects

A whirlwind, github tour of 4 active Haskell projects.

There's a lot happening in Haskell-land, and I thought I'd take stock of a few issues lists in github. Let's jump in.

## shellcheck

In other news, [shellcheck](https://github.com/koalaman/shellcheck) has overtaken [pandoc](https://github.com/jgm/pandoc) as the most [starred](https://github.com/search?q=stars%3A%3E300+language%3AHaskell&type=Repositories&ref=advsearch&l=Haskell&l=) Haskell github repository.

Looking through the readme, Haskell onboarding is first-class. Interesting that Cabal and stack are so highly aligned:

``` example
On systems with Cabal (installs to ~/.cabal/bin):

cabal update
cabal install ShellCheck
```

``` example
On systems with Stack (installs to ~/.local/bin):

stack update
stack install ShellCheck
```

The Windows install even includes the magical incantation `chcp 65001`, which I always have to recall on a fresh Windows install.

I had heard that rust was eating Haskell's lunch (in systems programming at least), but we're vibing on code analytics.

## pandoc

The grand old dame of Haskell projects, pandoc, has closed a page of [issues](https://github.com/jgm/pandoc/issues?q=is%3Aissue+is%3Aclosed) in the last 18 days - incredible active throughput. I use [pandoc-types](https://hackage.haskell.md/package/pandoc-types) extensively, and the last rewrite was sublime - native Pandoc gives me a lot of freedom. I do have to lug around the full pandoc to get the markdown+lhs+github readers & writers, though, and the pandoc compile chokes my travis builds.

## Tidal

[Tidal](https://github.com/tidalcycles/Tidal) is a massive success at 1.2k stars, and always super active, with 11 issues needing help. Tidal is at the core of my local music scene; meetups are load atom, and start making coding up music, doing experiments.

A typical email I will get from my daughter is:

``` example
d1
  $ density 1
  $ s "sn*100" -- Mess with number of samples,, up to audio rate (starts at like 100)
  # lpf "[50, 100, 500, 1500, 10000]"
  # lpq "0.7" -- Filter resonance
  # pan (stitch "[t f]*5" (range 0 0.5 saw) (range 1 0.5 saw))
  # gain "0.8"
```

That's it! The benefits of composition are there. You can see how tidal, the strings, are threaded through functional, transformation control.

## HLS

The [haskell-language-server](https://github.com/haskell/haskell-language-server#readme) has the most focus in the community right now. New versions pop out of a multitude of GUIs and features look easy to add with the plug-in vibe.

By now, one would assume that hie (aside from .hie files which will be baked in to ghc forever) and ghcide are integrated. From what I see, interests bifurcate but it's one solid project. There's a lot of deconstruction work on the ghcide side, of tests rearranged and supported, that would clean things up nicely.

In contrast, the [ghc issues board](https://gitlab.haskell.md/ghc/ghc/-/issues), looks like an emergency ward. Issue on issue demands such specialist knowledge - the adults are definitely in charge.

Now back to a Tidal [ticket](https://github.com/tidalcycles/Tidal/pull/766).

## lower-case haskell

Type-level trickery, UpperCase Haskell, is the showy rock-star of Haskell and all power to the Type. I like a good Type as much as anyone, but what keeps me using the language is the work-a-day value-level coding tool-kit - lower case haskell.

Let me explain.

In noodling around with [stan](https://hackage.haskell.md/package/stan), a wonderful tool developed with obvious care and attention by the talented [kowainik](https://kowainik.github.io/) troupe, I created [hcount](https://hackage.haskell.md/package/hcount), a cheap and cheerful attempt to count symbol usage in Haskell projects. Please try it out and let me know how it runs.

Here's my personal top 10:

``` haskell
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

You can tell so much about a coder from their top 10 list. I'm an unreconstructed `$` user after a decade of Haskell hacking: you write from right to left, plonk down a `$`, add the next thing, think some more, rinse and repeat. I use `<$>` extensively because of this style: I plonk down a `$` and then realise I am moving up a functorial level so wrap it to a `<$>`.

The very next refactor I have planned, however, is to replace each and every `<$>` with `fmap`. The flow I get from subbing `<$>` for `$` is almost always interrupted with having to then bracket everything on the right because of the change in fixity.

I have made some efforts to move to a more dotty approach and I suspect my `.` usage has risen of late. I also, somewhat unusually I suspect, commonly write left to right using `&`, the flipped `$`, especially when using lens. But I never use `>>>` (the flipped `.`) probably because of it's garish look.

I'm an unrepentant user of [OverloadedLabels](https://ghc.gitlab.haskell.md/ghc/doc/users_guide/exts/overloaded_labels.html), hence the `fromLabel` (which ghc inserts on my behalf) and `.~` usage. I tend towards single letter, anonymous local names and hope that my logic function blocks and Typing habits are simple enough to justify this (they're probably not). I'm a mathy coder given `*` is right up there (`+` is 14th). I love semigroup and look for it constantly.

## lower-case haskell

``` haskell
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

Once I filtered out the operators I was immediately struck by just how much love and respect I have for lower case haskell (except for `fromIntegral` which always grates given how loooong and boringly obvious it is). Why the love?

## [pure](https://hackage.haskell.md/package/base/docs/Prelude.html#v:pure)

I came late to the Haskell party, started up the typical learning curve of Monoid-Functor-Applicative-Monad and never quite made it to the top. Monads (or MoNaDs as they used to spell it) are just so 90s, so late Gen X - early Millennial. They remind me of that other 90s sickly-child that is Radiohead; if they saved rock and roll (twice!) then how come you can't dance to it?

Wading through other peoples' library monad wrappers ("i wanna have control"), part of the mess was the metaphysical-linguistic confusion of `return`. Where am I that I have to now return, I would ask ("i don't belong here"). The tutorials told me I was somewhere ("what the hell am i doing here"), somewhere special ("i wish i was special"), but now I need to go back to somewhere else ("but I'm a creeeeep!").

Don't @ me on this; I understand monads. I hate on 'em cause I don't like 'em.

Looking through my lower-range usage, I have one use of `=<<`, one of `>=>`, a single `>>=` and can't recall the signature for any of them.

Meanwhile `pure` means simple; it's only a value, gently lifted and placed into an Applicative. Rock on AMP!

## [bool](https://hackage.haskell.md/package/base/docs/Data-Bool.html#v:bool)

When I first learnt about this gem, this functional eliminator, I had an epiphany: if I used `bool` I would never ever again have to decide how to indent an `if`-`then`-`else` statement. I'd never even have to recall whether haskell has an else clause (of course it does because without it it's a partial but I'm forgetful and often dull).

Most of all, what happens when I code with `bool` is I stay in the flow compared with having to use my `if`-`then`-`else` fingers or construct a `case`-`blah`-`of`-`x->etc` monolith. I switch from *homo categoralis*, master of domain and codomain alike, to *homo narratavis*, teller of post-modern stories and (sometimes) bender of truthiness.

A nice extra of `bool` usage is cheap refactoring. You can cut and paste a bool statement, no matter the complexity, and stand a very good chance it will work in a new context, without moving brackets or indents or redesigning a case statement.

The only problem is nested `bool`'s start to get a bit hairy (though not as bad as nested if-then-else's). More than 2 layers of bool and it's time for guards.

## [maybe](https://hackage.haskell.md/package/base/docs/Prelude.html#v:maybe) & [either](https://hackage.haskell.md/package/base/docs/Prelude.html#v:either)

It took me years to discover `maybe` and `either`. I knew they were there but I didn't naturally reach for them. And then one day, neck deep in some transformer stack, looking up whether I needed to run my runEitherT before or after I execute my execMaybe, deleted the whole stack in anger and never looked back.

| Consider:  |                               |
|---|---|
|------------|-------------------------------|
| throwing   | `maybe (throw SomeException)` |
| defaulting | `either (const def)`          |
| maybeing   | `either (const Nothing) Just` |
| eithering  | `maybe (Left "error") Right`  |
there is an economy of compositional movement you don't get anywhere else.

## [zero](https://hackage.haskell.md/package/numhask/docs/NumHask-Algebra-Additive.html#v:zero) & [one](https://hackage.haskell.md/package/numhask/docs/NumHask-Algebra-Multiplicative.html#v:one)

In plain Haskell, there is no `zero` and there is no `one` - our ancestors weep at the loss. Sure we have the literate `0` & `1` which desugars to `fromInteger 0` & `fromInteger 1` but these are pale shadows of the twin unital gods of arithmetic.

Consider these examples:

``` haskell
-- what signum would look like if we had a zero and one
sign = bool (negate one) one . (>= zero)

-- conversion of boolean to a number to construct an identity matrix
ident = tabulate (bool zero one . isDiag)

-- applicative standard deviation
(\ss s -> (ss - s ** (one + one)) ** (one / (one + one))) <$> (** (one + one)) <*> id

-- boolean conversion to a number
bool zero one
```

In each, there is a sense of intent, of unital usage to shift domain, rather than the *just another magical number* feel that comes from using `0` or `1`.

## [first](https://hackage.haskell.md/package/base/docs/Data-Bifunctor.html#v:first) & [second](https://hackage.haskell.md/package/base/docs/Data-Bifunctor.html#v:second)

Another pair of terse, vital tools in my kit that are not even in prelude. They were trapped in the unfortunate arrows abstraction for a long while but find a nice home in bifunctors.

Consider adding commas to a number and fixing the decimal points:

``` haskell
addcomma n x =
  uncurry (<>) .
  second (take n) .
  first (reverse . intercalate "," . chunksOf 3 . reverse) .
  Text.breakOn "." $
  x
```

How do you write that without `first` & `second` (or bimap)? Only by busting the composition into components and exponentiating complexity.

Amazingly and somewhat mysteriously, they work with both tuples and Either, so you can refactor between the two.

I track two things at once so much in my code that [`<<*>>`](https://hackage.haskell.md/package/bifunctors/docs/Data-Biapplicative.html#v:), the biapplicative version of spaceship,is in my top 20. I bet others do too.

## [fmap](https://hackage.haskell.md/package/base/docs/Data-Functor.html#v:fmap)

For the most quint-essential function in all of haskelldom, `fmap` has the worst documented explanation. It starts with reference to -XApplicativeDo (scoring 0% proliferation on a recent [GHC2021](https://mail.haskell.md/pipermail/ghc-steering-committee/2020-November/001876.html) post), sugars the very definition being explained into do notation, and then talks self-referentially about an implied Functor constraint, when fmap is the sole operator of Functor. Never get a committee of fish to explain water.

But it's the best named, especially in comparison to the other maps in the other languages. This is the functor-map (personally, I pronounce it `f'n'map`) because, unlike where you're from, we have other ways to map. There is the [bimap](https://hackage.haskell.md/package/base/docs/Data-Bifunctor.html#v:bimap) of bifunctors, the [dimap](https://hackage.haskell.md/package/profunctors/docs/Data-Profunctor.html) of profunctors (with lmap and rmap), the hippy [contramap](https://hackage.haskell.md/package/contravariant-1.4.1/docs/Data-Functor-Contravariant.html#v:contramap), to say nothing of the various monomorphic maps such as [Map.map](https://hackage.haskell.md/package/containers/docs/Data-Map-Strict.html#v:map). And when others say map we instead often see traversing, lifting, sequencing, aligning or zipping. Haskell has 50 different names for mapping.

## [const](https://hackage.haskell.md/package/base/docs/Prelude.html#v:const) & [id](https://hackage.haskell.md/package/base/docs/Prelude.html#v:id)

I was surprised that neither `id` nor `const` made the top 10 (11th and 24th). I think that are more common in early code but get eventually factored out as polish occurs (eg `maybe def id` becomes `fromMaybe def`). These two, and their upper class cousins `Identity` and `Const` are what's most noticeably missing in other language constructs.

## A prediction

There's so much more I could that can be said from this simple, cheesy analysis. For instance, my heavy usage of `reverse` (19th most common) is no doubt a code smell. I get lazy and use lists where I should be using [Seq](https://hackage.haskell.md/package/containers/docs/Data-Sequence.html#t:Seq).

Instead, I'll hazard an adjunctive prediction. Starting from a position of [terrible](https://avi-d-coder.github.io/post/dear_haskell/), Haskell tooling has now moved into a zone of [getting there](https://www.reddit.com/r/haskell/comments/feptnt/is_haskell_tooling_lacking/). With the gap between ghc hacking and front-line tools having considerably shrunk, so much so that I can interface with the beast, then expect the Haskell user interface to evolve at speed.

And this may be a catalyst for ubiquitous adoption,, lower case haskell may yet have it's day to shine. If Haskell begins to turn GHC towards it's own behaviours and standards, and the community starts to apply it's categorical sharpness on the problem domain of software development. Watch this space and remember to upgrade your GUI's!

## mealy machines

For some reason the [folds](https://hackage.haskell.md/package/folds) library has never gotten much love, and it is beautiful.

I prefer a different ergonomics and an evocative name, so I newtype the [L1](https://hackage.haskell.md/package/folds-0.7.8/docs/Data-Fold-L1.html) there and call it a Mealy in the mealy library. It is a workhorse Category and Profunctor for me, and a delight to use.

The best way to explain might be to the correlation

## Introducing `harpie`

This library was formerly known as `numhask-array`, but numhask has been refactored out and so a name change was necessary.

The library uses uiua as a reference, and has been influenced by my attempt to reproduce the API of [uiua](https://www.uiua.md/).

`uiua` is a highly active open-source project exploring right-to-left, tacit coding for array programming and represents a high-quality target. An early-release of [huihua](https://github.com/tonyday567/huihua/issues) should be hitting hackage soon.

## backpropogate

In the process, I kept seeing a repeated pattern in many of the functions, where there might be opportunities for both vector streaming and laziness to kick in.

The original backpermute function is detailed in the conanical \[Regular, Shape-polymorphic, Parallel Arrays in Haskell\](<https://benl.ouroborus.net/papers/2010-rarrays/repa-icfp2010.pdf>) and would be implemented in harpie as:

\![alt text\](./backpermute.png)

Translated into more modern haskell, with a Representable, we get something like this:

\> repa<sub>backpermute</sub> f a = tabulate (f (shape a)) (index a . f)

The harpie backpermute splits the function application into two parts, the macro-shape change and the pre-indexing operation:

\> harpie<sub>backpermute</sub> f g a = tabulate (f (shape a)) (index a . g)

This, more general specification, should (one-day) allow more opportunities for a fusion rule to kick in:

\> forall f f' g g' (a :: forall a. Array a)). backpermute f g (backpermute f' g' a) == backpermute (f . f') (g . g') a

This uncoupling means that composition can go both ways: into shape-changing functions, and array programming is full of these opeartions - reversing, rotating, windowing; and pre-indexing. For instance, indexes:

\> indexes :: Dims -\> \[Int\] -\> Array a -\> Array a \> indexes ds xs a = backpermute (deleteDims ds) (insertDims ds xs) a

Taking an index involves a shape-changing `deleteDims` (what does the new array look like) and a pre-indexing `insertDims`, (where do I get the underlying data). These often form duals, and thinking about the opposite of everything gives a robust way to find efficient code.

Being flexible about **which** dimensions you are indexing also provides an ability to cross n-dim boundaries without touching the original array. Specialized n-array things like vectors, matrices and scalars often have specialized APIs, but, with backpermute, the library allows an identical API, with a reduced cognitive load.

## Upper-case versus lower-case Haskell

Type-level programming is a different language to value-level Haskell. Some of our younger cousins would be tempted to specify the indexing not using a list, but as itself an Array.

But you will never (ever?) have an Array at the type-level, really. Or, at least, a `[Nat]` for Type and an `[Int]` for value each hit a sweet spot for their respective language support.

The library is a corner solution to a large class of bug: discrepancies and drift between otherwise identical type-level and value-level computations. `Harpie.Shape` contains two versions of everthing to do with index manipulation at the value and type level.

## The Ugly

Unusually, I used first-class-families to impose some sanity on singleton-style coding. Type constraints form an important part of the type-level language canvas, and a defunctionalized approach creates good static guardrails.

There is, however, an unresolved [bug](https://github.com/tonyday567/harpie/issues/1) where `cabal repl` and `ghc --interactive -package harpie` lead to different answers:

\> cabal repl \> F.index x1 (fins @\[3,4\] \[0,1\]) \> 4

\> ghc –interactive -package harpie \> F.index x1 (fins @\[3,4\] \[0,1\]) \> 1

Just **wow**. How do I debug that?

## Harpie.Array versus Harpie.Fixed

The type-level shape and the type-level shape versions of an Array share an API, except where specialization is obvious. In the example below, a is a 3-dimensional Harpie.Fixed.Array

``` haskell-ng
-- >>> a = range @[2,3,4]
-- >>> a
-- [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23]
-- >>> pretty a
-- [[[0,1,2,3],
--   [4,5,6,7],
--   [8,9,10,11]],
--  [[12,13,14,15],
--   [16,17,18,19],
--   [20,21,22,23]]]
```

`range` is a highly useful function,, think \[0..(n-1)\].

``` haskell-ng
-- >>> pretty $ indices @[3,3]
-- [[[0,0],[0,1],[0,2]],
--  [[1,0],[1,1],[1,2]],
--  [[2,0],[2,1],[2,2]]]
```

So, to delete the index 3 (last) item on the last dimension (2, 3, `4`):

``` haskell-ng
-- | Delete along a dimension at a position.
--
-- >>> pretty $ delete (Dim @2) (UnsafeFin 3) a
-- [[[0,1,2],
--   [4,5,6],
--   [8,9,10]],
--  [[12,13,14],
--   [16,17,18],
--   [20,21,22]]]
delete ::
  forall d s s' p a.
  ( KnownNats s,
    KnownNats s',
    s' ~ Eval (DecAt d s),
    p ~ 1 + Eval (GetDim d s)
  ) =>
  Dim d ->
  Fin p ->
  Array s a ->
  Array s' a
delete Dim p a = unsafeBackpermute (\s -> bool (incAt d s) s (getDim d s < fromFin p)) a
  where
    d = valueOf @d
```

And here's the Harpie.Array version:

``` haskell-ng
-- | Delete along a dimension at a position.
--
-- >>> pretty $ delete 2 0 a
-- [[[1,2,3],
--   [5,6,7],
--   [9,10,11]],
--  [[13,14,15],
--   [17,18,19],
--   [21,22,23]]]
delete ::
  Dim ->
  Int ->
  Array a ->
  Array a
delete d i a = backpermute (decAt d) (\s -> bool (incAt d s) s (getDim d s < i)) a
```

Dimension and index input shifts from type to value; shape change in the constraints (DecAt) becomes decAt within the main body code. Pre-indexing (non-shape-changing) is essentially the same, with an extra check on index veracity at compile time, helped by also promoting getDim `p ~ 1 + Eval (GetDim d s)`.

## the `tabulate` pattern

Going through Representable can lead to a very direct, concise coding style.

`chol` is a good example. If we take the [Cholesky](https://en.wikipedia.md/wiki/Cholesky_decomposition#The_Cholesky_algorithm) algorithm:

``` haskell-ng
ghci> import Harpie.Fixed
ghci> import Harpie.Fixed as F
ghci>  e = F.array @[3,3] @Double [4,12,-16,12,37,-43,-16,-43,98]
ghci> l = F.chol e
ghci> import Prettyprinter (pretty)
ghci> pretty l
[[2.0,0.0,0.0],
 [6.0,1.0,0.0],
 [-8.0,5.0,3.0]]
```

## Introducing `box`

## box

``` haskell-ng
data Proxy a' a b' b m r
    = Request a' (a  -> Proxy a' a b' b m r )
| Respond b  (b' -> Proxy a' a b' b m r )
||
    | M          (m    (Proxy a' a b' b m r))
    | Pure    r```

``` haskell

```
