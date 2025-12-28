+++
title = "Coder Climate Action"
author = "Tony Day"
date = 2021-06-17
lastmod = 2022-01-01
draft = false
tags = ["climate"]
+++

I'd be interested in the zero-carbon path that the software industry
anticipates, if there was such a thing. A professional guild of coders
may be concerned for the havoc their energy consumptions are unleashing
on the world. Unfortunately, coding is a new profession, with no guild
or central craft, having sprung up quite recently due to the
unreasonable success of a few mathematical logicians.

Instead, the people and the profession are cogs in a wider corporate
machine. Our climate action plans are written for us; by sales most
likely.


## Coder climate action {#coder-climate-action}

What might a coder call to climate action look like?

It would firstly recognise and seek to own the energy pathways that
coders lay out, and add up just how much of a problem the industry is,
in relation to everyone else. The most popular number is around 6% of
total energy
usage[^wiki]

[^wiki]: <https://en.wikipedia.org/wiki/IT%5Fenergy%5Fmanagement#cite%5Fnote-1>

This is a number from Gartner 2007, however, uncalculated since. The
number, if you include household computation and allow for global growth
in computation energy cost might be around 10%. The `computation`
complex that includes software development, computers, the internet and
media streams is about the same size as Aviation. As big a problem as
planes.

And yet, if you look through the
IEA[^iea] reports and models, the industry is classified ambiguously, as brown
goods and what is value-added services. Software energy usage patterns
are hidden within the fabric of the matrix, someone else's problems to
solve.

[^iea]: <https://www.iea.org/reports/world-energy-model/documentation#world-energy-model>


## Industry privilege {#industry-privilege}

This makes software a bit special because it is, in reality, a coherent
project and industry, one of the few that can move the dial. And we've
just seen the first ever reduction in human energy usage, the first of
many perhaps, driven by software facilitating pandemic-response
behavioural change. And at a speed no-one alive imagined possible.
Software is talking a bow and leading the way.

This combination makes the industry powerful indeed. It can, for
example, participating and celebrate the outrageous energy consumption
plans of Bitcoin, some Utopian vanity project. Name another big energy
vampire prepared to flaunt and moon us in the here and now. Big
Software, as part of the Big Tech posse, will not only eat your world;
but burn it too, because we can.

Helped by this invisible power source, the sum total of all green
commitments across the entire computational complex is a bug.

Take Google[^sustain]. There's not a single commitment to actually reducing energy footprint in any way. When you commit to buying renewable energy rather than carbon,
you help build confidence in the new technology at the margin, but you
also push prices and people around so that the overall excess energy
consumption just shifts somewhere else. Beyond this, they offer machine
learning to the masses, helping cities and forests by providing ever
more data to them, and branded machines who are learning how to help,
via consumption of massive heterogeneous data sets (MHDSs).

[^sustain]: <https://sustainability.google/commitments/#leading-at-google>


## Welcome to the Sunlight Age. {#welcome-to-the-sunlight-age-dot}

Since Software doesn't seem to be on the field, they may not understand
recent evolutions in potential climate action pathways, worked on by our
best and brightest. Just like mathematical logic, solar PV and wind to a
lesser extent has been unreasonably successful. So much so that, across the
scenarios a consistent line is forming, a milestone in development,
where energy usage will no longer be a problem. We will turn off the
meter readers and allow anyone access to energy if they want it. Not
because it will be free but because a metered system costs too much
versus the societal base price.

Welcome to the Sunlight Age! That's where we want to get to and if we
get to there, huzzah! Just like computation before it, the renewable
endgame will close off another epoch - the one we are living in right
now.


## The Great Carbon Squeeze {#the-great-carbon-squeeze}

When people right now talk about trusting the science behind the climate
crisis what they're really speaking to is our measurements of size,
scope and the morose certainty of it all. The knowledge that this is
happening at such scale gives it reliability, and extra oomph.

What will our era be like? Let's assume for a moment that our best and
brightest are working on climate response pathways, and not trying to
understand MHDSs. If you place trust in the pathways they map and
evolve, between us and the Sunlight Age lies a gap. The gap is a
shrinking carbon supply - the distance between total energy demanded and
renewable energy available. There is an energy usage squeeze coming,
beyond our experiences, as the fossil fuel economy is shut down.

So whether restricted by government decree, or by market forces, or
famine and pestilence, or food security, a reliable prediction is that
global energy usage is going down a lot. Trust the certainty of our
Gaian measurements; the obvious consequences and likely action and
response.

Within the Great Carbon Squeeze, how does the Software industry reduce
energy use and be most helpful? As much fun as they will be, nanotech,
quantum computers and other thermodynamic adventures towards the the von
Neumann--Landauer bound will be more likely Sunlight Age past-times. We
may have to cobble together something from what we have at hand.

We have Moore's law, in the physical infrastructure world, but it's
looking shaky. Bottlenecks along multiple production paths and recent
shortages signal wide systemic malaise. An entrepreneurial engine run out
of puff.


## MHDSs {#mhdss}

Is that it? We're as good as you get and you'll just have to live with
less computation? From Walport's tour de force survey[^walport] of the
intersection between thermodynamics and computation, here's a hint of
another path:

> quantum computers as currently understood could only reduce the costs
> (and increase the speed) of an extremely limited set of possible
> computations, like factoring products of prime numbers. More
> importantly, many of the major challenges facing modern computing,
> both in terms of speed and thermodynamic costs, do not arise in the
> operation of the CPU (which is where quantum computation might be
> helpful), but rather in the i/o. In applications ranging from widely
> popularized applications of deep learning to search engines to simple
> back-office database management, modern computation exploits massive,
> heterogeneous data sets

Across a wide range of computations; management reporting, AI media
selection, data mining and machine learnt business logic, MHDSs
congregate. And in every case, you can liken them to a form of
computational composting.

From inside looking out, the informational content in MHDS's is
incredibly sparse. The likelihood that any particular piece of data in
them matters, that it will ever effect the world, is vanishingly small.
Machine learning tools like python and SQL run big sieves repeatedly
through this low-nutrient soup in the hope of constructing a meal.


## Big State not Big Data {#big-state-not-big-data}

My main Machine Learning interface suggestion looks a bit like this:

```haskell
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

If you muddle through an MHDS it becomes obvious that vast oceans of
data have been collected shabbily and in haste. A record of historical
summary has long been seen as a weakness in data science. An MHDS is
low-level memory stripped of context and purpose. Machine learning needs
to go live, becoming Summary State updates for other computations
happening around them. It wouldn't need as much data, would be much
faster and more timely, and may prove more useful.

In this world, MHDSs would undergo `compression` at source; take these
large volume but ephemeral data streams and capture a current Summary,
with the wider context and purpose to the best you can, so that the
Summary can be improved over time. Let the rest go down the Sink.

The coding professions and craft have only a short window to recognise
they're on the wrong side of the carbon divide, are laggards as a
profession in real climate action, and could even be the bad guys.
Saving all the Stuff, assuming near-infinite computational capacity and
confidence in high fidelity reproduction of the past maybe a good
approach in the Sunlight Age, but we're not there yet, and we may have
to be more clever. We need to count our cycles and conserve our
capacities, in the coming great carbon squeeze that we can now just
feel.

[^walport]: Wolpert, David (2019). The stochastic thermodynamics of
computation. Journal of Physics A: Mathematical and Theoretical, (), --.
doi:10.1088/1751-8121/ab0850

[^adjoint]: From a category theory point of view, the Stuff has been
cleaved by a left adjoint to Information, forgetting Meaning and
Context.
