+++
title = "Active Haskell Projects"
author = "Tony Day"
date = 2021-01-22
lastmod = 2022-01-01
draft = false
tags = ["haskell"]
+++

A whirlwind, github tour of 4 active Haskell projects.

There's a lot happening in Haskell-land, and I thought I'd take stock of
a few issues lists in github. Let's jump in.


## shellcheck {#shellcheck}

In other news, [shellcheck](https://github.com/koalaman/shellcheck)
has overtaken [pandoc](https://github.com/jgm/pandoc) as the most
[starred](https://github.com/search?q=stars%3A%3E300+language%3AHaskell&type=Repositories&ref=advsearch&l=Haskell&l=)
Haskell github repository.

Looking through the readme, Haskell onboarding is first-class.
Interesting that Cabal and stack are so highly aligned:

```text
On systems with Cabal (installs to ~/.cabal/bin):

cabal update
cabal install ShellCheck
```

```text
On systems with Stack (installs to ~/.local/bin):

stack update
stack install ShellCheck
```

The Windows install even includes the magical incantation `chcp 65001`,
which I always have to recall on a fresh Windows install.

I had heard that rust was eating Haskell's lunch (in systems programming
at least), but we're vibing on code analytics.


## pandoc {#pandoc}

The grand old dame of Haskell projects, pandoc, has closed a page of
[issues](https://github.com/jgm/pandoc/issues?q=is%3Aissue+is%3Aclosed)
in the last 18 days - incredible active throughput. I use
[pandoc-types](https://hackage.haskell.org/package/pandoc-types)
extensively, and the last rewrite was sublime - native Pandoc gives me a
lot of freedom. I do have to lug around the full pandoc to get the
markdown+lhs+github readers & writers, though, and the pandoc compile
chokes my travis builds.


## Tidal {#tidal}

[Tidal](https://github.com/tidalcycles/Tidal) is a massive success at 1.2k stars, and always super active, with 11 issues needing help.
Tidal is at the core of my local music scene; meetups are load atom, and
start making coding up music, doing experiments.

A typical email I will get from my daughter is:

```text
d1
  $ density 1
  $ s "sn*100" -- Mess with number of samples,, up to audio rate (starts at like 100)
  # lpf "[50, 100, 500, 1500, 10000]"
  # lpq "0.7" -- Filter resonance
  # pan (stitch "[t f]*5" (range 0 0.5 saw) (range 1 0.5 saw))
  # gain "0.8"
```

That's it! The benefits of composition are there. You can see how tidal,
the strings, are threaded through functional, transformation control.


## HLS {#hls}

The
[haskell-language-server](https://github.com/haskell/haskell-language-server#readme)
has the most focus in the community right now. New versions pop out of a
multitude of GUIs and features look easy to add with the plug-in vibe.

By now, one would assume that hie (aside from .hie files which will be
baked in to ghc forever) and ghcide are integrated. From what I see,
interests bifurcate but it's one solid project. There's a lot of
deconstruction work on the ghcide side, of tests rearranged and
supported, that would clean things up nicely.

In contrast, the [ghc
issues board](https://gitlab.haskell.org/ghc/ghc/-/issues), looks like an emergency ward. Issue on issue demands
such specialist knowledge - the adults are definitely in charge.

Now back to a Tidal
[ticket](https://github.com/tidalcycles/Tidal/pull/766).
