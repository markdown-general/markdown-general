---
title: "ToDo new project ideas"
date: 2025-12-23
---

# ToDo new project ideas

## palette generator

[How to pick more beautiful colors for your data visualizations -
Datawrapper …](https://blog.datawrapper.de/beautifulcolors/)

- combine colors with different lightness
- avoid bright, saturated colors
- concentrate in the color wheel
  - complementary (\*2 close) should score highly
  - off by a bit (in hue just a bit)
- don't use forest green 90 to 150, peak at 120 lighten it but also
  desaturate
- avoid 0 60 120 180 240 360
  - tune down saturation
  - darken them
- vary grayscale, avoid light, saturated

Get a generator like this going

## calculational

## functorof library

## FunctorOf notes

icelandjack

[icelandjack 6 years oga on
reddit](https://www.reddit.com/r/haskell/comments/eoo16m/base_category_polymorphic_functor_and_functorof/)
[Type class backend: how to evolve with
class](https://www.reddit.com/r/haskell/comments/pqhivm/type_class_backend_how_to_evolve_with_class/)
[2 years ago
…](https://www.reddit.com/r/haskell/comments/12y74y2/examples_of_categorical_functors_in_haskell/)

eviefp

[Functor-Of](https://eevie.ro/posts/2019-05-12-functor-of.html) full
code at: <https://github.com/eviefp/functorof/tree/master>

[Notions of Computations as
Monoids](~/org/adjunction.org::*Notions of Computations as Monoids)

The meow method for variable number of input types:

<https://www.reddit.com/r/haskell/comments/1kwwgfw/datayoneda_vs_dataprofunctoryoneda/>

``` haskell
Functor            = FunctorOf (->) (->)
Contravariant      = FunctorOf (<˗) (->)
Bifunctor          = FunctorOf (->) (->) (->)
Profunctor         = FunctorOf (<˗) (->) (->)
FFunctor           = FunctorOf (~>) (->)
HFunctor           = FunctorOf (~>) (->) (->)
ExpFunctor         = FunctorOf (<->) (->)
Linear.Functor     = FunctorOf (#->) (#->)
FunctorWithIndex i = FunctorOf (Cayley (i ->) (->)) (->)
Filterable         = FunctorOf (Kleisli Maybe) (->)
```

### libraries

<https://hackage.haskell.org/package/functor-combinators-0.4.1.2/docs/Data-HFunctor.html>
<https://hackage-content.haskell.org/package/yaya-0.6.2.3/docs/Yaya-Functor.html>

## learning core

<https://www.scs.stanford.edu/14sp-cs240h/slides/ghc-compiler.html>

ghc-core

``` bash
ghc-core --no-asm --no-cast -- -dsuppress-var-kinds -dsuppress-type-applications -dsuppress-uniques Foo.hs
```

<https://www.schoolofhaskell.com/user/thoughtpolice/using-reflection>

odd ghc message

``` bash
harpie git:(bugsimp) ✗ runghc -- -fforce-recomp test/harpie-issue1-simp
SNats @[5, 6, 7]
[5,6,7]
[5,6,7]
[5,6,7]
[]
[]
[]
ghc: internal error: evacuate: strange closure type 2669768
    (GHC version 9.12.2 for aarch64_apple_darwin)
    Please report this as a GHC bug:  https://www.haskell.org/ghc/reportabug
[1]    52725 abort      runghc -- -fforce-recomp test/harpie-issue1-simp
```

## stencil calc for harpie

From about 50mins a really interesting explanation of stencil calc for
harpie. <https://www.youtube.com/live/GIr_Ww0Hv0I>

## charting category theory

[Adventures in Category Theory - The algebra of
types](https://miklos-martin.github.io/learn/fp/category-theory/2018/02/01/adventures-in-category-theory-the-algebra-of-types.html)

### Bruno blog post

<https://www.brunogavranovic.com/posts/2022-02-10-optics-vs-lenses-operationally.html>

### fresnel

[GitHub - fresnel/fresnel: high-powered optics in a small
package](https://github.com/fresnel/fresnel)

## relations

Contains a bigup for point-free applicable to harpie.

[A Short Skinny on Relations & the Algebra of Programming \| Hey There
Buddo!](http://www.philipzucker.com/a-short-skinny-on-relations-towards-the-algebra-of-programming/?preview=true)

## Tristero

📯

### FFunctor

from yaya

``` haskell
class DFunctor (d :: (* -> *) -> *) where
  dmap :: (forall x. f x -> g x) -> d f -> d g
```

``` haskell
-- | An endofunctor in the category of endofunctors.
--
--  __NB__: This is similar to `Control.Monad.Morph.MFunctor` /
--         `Control.Monad.Morph.hoist` from mmorph, but without the `Monad`
--          constraint on `f`.
class HFunctor (h :: (* -> *) -> * -> *) where
  hmap :: (forall x. f x -> g x) -> h f a -> h g a
```

from hkd

``` haskell
class FFunctor (t :: (k -> Type) -> Type) where
  ffmap :: (f ~> g) -> t f -> t g
```

and Some is used to hide the a

### nucleus

[{2004.05631} At the Interface of Algebra and
Statistics](https://arxiv.org/abs/2004.05631)

[{2004.07353} The nucleus of an adjunction and the Street monad on
monads](https://arxiv.org/abs/2004.07353)

[The Nucleus of a Profunctor: Some Categorified Linear Algebra \| The
n-Categor…](https://golem.ph.utexas.edu/category/2013/08/the_nucleus_of_a_profunctor_so.html)

[Nucleus of a profunctor ·
GitHub](https://gist.github.com/sjoerdvisscher/749bf3ce500d06253ed26b7c75daaf47)

<https://twitter.com/sjoerd_visscher/status/1394379344743702530>

### Hyperfunctions

[Hyperfunctions - Donnacha Oisín
Kidney](https://doisinkidney.com/posts/2021-03-14-hyperfunctions.html)

``` haskell
newtype Nucleus p b = Nucleus (forall a. Nucleus (Flip p) a -> p a b)
```

``` haskell
newtype Hyper p a b = Hyper (Hyper (Flip p) a b -> p a b)
newtype Hyper (Flip Const) a b = Hyper (Hyper (Flip (Flip Const)) a b -> (Flip Const) a b)
```

[Control.Monad.Trans.Select](https://hackage.haskell.org/package/transformers-0.6.0.2/docs/Control-Monad-Trans-Select.html)

``` haskell
newtype Select r m a = Select { runSelect :: (a -> m r) -> m a }
```

``` haskell
instance Profunctor Hyper where
  dimap f g h = Hyper $ g . invoke h . dimap g f
  lmap f h = Hyper $ invoke h . rmap f
  rmap f h = Hyper $ f . invoke h . lmap f
```

``` haskell
Hyper a a ~ Fix (Cont a)
```

[Hyperfunctions :
haskell](https://www.reddit.com/r/haskell/comments/m5lb97/hyperfunctions/)

[GitHub - lamdu/hypertypes: Hypertypes - generic programming for
heterogeneous…](https://github.com/lamdu/hypertypes)

### zanz steps

1.  Step 1 - Recursion Schemes

    free algebras and folds over them, and the sense in which this is
    'universal', ie how all functions over a free monoid can be
    expressed using a fold.

    [Functional Programming with Bananas, Lenses, Envelopes and Barbed
    Wire](https://maartenfokkinga.github.io/utwente/mmf91m.pdf)

    <https://blog.sumtypeofway.com/posts/introduction-to-recursion-schemes.html>

2.  Step 2 - Fitting effects into recursion scvhemes

    Monads, applicatives, etc are also just monoids/etc in the functor
    category.

    Generic folds can evaluate monads with the same fold that we use to
    evaluate regular monoids.

    A lot of the theory behind this is folklore but no one's figured out
    a nice enough way to implement this in a way that's ergonimic and
    practical. Here's a good write up of one approach
    <https://oleg.fi/gists/posts/2018-02-21-single-free.html>

    And the theory is explored more in the paper "notions of computation
    as monoids" and follow-ups, ie the one on near-semirings.

3.  Step 3 - "monoids in the category of functors"

    <https://www.reddit.com/r/haskell/comments/1ks6z45/functional_pearl_f_for_functor/>

    This is what gives relative monads.

    "FunctorOf" ie 'the proper categorical functor'. Here's a good
    implementation in Haskell: (Though I want to do this using free
    functors ie Coyoneda rather than typeclasses)

    <https://cvlad.info/posts/2019-05-12-functor-of.html>

    The advantage of this approach is that it subsumes profunctors as
    just a special kind of functor. so if monads are monoids in the cat
    of endofunctors, and arrows are monoids in the cat of profunctors,
    relative monads generalise both. so another potential output of this
    work is to find combinators for composing relative monads that would
    work similarly to the arrow hierarchy

4.  Alternative

    take 1

    - we can get a kleisli category from a monad
    - this kleilsli category has structure that corresponds to the
      structure of the monad
    - arrows have a kind of kleisli category associated with them,
      "freyd category" - they also have the same correspondence, the
      structure of an arrow (or the underlying profunctor) determines
      the structure of the kleisli category.
    - 'Arrow' is Free (Coyoneda (Profunctor))
    - The free profunctor is FreeA(Coyoneda(ProfExpr)) (over ProfExpr)
    - ProfExpr : (p : Type-\>Type-\>Type) -\> (a:Type) -\> (b:Type) -\>
      Type (needs a GADT)
    - universal arrow: if you define a data structure, then **any**
      function on that data structure can be done using a fold.
      contravariance: But if the data structure contains contravariance,
      you can't use foldr/recursion schemes. A fold (aka catamorphism)
      is a universal way to deconstruct something and use it in
      computation, but foldr only works on bifunctorial, covariant
      patterns.

    take 2 So the way we construct a freer monad is by starting with a
    GADT for the syntax, which has type Expr : (f:Type-\>Type) -\>
    (a:Type) -\> Type then we wrap it with "free functor/coyoneda"
    Coyoneda :: (b -\> a) -\> f b -\> Coyoneda f a and then we wrap that
    with the free monoid in the category of endofunctors, ie a free
    monad so Free(Coyoneda(Expr)) gives us the free monad over Expr.

    Compare with: <http://okmij.org/ftp/Haskell/extensible/more.pdf>
    Now, the same thing can be made analogously for profunctors. The key
    step is that we need a GADT that has both a covariant and
    contravariant argument ProfExpr : (p : Type-\>Type-\>Type) -\>
    (a:Type) -\> (b:Type) -\> Type then we wrap this with the
    'profunctor yoneda' data Coyoneda p a b where Coyoneda :: (a -\> x)
    -\> (y -\> b) -\> p x y -\> Coyoneda p a b which gives us the 'free
    profunctor' and all we need to do after that is wrap this with 'the
    free monoid in the category of profunctors', ie the free arrow
    FreeA(Coyoneda(ProfExpr)) eg. so a closure/fold over
    FreeA(Coyoneda(ProfExpr)) will give us "operational arrows"

### essential tristero references

[Notions of Computations as
Monoids](adjunction.org::*Notions of Computations as Monoids) [Essence
of AD](adjunction.org::*Essence of AD) [What You Needa Know about
Yoneda](https://dl.acm.org/doi/pdf/10.1145/3236779) [profunctor
optics](http://www.cs.ox.ac.uk/people/jeremy.gibbons/publications/poptics.pdf)
[Arrows, like Monads, are
Monoids](https://www.pure.ed.ac.uk/ws/portalfiles/portal/22099462/http//www.haskell.org/arrow.pdf)
[Compiling to
Categories](http://conal.net/papers/compiling-to-categories/)

### Reflection without Remorse notes

<https://stackoverflow.com/questions/45334985/when-to-use-cps-vs-codensity-vs-reflection-without-remorse-in-haskell>

Reflection without Remorse paper:

<https://okmij.org/ftp/Haskell/zseq.pdf>

<https://www.janis-voigtlaender.eu/papers/AsymptoticImprovementOfComputationsOverFreeMonads.pdf>

Finite State Transducer explanation (base functor)

<https://stackoverflow.com/questions/27997155/finite-state-transducers-in-haskell>

### coroutines

pipes are [coroutines](https://en.wikipedia.org/wiki/Coroutine).

## haskell-lite

### comint

[Painless Emacs shell commands \| Eigenbahn
blog](https://www.eigenbahn.com/2020/01/19/painless-emacs-shell-commands)

[Deep diving into a major mode - Part 2 (IDE Features) \| Modern
Emacs](http://www.modernemacs.com/post/major-mode-part-2/)

<https://github.com/haskell/haskell-mode/blob/master/haskell-interactive-mode.el>

### comint-preoutput-filters

- comint-preoutput-filters (add-hook 'comint-preoutput-filter-functions
  \#'haskell-lite-repl-result-save) (add-hook
  'comint-preoutput-filter-functions \#'haskell-lite-repl-error))

### babel notes

- [ ] comint-input
- [ ] try full ruby method
- [ ] add processed-params

Using comint

``` elisp
(org-babel-comint-with-output
    (insert full-body "\n" eoe)
    (comint-send-input nil t))
```

Using haskell-ng-repl

``` elisp
(org-babel-comint-with-output
   (haskell-ng-repl-buffer-name ob-haskell-ng-eoe t full-body)
  (haskell-ng-repl--send-string raw-input)))
```

### treesitter references

[Making an Emacs major mode for Cabal using
tree-sitter](https://magnus.therning.org/2023-03-22-making-an-emacs-major-mode-for-cabal-using-tree-sitter.html)
[Tree-sitter in Emacs 29 and
Beyond](https://archive.casouri.cc/note/2023/tree-sitter-in-emacs-29/index.html)

### async

asynchronous: register a function with comint-output-filter-functions.
synchronous: use accept-process-output

1.  cider

    [cider/cider-mode.el at master · clojure-emacs/cider ·
    GitHub](https://github.com/clojure-emacs/cider/blob/master/cider-mode.el)

2.  inf-haskell

    inferior-haskell-get-result

    - key sync loop. Uses inferior-haskell-no-result-return (async)
      internally.
    - also uses haskell-extract-exp

    inferior-haskell-result-history

    accept-process-output - basic blocking, waiting for output

3.  fd-haskell-comint

    - haskell-shell-send-string-no-output Synchronous process Neither
      the input nor the output make it to the process buffer. uses
      accept-process-output

    - haskell-shell-send-string Asynchronous Output appears in process
      buffer (but not original input)

### big picture guess

1.  in-scope

    - repl Functionality not yet in haskell-ng bring restart eval prompt
      error management
    - cabal (see
      [rustic-cargo.el](https://github.com/brotzeit/rustic/blob/master/rustic-cargo.el))
      runner (preset options for cabal) compile create/manage project
      root finding
    - org-babel
    - hoogle
    - hackage/stackage
    - doctor

2.  out-of-scope

    - syntax (via treesit)
      - fontlock
      - faces
      - regexps
    - completion (via lsp)
      - keyword lists
      - pragmas
      - [horellana/company-ghci](https://github.com/horellana/company-ghci)
    - navigation (via treesit)
      - motion
      - structure definition blocking, folding
      - indentation
    - flymake/flycheck (lsp)
    - documentation (lsp)
    - tags [MarcWeber/hasktags#81 Tracking: Maintenance and
      modernization](https://github.com/MarcWeber/hasktags/issues/81)
      [GitHub - arybczak/ghc-tags: A command line tool that leverages
      GHC API for ge…](https://github.com/arybczak/ghc-tags)

### comint

[haskell/haskell-mode#1547 Comint everywhere by
vasanthaganeshk](https://github.com/haskell/haskell-mode/pull/1547)
[Comint: Writing your own Command Interpreter - Mastering
Emacs](https://masteringemacs.org/article/comint-writing-command-interpreter)

1.  comint audit

    output arrives buffered. The most reliable way to process it, is to
    append it to the buffer.

    [subprocess - Asynchronously wait for output from a comint process -
    Emacs
    Sta…](https://emacs.stackexchange.com/questions/692/asynchronously-wait-for-output-from-a-comint-process)

    - comint-show-output This function is independent of the
      input/output process

    - comint-send-input bound to \<RET\> calls variable
      comint-get-old-input

    - comint-output-filter usage?

    - comint-send-string usage?

    - comint-check-source

    - comint-default-source

    - comint-proc-query Canonical sync process

    - comint-accumulate for multiline?

    - process-mark The process mark separates output, and input already
      sent, from input that has not yet been sent.

      [helm/helm-comint.el at master · emacs-helm/helm ·
      GitHub](https://github.com/emacs-helm/helm/blob/master/helm-comint.el)

### org-dev

debugging org-node babel haskell.

1.  double square brackets

    Double square brackets gets interpreted as a link, even in results,
    so that they visibley disappear in results.

    `SPC m l t` org-toggle-link-display to see them properly.

2.  comint-prompt-regexp

    comint-prompt-regexp gets set by fd-haskell.

    to reset the below needs to be run in the `*haskell*` process buffer

    ``` elisp
    (setq comint-prompt-regexp "ghci> ")
    ```

3.  org-babel-interpret-haskell

    Assumes an inferior-haskell-process is happening, and adds this:

    ``` elisp
    (setqlocal comint-prompt-regexp (concat haskell-prompt-regexp "\\|^λ?> "))
    ```

    But not sure it matters …

    [emacs - How to set up org-babel for Haskell with Stack - Stack
    Overflow](https://stackoverflow.com/questions/42081379/how-to-set-up-org-babel-for-haskell-with-stack)

4.  Avoiding the swallowing of "\> "

    Use "ghci\> " as a prompt

    ``` haskell
    :t id
    ```

5.  other than "ghci\> " prompt bug

    ``` haskell
    :set prompt "ghci> "
    -- :set prompt "λ "
    ```

    ``` haskell
    :t id
    ```

6.  function call sites

    org-babel-interpret-haskell org-babel-comint-with-output

7.  ob-haskell bug thread

    <https://list.orgmode.org/E1otE9z-000372-Gn@lists.gnu.org/T/>

8.  debug technique

    [Macro
    Expansion](https://www.gnu.org/software/emacs/manual/html_node/elisp/Expansion.html)

    from <https://list.orgmode.org/87vadbcj3b.fsf@gmail.com/T/>

    ``` elisp
    (org-babel-comint-with-output
         ("*haskell*" (format "%S" org-haskell-eoe) t ":t id")
       (mapc
        (lambda (line)
          (insert (org-babel-chomp line))
          (comint-send-input nil t)))))
    ```

### doom sync bug notes on haskell-ng-mode

What does doom sync do? Byte compile?

> x There was an unexpected runtime error Message: Symbol's function
> definition is void Details: (treesit-font-lock-rules)

Possible solutions:

- [x] try loading haskell-ng after startup haskell-ng-mode.el evaluates
  fine if loaded manually
- [ ] new install of emacs 29.1 follwing: [How to Get Started with
  Tree-Sitter - Mastering
  Emacs](https://www.masteringemacs.org/article/how-to-get-started-tree-sitter)
- [x] treesit or something else needs manual upgrade eg
  [doomemacs/doomemacs#4950 \`void-function straight–process-buffer\`
  error on \`d…](https://github.com/doomemacs/doomemacs/issues/4950)
- [x] turn tree-sitter on in doom? No change when haskell-ng package is
  included

### examples of ob-haskell usage

[GitHub - lurdan/ob-typescript: Emacs org-mode babel support for
typescript](https://github.com/lurdan/ob-typescript)

[~bzg/worg: org-contrib/babel/ob-template.el - sourcehut
git](https://git.sr.ht/~bzg/worg/tree/master/item/org-contrib/babel/ob-template.el)

[~bzg/org-contrib - Contributed packages to Org in search for new
maintainers …](https://git.sr.ht/~bzg/org-contrib)

<https://github.com/emacs-mirror/emacs/blob/master/lisp/org/ob-python.el>

## Program design

<https://www.reddit.com/r/haskell/comments/xjptp8/comment/ipc8uf6/?context=3>

At module level I try to expose the data types and functions that model
the problem I am targeting. Ideally it will be clear how to use the
functions "on a pipeline".

Most of my programs are a bunch of tiny, conceptually separated
pipelines. These will be combined by the real main module.

At top level there is some sort of eventloop anyway. I use functions
defined on there that use this pipelines at different levels. I make
sure that every stored value that is not immediately demanded is
evaluated to normal form. At this level I also coordinate the threads
and STM channels if needed.

## cabal explore

### grandfathering

### towards a hackage score

## codensity types

<https://www.reddit.com/r/haskell/comments/1kjya6x/backend_developers_use_continuation_passing_style/>

## shortest path matrix application

<https://gist.github.com/sjoerdvisscher/6bbaa3556b7572b6c5b4d0968226edd2>

## huihua development

### huihua fixes

- [ ] refactor pretty instance of ArrayU
- [ ] bugs stack is printing out in the wrong order

### huihua to be developed

- [ ] boxing
- [ ] fold, scan, group, partition
- [ ] planet operators
- [ ] stack operators
- [ ] each
- [ ] rows
- [ ] inventory
- [ ] repeat
- [ ] do
- [ ] Un, Under
- [ ] Content, Fill
- [ ] MiscG
- [ ] Random
- [ ] () function
- [ ] \<\> switch
- [ ] @ \$
- [ ] bindings

[test isms](~/repos/huihua/readme.org::*test isms)
[NYI](~/repos/huihua/readme.org::*NYI) [negative
bug](~/repos/huihua/readme.org::*negative bug)

### symbols

<https://en.wiktionary.org/wiki/Appendix:Unicode/Geometric_Shapes>

<https://en.wikipedia.org/wiki/Mathematical_operators_and_symbols_in_Unicode>

<https://en.wikipedia.org/wiki/List_of_mathematical_symbols_by_subject>

### eulers

euler1

/+▽/↥⊞(=0◿)3<sub>5</sub>.⇡10

euler 2

/+▽=0◿2.\[;⍥(⎋\>4000000.+,,)∞ 2 1\]

euler 3

/↥×+2⇡⌊√∶⇌\[;;⍥(~~1→→(!(×0)\_(↥≠0/~~\[⍥(+1→→(⎋·)→→(=0◿),,)⌊√,2\]∶=2.)∶),∶→→(=0◿),,)⌊√,
2\].

⊢\[;⍥(∶⊙÷.⊢⎋=0⧻.▽⊃(=0◿)∘↘2⇡⌈√..)∞\] 600851475143

Sieve ← ⊏⌂.⍥(⊂∶→(▽≠0◿).⊢..)⌊√∶+2⇡.

Primes ← (▽=1/+⊞(=0◿)..+2⇡)

/×ⁿ⌊ₙ,∶Primes.20

⧻▽⊃(≠0◿17)∘▽⊃(≠0◿13)∘▽⊃(≠0◿11)∘▽⊃(≠0◿7)∘▽⊃(≠0◿5)∘▽⊃(≠0◿3)∘+3×2⇡100000

euler 4

    Digits ← |1 /∘[] [;⍥(⎋=0.⌊÷10∶◿10.)∞]
    A ← ♭⊞×.↘100⇡1000
    IsPal ← ≅⇌.Digits
    /↥▽∵IsPal .A

    A ← ♭⊞×.↘100⇡1000
    IsPal ← ≅⇌.|1 /∘[] [;⍥(⎋=0.⌊÷10∶◿10.)∞]
    /↥▽∵IsPal .♭⊞×.↘10⇡100

!(\|1 ↬\>0.dm) 999

Digits ← \[;⍥(⎋=0.⌊÷10∶◿10.)∞\] Digits 56799944

Digits ← \[;⍥(⎋=0.⌊÷10∶◿10.)∞\]

A ← ♭⊞×.↘90⇡100 IsPal ← (≅⇌.\[;⍥(⎋=0.⌊÷10∶◿10.)∞\])

∺IsPal\[989\]

1.  euler 3

    – haskell primeFactors 1 = \[\] primeFactors n

|                                                           |
|---|
    | factor == \[\] = \[n\]                                    |
    | otherwise = factor || primeFactors (div n \$ head factor) |
    where factor = take 1 \$ filter ( -\> (mod n x) == 0) \[2 .. n-1\]

    print \$ primeFactors 600851475143

## Matrix factorizations

<https://x.com/TivadarDanka/status/1936363978151924017>
