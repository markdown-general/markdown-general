---
title: "emacs"
date: 2025-12-23
---

# emacs

## blogs

[Coding blog \| Protesilaos Stavrou](https://protesilaos.com/codelog/) [but she's a girl…](https://www.rousette.md.uk/) [Karthinks](https://karthinks.com/) [category - emacs :: Sacha Chua](https://sachachua.com/blog/category/emacs/) <https://irreal.md/blog/> [System Crafters - YouTube](https://www.youtube.com/c/SystemCrafters/videos) [Zaiste Programming - YouTube](https://www.youtube.com/channel/UCzgkOWKcwy0uhYilE6bd1Lg) [DistroTube - YouTube](https://www.youtube.com/channel/UCVls1GmFKf6WlTraIb_IaJg)

## key-fu

## keyboard setup

- command and control are swapped.
- CapsLock is Meta

<img src="other/keys.png" width="300" />

Command ==\> C- Control ==\> s- Option ==\> M- CapsLock ==\> M-

[Emacs: How to Bind Super Hyper Keys](http://xahlee.info/emacs/emacs/emacs_hyper_super_keys.html)

``` elisp
(setq ns-function-modifier 'hyper)  ; make Fn key do Hyper
```

## useful but forgotten

| key         | command                                         |
|---|---|
|-------------|-------------------------------------------------|
| `SPC b C`   | clone-other-window                              |
| `C-;`       | embark-act                                      |
| `SPC w s`   | split vertically                                |
| `SPC w w`   | switch windows                                  |
| \-          | dired up a directory                            |
| `C-/`       | undo                                            |
| `M-SPC`     | cycle-spacing                                   |
| `SPC h b b` | embark-bindings                                 |
| `M-s-RET`   | insert header                                   |
| `SPC m l t` | show org link                                   |
| `z n`       | narrow to subtree                               |
| `g d`       | lookup def                                      |
| `g D`       | usages                                          |
|             |                                                 |
| `` C-c ` `` | pops a babel snippet into the codes major mode. |
| `z n` `z N` | narrow (widen) subtree                          |
|             |                                                 |
## avoid arrowing

- word-level movements
  - `w` `e` `b`
  - `e` `E` `M-e` forward: word, WORD, sentence
  - (J)oin
- avy
  - `gsSPC` one character
  - `gss` or `M-j` char timer
  - `gs/` two character
  - `gsj` line choice
- scrolling
  - `C-u` `C-d` Scroll up and down (half a page)
  - `C-f` `C-b` Scroll page down and back.
  - `C-e` `C-y` Scroll one line. dont move cursor

## marking

- quickly select a region (`vv` `vi(` `V`)
- mark `ma` `` `a ``
- various mark jumps `C-SPC` `g;` goto-last-change `gi` evil-insert-resume

## ergonomics

- quickly navigate a project: `SPC s i` `SPC c j` `consult-outline`
- registers `C-x r`
- yanking
  - `SPC s y` consult-yank-from-killring
  - `M-w`
- org-mode `C-RET` `S-TAB` `s-RET` insert-item-below
- find file other window: `SPC f f` select `C-;` `o`

## insert newline

`[ o` and `] o`

## evil

## vi

[Vim Crash Course · GitHub](https://gist.github.com/dmsul/8bb08c686b70d5a68da0e2cb81cd857f) [Graphical vi-vim Cheat Sheet and Tutorial](http://www.viemu.com/a_vi_vim_graphical_cheat_sheet_tutorial.html) <https://stackoverflow.com/questions/1218390/what-is-your-most-productive-shortcut-with-vim>

`ma` - mark current position as a `'a` - go to beginning of line of position a `` `a `` - go to exactly pos a `"add` - cut current line into a register `"ap` - paste register a `.` - repeat last text changing command. `*` - search for next instance of a word `#` is previous `gi` - go to last edited location.

## evil

[Toby 'qubit' Cubitt - Evil cursor model](https://www.dr-qubit.md/Evil_cursor_model.html)

modes: `jk` `v` `V` `i` `R`

actions: `d` `y` `p`

mark: `m` `` ` `` `'` goto mark

register `"`

What is the `a` in `vaw`?

a selection: `w` word `s` sentence `p` paragraph `(` () block

Hierarchy of object shortcuts: `hljk` `web0$` `(){}`

|               |                                              |
|---|---|
|---------------|----------------------------------------------|
| vaw           | mark word                                    |
| dw            | delete word                                  |
| d\$           | delete to end of line                        |
| dd            | delete whole line                            |
| 2dd           | delete 2 lines                               |
| yw            | yank word                                    |
| /             | search, enter to exit, (n)ext                |
| \*            | search for word at point                     |
| :%s/old/new/g | change every occurrence in the whole buffer. |
| R             | replace                                      |
| ma            | mark with a                                  |
| \`a           | go to mark a                                 |
| \`\`          | go to where you just were                    |
| di"           | delete inside quotes                         |
| ci"           | change inside quotes                         |
| HML           | top, middle, bottom of screen                |
| "ayy          | copy line to register a                      |
## new emacs ways

## embark

[Fifteen ways to use Embark \| Karthinks](https://karthinks.com/software/fifteen-ways-to-use-embark/) [System Crafters Live! - The Many Uses of Embark - YouTube](https://www.youtube.com/watch?v=qk2Is_sC8Lk) [Emacs: Embark and my extras - YouTube](https://www.youtube.com/watch?v=uoP9ZYdNCHg) [GitHub - oantolin/embark: Emacs Mini-Buffer Actions Rooted in Keymaps](https://github.com/oantolin/embark)

## consult

[GitHub - minad/consult: consult.el - Consulting completing-read](https://github.com/minad/consult)

Best of the consult commands:

- consult-find `SPC s f`
- consult-imenu `SPC s i`
- consult-lsp-symbols `SPC c j`
- consult-outline
- consult-isearch

## lsp

<https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/>

## Karthinks

[Avy can do anything \| Karthinks](https://karthinks.com/software/avy-can-do-anything/)

The idea: stop scrolling to a place on the screen to do something and then scroll back.

- `gsSPC` `M-j` single char, multiple windows
- `C-x C-SPC` pop-global-mark
- `SPC a` embark-act

`Filter` -\> `Select` -\> `Act`

Current tools

- Filter: winnow a large set of candidates to a small number.
- Select: specify the candidate as the one.
- Act: run a task with the candidate.

orderless -\> vertico -\> embark

Short action dont's:

- switch windows to jump to visible text.

- move point to lookup help.

- jumping to a location to kill/copy/move/delete a single word/line.

- `M-j`

- `C-x C-SPC` pop-global-mark returns after a short motion jump

Short action do's:

## gptel

<https://github.com/karthink/gptel?tab=readme-ov-file#doom-emacs>

***REMOVED***

## potential new emacs habits

## occur

The big idea is that an occur buffer is editable. [Emacs’ powerful OCCUR function in practice \| Protesilaos Stavrou](https://protesilaos.com/codelog/2019-08-04-emacs-occur/)

occur andthen e to be able to edit

## bookmarks & jumping

[Primer on Emacs bookmarks \| Protesilaos Stavrou](https://protesilaos.com/codelog/2021-09-08-emacs-bookmarks-intro/)

## emacs stuff I forget

- org-ctrl-c-minus

- hidden files

  `SPC m h`

- tramp Manually type this to get sudo in ido-find-file: *sudo:root@localhost:*

- unzip z !

- org link edit `C-c C-l`

- save-excursion

- org-redisplay-inline-images

- [underscored words](https://www.reddit.com/r/emacs/comments/11u0bb1/why_are_my_underscored_words_dropping_a_level/)

## doom-fu

## map!

See practical map! usage here:

[doom-emacs/+evil-bindings.el at develop · hlissner/doom-emacs · GitHub](https://github.com/hlissner/doom-emacs/blob/develop/modules/config/default/+evil-bindings.el)

## adding a feature

[doom-emacs/getting<sub>started</sub>.md at develop · hlissner/doom-emacs · GitHub](https://github.com/hlissner/doom-emacs/blob/develop/docs/getting_started.md#usingloading-local-packages)

Add bespoke variation in packages.el

``` elisp
(if (featurep! +lite)
  (package! haskell-lite
    :recipe (:host github :repo "tonyday567/haskell-lite")
    :pin "af9c4b968698e285c453516b2104a347ad954849")
  (package! haskell-mode)
  )

```

## completely removing a library

eg after switching from local to elpa or viceversa.

``` bash
rm -rf ~/.emacs.d/.local/straight/repos/haskell-mode
rm -rf ~/.emacs.d/.local/straight/build-28.0.50/haskell-mode
```

possible doom commands after changing recipes:

- doom sync -u (119/188) Skipping haskell-mode because it is local

## hugo-fu

[hugo](https://gohugo.io/)

Static site generation. See <span class="spurious-link" target="hugo details">*hugo details*</span> for specifics.

## [ox-hugo](https://github.com/kaushalmodi/ox-hugo/)

Whilst hugo does support org-mode, ox-hugo seems like a very active and well-thought through set of functions to convert org files to `*.md`.

    #+TITLE: Colophon
    #+draft: true
    #+date: 2020-09-29
    #+hugo_base_dir: ~/site/
    #+hugo_section: notes

this is enough for ox-hugo to continuously convert everything to markdown with correct links.

- real-time rendering and visualisation with hugo

<!-- -->

    hugo server -D --navigateToChanged --disableFastRender

[GitHub - fniessen/org-html-themes: How to export Org mode files into awesome …](https://github.com/fniessen/org-html-themes)

## references

- [Hugo Coder](https://themes.gohugo.io/hugo-coder/) theme

- simple tags: [Add Tags to Your Hugo Blog Using Taxonomies \| Jake Wiesler](https://www.jakewiesler.com/blog/hugo-taxonomies/)

- ox-hugo references

  [GitHub - kaushalmodi/ox-hugo: A carefully crafted Org exporter back-end for Hugo](https://github.com/kaushalmodi/ox-hugo/)

  [ox-hugo - Org to Hugo exporter](https://ox-hugo.scripter.co/doc)

  [Ken Grimes :: Using ox-hugo To Build Websites with Emacs](https://www.kengrimes.com/ox-hugo-tutorial/)

- rss references

  [Customizing RSS Feed for Hugo Blog \| Pritesh Usadadiya](https://www.priteshusadadiya.com/post/custom-rss-feed/)

  [RSS Templates \| Hugo](https://gohugo.io/templates/rss/#the-embedded-rss-xml)

  [GitHub - xianmin/hugo-theme-jane: A readable & concise theme for Hugo](https://github.com/xianmin/hugo-theme-jane)

  [Enabling RSS Feeds in Hugo \| finde labs](https://www.findelabs.com/post/enable-rss-feeds-in-hugo/)

  [Wrangling Hugo's RSS templates - Jack Baty's weblog](https://www.baty.net/2019/wrangling-hugos-rss-templates/)

  <https://www.baty.net/>

## Hugo notes

`SPC m l p` to export a subheading.

To develop:

``` zsh
cd ~/site
hugo -D server
```

To turn a subheading into a post.

- add these properties to the subheading:

  > 

- `ToDo` makes a post a draft.

- `#+hugo_auto_set_lastmod: t` at the top of the file

- `SPC m l p`

## Blog Tests

### org test <span class="tag" tag-name="dev"><span class="smallcaps">dev</span></span> <span class="tag" tag-name="ignore"><span class="smallcaps">ignore</span></span>

This is a test page, illustrating formatting of various org-mode stylistic elements.

1.  list

    - [x] tests
      - [x] code
        - [x] unfenced
        - [x] fenced
        - [x] inline
      - [x] checklists
      - [x] plain lists
      - [x] links
      - [x] image
      - [x] quotes

2.  quote

    > and by each crime and every kindness, we birth our future.” ~ David Mitchell, Cloud Atlas

3.  image

    [![](~/repos/chart-svg/other/venn.svg)](file:~/repos/chart-svg/other/venn.svg)

4.  unfenced code

        #+TITLE: Test page
        #+draft: true
        #+date: 2020-03-15
        #+hugo_base_dir: ~/site/
        #+hugo_section: notes
        #+hugo_tags: setup

5.  code - fenced

    ``` haskell
    main :: IO ()
    main = do
      let n = 10
      let answer = product [1..n::Integer]
      void $ runOutput ("example.lhs", LHS) ("readme.md", GitHubMarkdown) $ do
        output "example1" (Fence "Simple example of an output")
    ```

6.  links

    external link

    [GitHub - kaushalmodi/ox-hugo: A carefully crafted Org exporter back-end for Hugo](https://github.com/kaushalmodi/ox-hugo/)

    sectional link

    <span class="spurious-link" target="*Section 2">*Section 2 link*</span>

    ``` markdown
    [color-adjust]({{< relref "color-adjust" >}})
    ```

    \[color-adjust\]({{\< relref "color-adjust" \>}})

7.  inline code

    - This is what ~ `sqiggles` looks like.
    - This is what = `equals` looks like.
    - This is what \* **bold** looks like.
    - This is what / *slash* looks like.

8.  ToDos

    '**\*\*** ToDo This is what ToDo looks like. '**\*\*** Next This is what Next looks like. '**\*\*** Blocked This is what Blocked looks like.

9.  Section 2

    This is section 2 that is linked to.

10. latex

    If $`a^2=b`$ and $` b=2 `$, then the solution must be either
    ``` math
     a=+\sqrt{2} 
    ```
    or
    ``` math
     a=-\sqrt{2} 
    ```
    .

    ``` math
    (E = \sqrt{m^2 + p^2}\\)
    ```

    $`\mathbb{R}`$

    $`\le`$

    pairs (x,y)∈ℝ2 where a≤b when ax≤bx and ay≤by

    $` (x,y) \in \mathbb R^2`$ where $` a \leq b`$ when $` a_x \leq b_x`$ and $` a_y \leq b_y`$

## site styles

[It Doesn’t Have to Be Arbitrary \| Freckle Education](https://tech.freckle.com/2022/04/07/it-doesnt-have-to-be-arbitrary/) [Try Fold First! — Monday Morning Haskell](https://mmhaskell.com/blog/2022/4/7/first-try-fold)

## treesitter-fu

cabal install problem

[tree-sitter/tree-sitter-haskell#34 Unable to setup on mac](https://github.com/tree-sitter/tree-sitter-haskell/issues/34)

Needed:

c compiler: gcc-13 c++ compiler: c++-13

FIXME: bad solution ==\>

``` bash
ln -s /usr/local/Cellar/gcc/13.2.0/bin/gcc-13 /usr/local/bin/gcc
```

> Warning (treesit): Error encountered when installing language grammar: (treesit-error Command: c++ -fPIC -c -I. scanner.cc Error output: scanner.cc:27:53: error: expected expression for<sub>each</sub>(indent<sub>lvls</sub>.cbegin(), indent<sub>lvls</sub>.cend(), \[buffer, &i\](uint8<sub>t</sub> const n) { ^ scanner.cc:75:9: warning: 'auto' type specifier is a C++11 extension \[-Wc++11-extensions\] auto cur<sub>indent</sub>\_lvl = indent<sub>lvls</sub>.back(); ^ 1 warning and 1 error generated. )

Possible debug path is to install via tree-sitter client executable

<https://github.com/tree-sitter/tree-sitter/blob/master/cli/README.md#tree-sitter-cli>

## lsp-fu

<https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/>

<https://github.com/emacs-lsp/lsp-haskell>

[Features — haskell-language-server 2.5.0.0 documentation](https://haskell-language-server.readthedocs.io/en/latest/features.html)
