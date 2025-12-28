# Repository Architecture

**Where git repositories live and why**

**Date:** 2025-12-23

---

## Repositories Outside ~/repos/

### ~/.oh-my-zsh/

**Type:** Third-party framework
**Keep:** Yes - managed by oh-my-zsh installer

### ~/mash/

**Type:** Active workspace
**Remote:** https://github.com/tonyday567/markdown
**Keep:** Yes - this is where mashing happens

```
~/mash/
├── base/           # Stable knowledge
├── self/           # Experimental thinking
├── projects/       # Active work
└── artifacts/      # Supporting files
```

Different purpose than development repos in ~/repos/. This is the mashing machine, not a code library.

### ~/org/

**Type:** Operational workspace
**Remote:** https://github.com/tonyday567/org
**Keep:** Yes - todos stay in org-mode

See todo-architecture.md for rationale.

### ~/site/

**Type:** Hugo static site
**Remote:** https://github.com/tonyday567/site
**Keep:** Yes - Hugo expects specific structure

### ~/site/public/

**Type:** GitHub Pages deployment
**Remote:** https://github.com/tonyday567/tonyday567.github.io
**Keep:** Yes - generated output for deployment

### ~/.config/doom/

**Type:** Doom Emacs config
**Remote:** https://github.com/tonyday567/doom
**Keep:** Yes - Doom expects config here

Executable configuration, not content. Documentation lives in mash/base/emacs-config.md where it can be mashed.

### ~/.config/emacs/

**Type:** Doom framework itself
**Keep:** Yes - third-party, managed by Doom

---

## Repositories Inside ~/repos/

32 development repositories:
- 27 Haskell projects
- 4 Haskell development packages
- 1 dotfiles (GNU Stow managed)
- 1 checklist

---

## Organization Principles

### What belongs in ~/repos/

Development repositories:
- Libraries and packages being developed
- Code that will be published/shared
- Active software projects
- Forks of upstream projects

### What belongs at ~/ level

Active workspaces:
- ~/mash/ - Content workspace (mashing machine)
- ~/org/ - Operational workspace (todos)
- ~/site/ - Website project

Different purpose than libraries. Need specific locations for tooling. Frequently accessed.

### What belongs in ~/.config/

Application configurations following XDG standard:
- ~/.config/doom/ - Emacs config
- ~/.config/emacs/ - Emacs framework

Not development repos, just configuration.

---

## Why mash/ Isn't in ~/repos/

~/repos/ is for code libraries and development projects.

~/mash/ is:
- An active workspace, not a library
- Where content undergoes transformation
- The mashing machine for markdown
- Different purpose entirely

Moving it would be organizational tidiness without benefit. It works where it is.

---

## Summary

Current structure is well-organized:
- Development projects in ~/repos/
- Active workspaces at ~/ level  
- Configs in ~/.config/

No changes recommended.

---

## Related Documentation

- todo-architecture.md - Why ~/org/ stays separate
- content-guidelines.md - How mash/ works
- publication-workflow.md - How ~/site/ integrates

---

**Everything is where it needs to be for the work to happen.**
