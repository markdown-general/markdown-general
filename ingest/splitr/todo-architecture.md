# Todo Architecture

**Why todos stay in org-mode while content lives in markdown**

**Date:** 2025-12-23
**Status:** Active decision

---

## The Separation

**~/mash/org/** - Operational todos (org-mode, ephemeral)
**~/mash/** - Content (markdown, permanent, mashed)

This isn't organizational preference - it's about different lifecycles and different processes.

---

## Why Separate

### The Problem with Mixed Systems

**Before:** Org-mode for everything
- Content authoring in org
- Todo management in org
- TODOs could be tagged directly in content

**Problem discovered:**
- TODOs became content unintentionally
- Operational concerns mixed with knowledge
- Hard to distinguish ephemeral vs permanent
- Content files accumulated stale TODO markers

### The Solution

**Moved content to markdown:**
- Better portability (org is Emacs-specific)
- Simpler for version control
- GitHub/web native
- Right format for mashing

**Kept todos in org-mode:**
- org-capture extremely fast (SPC X)
- org-refile for processing
- org-agenda for views
- Mature Emacs integration

---

## Current Structure

```
~/mash/org/              # Operational (ephemeral)
├── todo.org                 # Active tasks
├── refile.org               # Inbox (capture target)
└── bugz.org                 # Bugs with source links

~/mash/                  # Content (permanent)
├── base/                    # Stable knowledge
├── self/                    # Experimental thinking
├── projects/                # Active work
└── artifacts/               # Supporting files
```

---

## Workflow

### Capture (SPC X)

Templates in ~/.config/doom/config.el:
- `SPC X r` - Quick capture to refile.org
- `SPC X z` - Bug with link to source

Items captured instantly without breaking flow.

### Processing

```
Capture → Process → Execute → Archive/Document

SPC X r          Refile to        Complete       Decision:
  ↓              appropriate      task           ↓
refile.org  →    todo.org    →    [DONE]    →   Important? → ~/mash/
(inbox)          (active)                        Trivial? → delete
```

### Cleanup

Regular processing of ~/mash/org/:
- Review refile.org frequently
- Complete tasks, mark DONE
- **Move to ~/mash/** if contains:
  - Learning worth preserving
  - Decision documentation
  - Project progress
  - Code snippets to remember
- **Delete** if:
  - Simple completion (nothing to document)
  - Obsolete/irrelevant
  - Ephemeral operational item

**Result:** ~/mash/org/ stays minimal, ~/mash/ accumulates knowledge

---

## The Mashing Machine Principle

**~/mash/ is where content undergoes continuous transformation through AI collaboration.**

Not organizational tidiness - it's a **process arena** where:
- New content challenges old content
- Better ideas compete with and replace weaker ones
- Material gets stress-tested
- AI works with content as discrete, evolvable units
- Documentation improves through iteration

**Markdown as the unit because:**
- Right granularity for focused work
- Plain text shows what changed
- Easy to compare, reorganize, split, merge
- Version control tracks evolution
- AI can challenge and improve systematically

This is "we mash" in action.

---

## What Gets Mashed vs What Doesn't

### Content in the mashing machine:

- Architecture documentation (this file)
- Publication workflows
- Blog posts (draft → refine → publish)
- Project notes (capture → organize → distill)
- Decision records (document → challenge → update)

These benefit from iterative refinement with AI. They evolve through transformation.

### Content NOT in the mashing machine:

- Active todos in ~/mash/org/ (operational, ephemeral)
- Build artifacts in ~/site/public/ (generated output)
- Git history (immutable record)

These don't benefit from refinement - they complete or delete.

---

## Why This Separation Works

**Todos (~/mash/org/):**
- Operational, not knowledge
- Complete or delete
- Don't need iterative refinement
- Ephemeral by nature

**Documentation about todos (this file):**
- Goes into the mashing machine
- Can be challenged by new ideas
- Benefits from continuous refinement
- Will evolve as system evolves
- Living documentation, not operational data

**The real principle:**

Not "everything in markdown for tidiness"

But "content that benefits from iterative refinement goes into ~/mash/ where it can be mashed"

---

## Lost and Recovered: Org-Babel

### What We Lost

Org-mode's code execution:
```org
#+begin_src bash
echo "hello"
#+end_src

#+RESULTS:
: hello
```

Literate programming in org files - execute code blocks, results inserted automatically.

### What We Recovered

**Literate programming via executable markdown:**

publish.md pattern - embed bash script in markdown:
```markdown
#!/bin/bash
sed -n '/^# SCRIPT_START/,/^# SCRIPT_END/p' "$0" | bash
exit $?

# SCRIPT_START
# ... actual script ...
# SCRIPT_END

# Documentation continues...
```

**Benefits over org-babel:**
- Portable (runs without Emacs)
- Version controlled
- Self-documenting
- Lists embedded as markdown
- Executable from shell
- Viewable as formatted markdown on web

This is "we mash" applied to scripts - code, docs, data unified.

---

## Trade-offs

### Why Not Convert Todos to Markdown?

**Would lose:**
- org-capture (instant workflow)
- org-refile (flexible organization)
- org-agenda (custom views, scheduling)
- TODO state workflows
- Source links in bugz

**Markdown task lists are simpler:**
```markdown
- [ ] Todo
- [x] Done
```

But loss of functionality not worth format consistency.

### Why Not Move ~/mash/org/ Inside ~/mash/?

**Considered alternatives:**
1. Convert todos to markdown → Lose org power
2. Move to ~/mash/org/ → Break conventions, still format mismatch
3. Dual system (current) → Keep both working

**Chosen: Intentional separation**

**Rationale:**
- Tool-appropriate formats (org for todos, markdown for content)
- Clear lifecycle distinction (ephemeral vs permanent)
- Prevents content drift (force decision point)
- Workflow efficiency (org-capture extremely fast)
- Emacs ecosystem alignment

---

## System Boundaries

### What Goes in ~/mash/org/

- Current todos
- Bug reports
- Quick captures
- Scheduled items
- Active project tasks

### What Goes in ~/mash/

- All permanent content
- Documentation
- Blog posts
- Project notes
- Decision records
- Code examples
- Reference material
- Important completed todos (as documented learnings)

---

## The Workflow in Practice

```
Content enters ~/mash/ → Gets mashed → Competes with alternatives
                                  ↓
                          Better version emerges
                                  ↓
                        Replaces weaker version
                                  ↓
                            Process repeats
```

**This file participates in that process** - it evolves as the system evolves, as new insights emerge, as rival methods get tried.

---

## Future Considerations

### If Org-mode Becomes a Problem

**Options:**
1. Markdown task lists (simple but less powerful)
2. Plaintext todo.txt
3. External task manager
4. Custom markdown capture system

**For now:** Org-mode works extremely well. No need to change.

### Extending Executable Markdown

The publish.md pattern works for:
- Backup scripts
- Deployment scripts
- Data processing
- Any operational task needing documentation

Literate programming in markdown, recovering org-babel capability.

---

## Summary

**Decision:** Keep ~/mash/org/ and ~/mash/ separate

**Rationale:** Different lifecycles (ephemeral vs permanent), different processes (operational vs mashing)

**Benefit:** Prevents operational tasks from unintentionally becoming content

**Trade-off:** Format inconsistency (org vs markdown) for tool-appropriate workflows

**Functionality recovered:** Literate programming via executable markdown

**This is a pragmatic exception to "everything in ~/mash/" - justified by workflow efficiency and clear separation.**

---

**mash/ is happy when content flows through transformation. Todos flow through completion. Different processes, different tools.**
