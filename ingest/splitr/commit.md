---
title: "Git Commit Convention"
date: 2025-12-24
description: "Commit message style for mash repository"
---

## Commit Message Format

**One-liner only.** No multiline commits.

## Style Rules

- Single line, concise description of changes
- No author information (no Co-Authored-By, no Generated-with)
- No emojis
- Start with imperative verb when possible
- Keep under 72 characters when practical

## Examples

**Good:**
```
Flatten site-content into base, mash posts to artifacts with updated links
Add preview.md convention for Hugo rendering
Move org-markdown-migration to projects
```

**Bad:**
```
🤖 Flatten site-content into base

- Detailed list
- More details

Co-Authored-By: Claude Sonnet 4.5 <noreply@anthropic.com>
```

## Why

Simple, scannable git history. The commit log should be a clean list of what changed, not decorated prose.

---

**This convention applies to all commits in ~/mash/ repository.**
