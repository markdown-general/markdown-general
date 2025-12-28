---
title: "markdown-unlit"
date: 2025-12-23
status: active
---

# markdown-unlit

## Overview

Exploring markdown-unlit as a tool for literate programming with Haskell code execution in markdown files.

This could replicate the org-babel functionality lost when moving from org-mode to markdown for content management.

---

## Tool Reference

https://github.com/sol/markdown-unlit

Allows running Haskell code blocks within markdown files.

---

## Goals

- Recover code execution capability in markdown (lost from org-babel)
- Extend the literate programming pattern established with publish.md
- Enable Haskell code examples that can be executed and tested
- Keep content in markdown while gaining executable code blocks

---

## Relationship to Publish.md Pattern

We've established literate programming via executable markdown (publish.md):
- Bash scripts embedded in markdown
- Self-documenting code
- Lists + docs + execution in one file

markdown-unlit could extend this to Haskell:
- Haskell code blocks in markdown
- Executable and testable
- Documentation + working code

---

## Investigation Tasks

- [ ] Install and test markdown-unlit
- [ ] Understand integration with existing workflow
- [ ] Compare to org-babel capabilities
- [ ] Determine use cases (blog posts vs project docs vs experiments)

---

## Related

- ~/markdown/base/workflows/todo-architecture.md - Context on org-babel → markdown migration
- ~/markdown/base/publish.md - Established literate programming pattern (bash)

---

**Started:** 2025-12-23
