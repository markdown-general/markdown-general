# card ⟜ executable pattern, archived and testable

## What is a card

A card is a markdown file encoding a repeatable pattern we've learned. It's:
- Something we actually did (or want to do predictably)
- Written down so it can be spun again (executed, tested, reshaped)
- Archived in Git with history

A card is not: tutorial, generic documentation, or fixed forever.

## Shape

```
title ⟜ one-liner describing the pattern

**instruction**
[the work, terse]

**comment** ⟜ [optional: constraints, learnings, decisions]
```

Optional sections only if they add signal:
- **input** ⟜ preconditions
- **output** ⟜ what gets written where

## Writing a card

1. Identify the pattern — What's worth doing again?
2. Name it — One line, active verb.
3. Write instruction — Terse steps. If unclear, split.
4. Add comment if needed — Otherwise skip.
5. Archive it — Commit to Git.

## How cards are used

**Spin** — Agent executes independently, writes to log/. **Inline** — Execute via bash, read immediately. **Circuit** — Background work, listener notifies.

## Evolving cards

Heuristics are meant to evolve and shouldn't replace sensibility. Reshape, delete, or improve cards as the library learns. If a card doesn't work, rewrite it or recognize the problem is novel.

Cards are the persistence layer. The learning lives in the library, not in memory.
