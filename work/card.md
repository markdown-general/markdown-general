# card ⟜ executable pattern, archived and testable

A card is a markdown file encoding a repeatable pattern. It's something we actually did, written down so it can be executed again, and archived in Git.

A card is not: tutorial, generic documentation, or fixed forever.

## Shape

```
title ⟜ one-liner

**instruction**
[the work, terse]

**comment** ⟜ [optional: learnings or constraints]
```

Include **input** and **output** sections only if they add signal.

## Writing a card

1. Identify the pattern — What's worth doing again?
2. Name it — One line, active verb.
3. Write instruction — Terse steps. If unclear, split.
4. Add comment if needed — Otherwise skip.
5. Archive it — Commit to Git.

## Spin and Inline

**Spin** ⟜ Agent executes independently, writes to log/.

**Inline** ⟜ Execute via bash, read immediately.

## Evolution

Heuristics are meant to evolve and shouldn't replace sensibility. Reshape, delete, or improve cards as the library learns. Cards are the persistence layer. The learning lives in the library, not in memory.
