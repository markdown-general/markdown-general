# card ⟜ executable pattern, archived and testable

## What is a card

A card is a markdown file encoding a repeatable pattern we've learned. It's:
- Something we actually did (or want to do predictably)
- Written down so it can be spun again (executed, tested, reshaped)
- Archived in ~/markdown-general/cards-purpose-based/ with Git history
- Always open to questioning and reshaping for future use

A card is not: tutorial, generic documentation, aspirational, or fixed forever.

## Shape

```
title ⟜ one-liner describing the pattern

**instruction**
[the work, terse]

**comment** ⟜ [optional: constraints, learnings, decisions]
```

Optional sections only if they add signal:
- **input** ⟜ preconditions (what's required to start)
- **output** ⟜ what gets written where (only if non-obvious)

## How to write a card

1. Identify the pattern — What did you just do that's worth doing again?
2. Name it — One line, active verb (debug-filewatcher-chain, library-metadata).
3. Write instruction — Terse steps, no philosophy. If longer than 10 lines, split.
4. Add comment if needed — Constraints? Decisions? Warnings? Otherwise skip.
5. Test it — Run once. Verify output matches what you promised.
6. Archive it — Commit to ~/markdown-general/cards-purpose-based/.

Note: Heuristics are meant to evolve. These guidelines shouldn't replace sensibility. If a card needs a different shape for clarity, reshape it.

## How cards are used

**Spin** — Agent executes independently, writes to log/, reports done.
**Inline** — You execute via bash, read output immediately, continue.
**Circuit** — Background to log/, listener notifies, you read when ready.

## When a card fails

Ask instead of retrying. If the pattern doesn't work, either rewrite the card or recognize the problem is novel and needs new work.

## The library

Cards are the persistence layer. The conversation is ephemeral. The learning lives in the library, not in a single operator's memory.

Reshaping, deleting, and evolving cards is how the library stays honest.
