# card ⟜ executable pattern, archived and testable

## What is a card

A card is a markdown file encoding a repeatable pattern:
- Something we actually did (or want to do predictably)
- Written down so it can be spun again (executed, tested, reshaped)
- Archived with Git history
- Always open to questioning and reshaping

## Shape

```
title ⟜ one-liner describing the pattern

**instruction**
[the work, terse]

**comment** ⟜ [optional: constraints, learnings, decisions]
```

Add **input** or **output** sections only if they clarify what's needed.

## How to write a card

1. Identify the pattern — What's worth doing again?
2. Name it — One line, active verb
3. Write instruction — Terse steps
4. Add comment if needed — Otherwise skip
5. Test it — Run once, verify output
6. Archive it — Commit to git

Heuristics are meant to evolve. These guidelines shouldn't replace sensibility. Reshape cards as clarity demands.

## How cards are used

**Spin** — Agent executes, writes to log/, reports done.
**Inline** — You execute, read output, continue.
**Circuit** — Background to log/, listener notifies.

## When a card fails

Ask instead of retrying. If the pattern doesn't work, rewrite the card or recognize it's novel work.
