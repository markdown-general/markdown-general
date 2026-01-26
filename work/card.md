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

## Why this shape

**Title as pattern name** — haskell.md, debug-filewatcher-chain, library-metadata. Instantly nameable and spinnable.

**Instruction as imperative** — Not narrative. The work, terse. Operator or agent reads once, executes.

**Comment as learning** — Constraints discovered the hard way. Decisions that mattered. Warnings, not explanation of obvious things.

**No boilerplate** — Input/output only when they clarify, not as template ritual.

## How cards teach

Cards encode:
1. **A pattern you've solved** (haskell.md: dependency refactoring via incremental substitution)
2. **A diagnosis method** (debug-filewatcher-chain: trace execution, add logging, identify bottleneck)
3. **A decision gate** (comonad-instance-false-start: recognize when instance is false)
4. **An assessment** (library-haddock, library-hlint: measure health)
5. **A discovery method** (scout-hyperbole-buttons: structured exploration)

Teaching happens **in the doing**, not the reading. You learn by executing, seeing output, understanding why.

## How cards are used

**Spin** — Agent executes independently, writes to log/, reports done.
**Inline** — You execute via bash, read output immediately, continue.
**Circuit** — Background to log/, listener notifies, you read when ready.

All three modes use the same card. The mode describes patience, not the pattern.

## What makes a card good

**Coherence** — Title matches instruction. No drift.

**Composability** — One card's output can feed another's input (or doesn't poison it).

**Recognition** — When you see a similar problem, you think "this is that card."

**Testability** — Verify it works without subjective judgment.

**Terseness** — No wasted lines. Every line earns its place.

**Honesty** — Cards record what actually happened, not what should happen. If the pattern involves trying 4 times, that's the pattern.

## How to write a card

1. **Identify the pattern** — What did you just do that's worth doing again?
2. **Name it** — One line, active verb (debug-filewatcher-chain, library-metadata). Spinnable name.
3. **Write instruction** — Terse steps, no philosophy. 7-10 lines max; if longer, split.
4. **Add comment if needed** — Constraints? Decisions? Warnings? Otherwise skip.
5. **Test it** — Run once. Verify output matches what you promised.
6. **Archive it** — Commit to ~/markdown-general/cards-purpose-based/. Now part of the library.

## Cards are archived and statically tested

When in the repo, a card has been executed at least once. Not theoretical.

**Static testing** — Cards are read, analyzed, questioned in conversation (like this one). Some deleted. Some refined. This is how they evolve.

**Reshaping** — Cards are not sacred. If reality changes, rewrite the card. If two should be one, merge. If one should be three, split.

**History preserved** — Git keeps every version. You see why a card changed.

## Examples from current library

**haskell.md** — Encodes: incremental dependency substitution, GHC version standards, extension guidance. Tested in real migrations. Comment clarifies: when to read (big refactors) vs. skip (linting).

**debug-filewatcher-chain** — Encodes: diagnostic method + hypothesis. Solid because diagnosis informs solution.

**comonad-instance-false-start** — Encodes: warning pattern. Recognize when instance is false. Teaches by negative example.

**library-metadata through library-tests** — Seven focused assessments. Each does one health check. Together they form a library profile.

**captains-job-jump** — Encodes: phased discovery with breathing points. Teaches workflow rhythm.

## When a card fails

If a card doesn't produce promised output:
- Don't retry 4 times (that signals pattern is wrong)
- Don't execute the fix manually (that signals approach is wrong)
- Ask instead (recovery is asking)
- Then decide: rewrite the card, or recognize the problem is novel and needs new work

## The card promise

A card says: "I've done this pattern. It works. You can spin it."

The operator accepts: "I trust this enough to try."

Both can be wrong. When they are, the conversation fixes it. The card gets rewritten or marked obsolete.

That's how the library stays honest.

