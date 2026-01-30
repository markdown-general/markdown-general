# Agent 1 - Over-specification Analysis (75% reduction target)

## What feels over-specified

1. **Card Flow Language section (lines 28-97)** — The entire gating language spec is elaborately detailed with symbols, logic syntax, recursion, nesting, and card-to-card linking. While the language exists, the document treats it as canonical specification rather than as illustrative example. The six subsections (basic structure, symbols, logic syntax, example, recursion, card-to-card linking, why this language) read like a formal grammar when cards themselves don't universally use this syntax.

2. **"Why this language" sub-section (lines 91-97)** — Lists five justifying principles (markdown-native, semantic bootstrap, operator-editable, agent-parseable, no ceremony) that essentially rationalize the same property repeatedly. These feel like defensive specification rather than necessary clarification.

3. **"Why this shape" section (lines 99-107)** — Each design decision (title as pattern name, instruction as imperative, comment as learning, no boilerplate) is explained as principle. The explanations are reasonable but the document treats them as rigorous justified choices rather than pragmatic guidelines that can shift.

4. **"How to write a card" section (lines 142-149)** — Six steps with line-count limit (7-10 lines max) feels prescriptive. Real card creation is messier; this reads like dogma rather than heuristic.

5. **"Examples from current library" section (lines 161-171)** — Lists seven specific cards with analysis. While illustrative, this section attempts to validate the model through examples rather than letting practitioners discover via use. It teaches by showing rather than trusting the reader understands.

6. **"When a card fails" section (lines 173-179)** — Three negations ("Don't retry 4 times", "Don't execute manually", "Ask instead") sound authoritative. These are useful heuristics but presented as rules.

7. **"The card promise" section (lines 181-188)** — Three paragraphs establishing a covenant between card and operator. Philosophically sound but verbose for a pragmatic concept.

## Core issue

The document conflates description of what cards ARE with prescription of how to write/use them. It treats emerging heuristics (gating language syntax, shape rules, testing practices) as established frameworks rather than working tools that should evolve.

## What should remain

- The essential definition: what a card is
- The minimal shape template
- One or two concrete examples showing actual usage
- Acknowledgment that cards change and aren't sacred
- Brief guidance on when to create a card

## What can be compressed or removed

- Detailed gating language spec (move to reference, not core doc)
- Multi-paragraph rationalizations of each design choice
- Step-by-step "how to write" prescriptions
- Library example catalog
- Formal sections on failure modes and promises
