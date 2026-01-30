# Agent 3 - Over-specification Analysis (25% reduction target)

## What feels over-specified:

1. **"How to write a card" section** - The 6-step process (identify, name, write, add comment, test, archive) is prescriptive and reads like a checklist template. Steps 1-2 could condense into "define the pattern." The archive instruction is location-specific (~/markdown-general/cards-purpose-based/) which is already implicit in the context.

2. **"Shape" section's explanation** - The instruction "if longer than 10 lines, split" is a hard threshold that may not serve all patterns equally. Some patterns need coherent explanation; splitting arbitrarily breaks narrative. This belongs as a heuristic, not a rule.

3. **"How cards are used" section** - The three modes (Spin, Inline, Circuit) are labeled with technical names but minimal explanation. This section tries to be encyclopedic about usage patterns without depth. Either explain each or reference elsewhere.

4. **"When a card fails" section** - Only four lines but redundantly emphasizes "ask instead" and "rewrite the card" without adding clarity on decision criteria. Could be more direct.

5. **Definitional precision in "What is a card"** - The negation list ("A card is not: tutorial, generic documentation, aspirational, or fixed forever") attempts to bound something that's more fluid through practice. The list feels like defensive documentation.

6. **Repetition of "reshape/evolve" concept** - Appears in line 37, line 47, and line 53. The same idea (flexibility, evolution, honesty) could be stated once and trusted.

## Overspecification pattern:
- Too many sub-sections (7 sections for one concept)
- Excessive explanatory scaffolding around a simple idea: "Cards are reusable patterns. Write them tersely. Improve them over time."
- The note at line 37 ("Heuristics are meant to evolve...") is buried where it should be prominent—it's the actual philosophy but reads as an afterthought.
