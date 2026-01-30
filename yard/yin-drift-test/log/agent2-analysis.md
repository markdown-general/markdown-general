# Agent 2 - Over-specification Analysis (50% reduction target)

## Analysis

The document contains several areas where specification exceeds what's necessary:

1. **"What is a card" section** — The negative definition ("is not: tutorial, generic documentation...") adds conceptual weight without instructional value. The four bullets on what a card IS already communicate the essence.

2. **Shape section** — The template example is clear, but the explanatory notes about optional sections ("only if they add signal") are prescriptive meta-commentary that duplicates the spirit of "keep it terse."

3. **"How to write a card"** — Six numbered steps with explanatory prose (e.g., "One line, active verb" with examples in parentheses). Steps 3-4 especially add advisory language ("If longer than 10 lines, split") that constrains rather than guides.

4. **"How cards are used"** — Three usage modes (Spin, Inline, Circuit) with terse descriptions. This section feels necessary but the context around it (explaining that cards serve different execution patterns) is embedded in main prose rather than being a standalone reference.

5. **"When a card fails"** — The advice to "ask instead of retrying" is valuable but exists in tension with the implicit assumption that cards are proven patterns. The section philosophizes rather than instructing.

6. **"The library"** — This section shifts to organizational philosophy (cards as persistence layer, conversation as ephemeral) that, while true, extends beyond practical guidance on how to write or use cards.

## Over-Specification Summary

The document over-specifies by:
- Including philosophical grounding alongside practical instruction
- Adding cautionary/prescriptive language ("only if," "shouldn't," "don't") that acts as rules rather than heuristics
- Mixing declarative statements about what cards ARE with imperative instructions on what to DO
- Explaining the _why_ cards exist (library strategy) when the document is about _how_ to create them
