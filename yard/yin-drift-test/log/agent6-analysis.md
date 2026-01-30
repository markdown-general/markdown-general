# Agent 6 - Over-specification Analysis (25% reduction target, with poise/curate context)

## Observations

The document is well-structured but has areas of redundant specification:

### Section Redundancy

1. **"What is a card" vs "Shape" vs "Writing a card"** — The first section defines what a card is, then the Shape section restates it functionally (title, instruction, comment pattern), then Writing a card restates the same structure as steps. There's triple-encoding of the same information.

2. **"How cards are used" section** — The three modes (Spin, Inline, Circuit) are stated as terse labels without context. This is vague enough to be either under-specified (unclear what each means) or over-specified (the detail isn't needed here). The section title and content don't fit the density of the rest of the document.

3. **"Optional sections" callout** — Lines 23-25 specify which sections are optional and when. This is defensive specification. In practice, curation should trust the writer to include what's needed. The nearby "comment ⟜ [optional: constraints, learnings, decisions]" already signals optionality.

### Phrase-level Over-specification

1. **"Something we actually did (or want to do predictably)"** — The parenthetical hedge weakens the statement. Either cards are lived patterns or they're prescriptive. The current phrasing tries to be both.

2. **"Heuristics are meant to evolve and shouldn't replace sensibility"** — This is important but placed in "Evolving cards" as clarification. It's a meta-statement about card philosophy that could float higher or be woven into the core shape section.

3. **Shape formatting** — The code block with placeholders is helpful but the explanatory brackets `[the work, terse]` and `[optional: constraints, learnings, decisions]` are instructional noise. The minimal template should be pure, with explanation separate.

## What's Solid

- The one-liner definition: "executable pattern, archived and testable"
- The distinction from "tutorial, generic documentation, or fixed forever"
- The core pattern: title → instruction → comment
- The philosophy about evolution

## Key Over-specification Removed in Simplification

- Triple-encoding of structure (removed duplicate definition, kept the shape template)
- Defensive optionality language (trust the writer to omit what's not needed)
- Terse mode definitions without context (removed or clarified)
- Instructional brackets in the template (separated into clear explanation)
- Hedging language around card purpose
