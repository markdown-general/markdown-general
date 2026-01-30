# Agent 4 - Over-specification Analysis (75% reduction target, with poise/curate context)

## What feels over-specified

The document over-constrains the card concept in several ways:

### 1. **Shape section is too prescriptive**
The template in the Shape section with its specific markdown syntax (the ⟜ symbol, specific formatting) is more ceremony than necessary. Cards are about repeatable patterns, not about enforcing a particular visual representation. The triple-backtick code block showing the template creates a false sense that this *exact* format is required, when in reality the key is clarity.

### 2. **Explicit optional sections undermine the message**
Listing "**input**" and "**output**" as "Optional sections only if they add signal" is itself over-specification. The entire premise of cards—as stated in "Evolving cards"—is that heuristics are meant to evolve. Telling people which optional sections to consider creates the assumption that these are the *canonical* optional sections, limiting creativity in how cards might take shape.

### 3. **"Writing a card" steps are linear and prescriptive**
The five-step process (1. Identify, 2. Name, 3. Write instruction, 4. Add comment, 5. Archive) feels like a workflow waterfall. In practice (as seen in poise.md and curate.md), good work is iterative: you might name it, discover the pattern is unclear, rename it, then write. The numbered list creates false process structure.

### 4. **Execution modes section is under-explained yet exists**
The line "**Spin** — Agent executes independently... **Inline** — Execute via bash... **Circuit** — Background work..." introduces three execution modes with almost no elaboration. Either explain them fully or remove them. As written, they feel like residual jargon that lost its context.

### 5. **Redundancy in core concept**
The opening repeats the essence three times: "A card is a markdown file...", "It's: - Something we actually did...", and "A card is not:...". This is explanatory scaffolding, but combined with the shape section, it over-builds the concept.

## Core issue
The document tries to be both **definitional** (what IS a card) and **prescriptive** (how to write one). This creates the feeling of over-specification because the prescriptive parts lock in heuristics that should be flexible.

## How poise.md and curate.md differ
Both are **descriptive**, not prescriptive. Poise offers principles (enjoy, be patient, follow leads, observe). Curate describes an activity (trace, prune, structure, breathe) without saying "you must do these in order" or creating false optionality lists. They trust the reader.

Card.md should do the same: describe what cards are, maybe hint at how they're typically used, then stop. Let sensibility guide the shape.
