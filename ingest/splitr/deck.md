# Deck Format

**A deck is 3-6 lines formatted with bolded lead tokens**

---

## Instructions

Format lists with **bolded lead tokens** that flow naturally into the sentence. Use ⟜ (U+27DC reverse lollipop) as separator, or lose the lolli where natural punctuation (colon, comma, dash) or direct flow works better.

**Token:** The core concept (1-3 words as needed) - not about counting words but finding the natural semantic unit.

**Density:** Maintain original density when editing - don't expand or compress unnecessarily.

**Use for:** Both creating new content and editing existing lists into deck format.

---

## When to Lose the Lolli

Use natural separators when they carry semantic meaning:

- **Colon (:)** for definitions or explanations
- **Comma (,)** for natural pauses or continuation
- **Em dash (—)** for elaboration
- **Nothing** when the token flows directly into the sentence

The lolli is optional decoration - use it for visual consistency or lose it when natural punctuation works better.

---

## Example 1: Directory Types

**Original:**
```markdown
## self/

Experimental knowledge undergoing mashing.

- Rough thinking, wild experiments
- AI or human authorship doesn't matter
- Safe space to try things without preserving them
- Gets challenged, replaced, evolved
```

**As deck:**
```markdown
## self/

Experimental knowledge undergoing mashing.

- **Rough thinking** ⟜ wild experiments in various states
- **Authorship agnostic** ⟜ AI or human contributions treated equally
- **Safe space** ⟜ try things without obligation to preserve
- **Gets challenged** ⟜ replaced and evolved continuously
```

---

## Example 2: Stable Knowledge

**Original:**
```markdown
## base/

Crystallized knowledge that survived mashing.

- Core processes, stable understanding
- Frequently referenced
- Still malleable - nothing is permanent
- Can be challenged and replaced
```

**As deck:**
```markdown
## base/

Crystallized knowledge that survived mashing.

- **Core processes** ⟜ stable understanding that's frequently referenced
- **Still malleable** ⟜ nothing is permanent
- **Can be challenged** ⟜ replaced when better versions emerge
```

---

## Example 3: Process Description

**Original:**
```markdown
## What "Mashing" Means

- Flatten structure into markdown
- Strip scaffolding, reveal content
- Defunctionalize: turn functions into data
- Transform aggressively knowing it will be transformed again
- Challenge existing content with new content
- Replace weaker versions with stronger ones
```

**As deck:**
```markdown
## What "Mashing" Means

- **Flatten structure** ⟜ into markdown
- **Strip scaffolding** ⟜ reveal content
- **Defunctionalize** ⟜ turn functions into data
- **Transform aggressively** ⟜ knowing it will be transformed again
- **Challenge existing** ⟜ with new content
- **Replace weaker** ⟜ with stronger versions
```

---

## Token Selection

**Good tokens:**
- **Flatten structure** (2 words, complete concept)
- **Strip** (1 word, clear action)
- **Defunctionalize** (1 word, technical term)
- **Transform aggressively** (2 words, manner + action)
- **Challenge existing** (2 words, action + object)

**The pattern:** Extract the core concept before the sentence continues. Could be 1 word, could be 3, depends on the semantic boundary.

---

## Deck Size

**Target:** 3-6 lines per deck

**Too few (<3):** Probably not worth deck formatting
**Too many (>6):** Consider splitting into multiple sections or using a different format

A deck should be scannable at a glance - the bolded tokens create a visual outline of the key concepts.

---

## Usage

**Creating new content:**
Start with rough bullets, then identify the lead token in each line and bold it.

**Editing existing content:**
Find list sections, identify natural tokens, bold them, adjust density if needed (but keep it close to original).

**When not to use decks:**
- Single items (use regular text)
- Very long explanations (use paragraphs or headers)
- Code examples (use code blocks)
- Simple enumerations without conceptual weight
