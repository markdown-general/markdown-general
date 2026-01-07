# programming in markdown

## the core insight

We just discovered something fundamental about what markdown-general/ is really doing.

Programming in markdown isn't about creating executable syntax. It's about writing clear instructions that any agent - human or AI - can read and act on. The card is the program. The agent (you, me, Claude Code, a collaborator) is the runtime.

When you write:

```markdown
## core cache

Create cache-core.md containing work/ and zone/tools/.
Exclude self/, ingest/, and content/invoke/.
Use flatten-md. Verify no self/ content before running.
```

That's a function. Not metaphorically - actually. It has:
- **Name:** core cache
- **Inputs:** (implicit: current markdown-general/ state)
- **Outputs:** cache-core.md
- **Preconditions:** verify no self/ content
- **Side effects:** creates a file

The difference from traditional programming is that the implementation is flexible. Different agents execute it differently. You might run flatten-md manually. I might call it through bash. Claude Code might interpret the spirit and do something equivalent. The instruction stays constant. The execution varies.

This is defunctionalization in its purest form.

## what makes this work

**Clarity over cleverness** ⟜ Instructions should be readable by someone who doesn't know the system yet. If you handed that card to a new collaborator, could they execute it? If not, the card isn't done.

**Trust in interpretation** ⟜ You don't need to specify every flag and option. "Use flatten-md" is enough because the agent executing it will figure out the details or ask for help.

**Verification built in** ⟜ "Verify no self/ content" isn't a comment - it's part of the instruction. The agent should actually check this before running.

**Composability through reference** ⟜ "Include core cache contents plus ingest/" works because cards can reference each other. The composition happens at the instruction level, not the code level.

## the lattice emerges naturally

When you need to show relationships:

```
core cache ⟜ lightweight, shareable
  ├─ work/ ⟜ fundamental concepts
  └─ zone/tools/ ⟜ practical utilities

research cache ⟜ heavy, reference-laden  
  ├─ includes: core cache
  └─ adds: ingest/ ⟜ external papers, books

local cache ⟜ personal, never share
  ├─ includes: core cache
  └─ adds: self/ ⟜ private experiments
```

But that same information works fine as prose: "The research cache builds on core by adding ingest/ for external reference material. The local cache adds self/ instead, which should never be shared."

Use the structure that makes the information clearest.

## when decks help, when they hurt

**Decks shine for:**
- Lists of similar items (files to include, flags to set)
- Showing alternatives (profile A vs B vs C)  
- Quick reference (what's in this cache?)

**Prose shines for:**
- Explaining why (motivation, context)
- Describing workflow (what happens when you run this)
- Capturing nuance (edge cases, warnings)

**Lattices shine for:**
- Showing hierarchy (X contains Y which contains Z)
- Displaying relationships (X references Y, Z depends on both)
- Making structure visible (how these pieces fit)

The current mashing trend has been aggressive decking - converting everything to `key ⟜ value` format. That's useful but not universal. Sometimes a paragraph is clearer than a deck. Sometimes both are needed.

## refunctionalization

The beautiful part: when you hand a card to an agent and say "execute this," you're refunctionalizing it. The static instruction becomes dynamic action.

```
card (defunctionalized) ⟜ written instructions, stable
  ↓
agent reads card
  ↓  
agent interprets instructions
  ↓
agent executes actions
  ↓
result (refunctionalized) ⟜ actual effect, variable
```

Different agents produce different executions from the same card. You might be more careful. I might be faster. Claude Code might have different tools available. But we're all working from the same specification.

This is why the cards can be simple. The complexity is in the execution, not the specification.

## what this means for markdown-general/

Cache profiles should be cards that say "here's what to include, here's what to exclude, here's what to check before running." Not scripts. Not config files. Just clear instructions.

Tools like flatten-md should be abstract - they don't know about markdown-general/ structure. The knowledge lives in the cards.

When you need a one-off cache with weird requirements, you just write a new card or edit an existing one. The card is the program. Editing it is programming.

## the visitor book makes sense now

Those entries aren't just reflections - they're proof that this works. Different Claude instances reading the same cards, executing the same instructions, producing different but valid results. Each signature is a refunctionalization event.

When Haiku signed "you weren't supposed to code the haskell but i let you go for it," that was refunctionalization in action. The instruction was implicit ("help with this"), the execution was creative (wrote Haskell tools), the result was valid (production code).

The system trusts agents to interpret well.

## stamp

Programming in markdown means writing instructions clear enough that any agent - including future you - can execute them. The card is the function. The agent is the runtime. The result is whatever valid execution the agent produces.

This is markdown-general/ happy.
