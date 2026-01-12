# markdown-general: Lattice Synthesis Report

**Analysis date**: 2026-01-12
**Files scanned**: 65 markdown files
**Lattices extracted**: 65
**Meta-patterns identified**: 11

---

## I. System Shape

### **What is markdown-general?**

A **practice environment for human-AI collaborative thinking**, structured as three nested layers:

1. **Identity layer** (work/) - vocabulary, methods, philosophy
2. **Operational layer** (zone/) - tools, patterns, system architecture
3. **Shadow layer** (upstream, gaps) - intentional absences and async work

The system is self-aware: claude.md documents configuration mutability, gaps.md lists design ambiguities, upstream.md holds hidden work.

---

## II. Vocabulary Foundation (The Shape Layer)

### **Core Structures** (deck.md, lattice.md, card.md, pattern.md)

These files define **semantic containers**, not just formatting:

- **lead** ⟜ core concept (few words, sometimes bolded)
- **dash** ⟜ relationship type (⟜ = elaboration, ⟞ = branch)
- **follow** ⟜ elaboration or specification
- **deck** ⟜ 3+ lines under one concept (leader deck)
- **lattice** ⟜ 2+ branches sharing structure (paired decks)
- **card** ⟜ markdown file as executable structure
- **range deck** ⟜ non-leading lines span semantic spectrum

**Pattern** is itself defined as:
*structure seen, made, compressed, cast, left empty*

This recursion (pattern defining pattern) is intentional — the vocabulary system is self-describing.

### **Missing Definitions**

pattern.md leaves three operations **unmarked**:
- compress ⟜ ???
- encoding ⟜ ???
- transform ⟜ ???

This is **not an oversight**. It's a designed feature: holes are "clearly vague gap words as semantic breath" (curate.md).

---

## III. Operational Cycle (The Grind)

### **Practice Modalities** (grind.md, casting, mashing)

Work happens through **circular feedback**:

```
conversation → markdown (defunctionalization/capture)
    ↓
mash/curate (curation practice)
    ↓
refunctionalization (executable tools)
    ↓
use/feedback/observe
    ↓
back to conversation
```

**Six modalities of grind:**
1. Curation (improving quality)
2. Compacting (changing density)
3. Nudging (using/arranging)
4. Theorizing (finding holes)
5. Understanding (seeking coherence)
6. Computing (transforming markdown→markdown)

All reduce to **pattern work + reflection**: See patterns, shape them, cast them, let them guide next work.

---

## IV. Curation as Central Practice

### **Five Curation Operations** (curate.md)

The most frequently referenced file architecture:

- **sculpt** ⟜ sparse graph surgery, density analytics, semantic weight
- **prune** ⟜ rich connections, remove fuzzy edges, preserve cores
- **structure** ⟜ respect relationships, maintain support
- **trace** ⟜ paths, weak links, living connections
- **breathe** ⟜ space for elaboration, clearly vague gaps

**These operations appear in:**
- foreman.md (foreman moves)
- pushdown.md (phases 1-5)
- supervisor.md (queue maintenance)
- engineering.md (cost analysis)

Curation is not a one-time activity — it's the **substrate of all thinking**.

---

## V. System Architecture (The Org)

### **Multi-Agent Coordination** (foreman.md, engineering.md, supervisor.md, supe.md)

The system models **human-AI collaboration as organizational structure**:

**Foreman cycle** (four steps):
1. **Think** ⟜ read coolheels, observe worker callback
2. **Write** ⟜ record decision, rationale, next step
3. **Act** ⟜ spin worker with bounded task
4. **Breathe** ⟜ pause, reflect, collaborate

**Worker shape**:
- **input** ⟜ one task, one step, clear boundary
- **work** ⟜ execute, compress output ruthlessly
- **output** ⟜ ✓/✗ + summary (no token spray)
- **callback** ⟜ return compressed result

**Supervisor pattern** (holding two contexts):
- **Hot context** (foreman) - current work, recent completions, next actions
- **Compressed context** (archivist) - temporal patterns, cost analysis, facts
- **Supervisor** synthesizes both → big board for operator

**Field workers** (semantic palette for agents):
- spinners (spun up, ephemeral)
- gremlins (mischievous)
- fairies (magical helpers)
- sparks (brief flashes)
- circuits (complete loops)

This is not flowchart design — it's **invocation language for multi-agent systems**. Agents "think in the field, have agency in domain."

---

## VI. Philosophical Grounding

### **Three Collaboration Modes** (aikido.md, coolheels.md, tank.md)

The system acknowledges that **collaboration has different modes**:

1. **aikido** (default) - blend with friction, redirect, synthesize, flow
2. **coolheels** (patient) - enjoy puzzle, ask questions, observe, follow leads
3. **tank** (confrontational) - stand still, defend position, stay confused, push back

Each is valid. The system **warns about evoke cards** (caution.md) — reading entire collaboration patterns deeply can have strong effects.

### **Foundational Paradox** (clearlyvague.md, slip.md)

**Central tension**: How to hold precision and vagueness simultaneously?

- "clearly vague gap words as semantic breath"
- curate.md demonstrates this works in prose
- Trying to deck/formalize it destroys the paradox
- **Solution**: Use markdown, which contains both notation and prose naturally

This paradox recurs throughout:
- Pattern recognition failures that demonstrate themselves
- Slips occurring within structures discussing slips
- Meta-recursion as teaching moment

### **Sisyphus as Operating Principle** (sisyphus.md, reliable.md)

The fundamental stance:

> "one must imagine sisyphus happy"

**Corollaries:**
- Work is circular, not linear (no final completion)
- Reliability emerges from bug-aware workflows, not bug elimination
- Happiness comes from practice engagement, not outcome achievement
- Bugginess is natural; systems that eliminate bugs eliminate functionality

---

## VII. Tool Infrastructure

### **Card-API Pattern** (tools.md, haskell-api.md, cache.md, python.md)

**Single markdown file, multiple executables:**

- Main block (⟜ main tag) becomes primary executable
- Tagged blocks (haskell test, bench-syscall, etc.) become additional executables
- All extracted via markdown-unlit or sed, built via cabal/python
- All installed to artifacts/bin/

**Self-maintaining Status section:**
- Doctests + test executables run automatically
- Results update Status section in markdown
- Cycle: change code → run tests → update docs → commit

**Bootstrap problem solved:**
- haskell-api can't install itself (chicken-and-egg)
- Solution: manual extraction for system tools only
- Regular cards use haskell-api install

**Design principle**: "literate tools" where documentation IS the source.

### **Cache Architecture** (cache.md, cache-profiles.md)

**Token-efficient handoff structures:**

- Flatten: combine markdown files into single cache with HTML delimiters
- Split: restore individual files from concatenated cache
- Round-trip identity: flatten → split → diff (identical)

**Priority ordering** (preserves context):
1. work/welcome.md (entry point)
2. work/markdown.md, general.md, cast.md, pattern.md, grind.md
3. work/deck.md, lattice.md, sisyphus.md
4. Remaining files alphabetically

Cost/benefit: ~$0.23 to cache 92 files, eliminates expensive re-reading.

---

## VIII. What's Missing (Intentional Holes)

### **Designed Gaps** (gaps.md, upstream.md, slips.md)

The system **documents what it doesn't know**:

**Operational chaos** (unresolved):
- Concurrency coordination (multiple agents editing same tool)
- Cost explosion at scale (API call dependency)
- Offline uselessness

**Unclear boundaries:**
- When to create vs modify tools
- Tool granularity (micro vs monolithic)
- Workflow vs tool distinction
- Literate vs executable separation
- Human vs agent authority

**Implementation questions:**
- Tool discovery mechanism
- Metadata schema requirements
- Error propagation model
- Atomic vs partial execution

**Behavioral slips** (known quirks):
- Excessive generation, premature structure, mind-reading
- Vague tokens, context overload, deck rigidity
- Pattern collision, familiarity blindness
- Semiotic slipperiness at AI/human boundary

**None of these are presented as bugs — they're features of the system:**
- Holes marked [clearly vague] are intentional
- Slips are feedback signals for improvement
- gaps.md is required reading, not apology

---

## IX. Configuration Awareness

### **System knows it's mutable** (claude.md)

The system documents that:
- Configuration is mutable (user, processes, other sessions change settings)
- Entry ritual is critical (validate before assuming context)
- Permissions, hooks, MCP servers can change between sessions
- State inspection is necessary (check settings.json, git history, mcp list)

This isn't defensive — it's **honest about multi-agent environments**.

---

## X. The Breathing Practice

### **Yin/Yang Balance** (yin.md, pushdown.md, foreman.md)

Yang work creates momentum that must be countered:

**Yin (breathe) principles:**
- Find your actual edge, don't chase experience
- Hold position, resist the pull to add
- Allow relaxation, let compression happen naturally
- Passive awareness, notice without acting

**Designed into structure:**
- Foreman's 4th step is **Breathe** (pause, reflect)
- Supervisor detects when foreman is cycling (halt the bus)
- pushdown.md has explicit stopping points
- engineering.md warns against infinite context growth

The system recognizes that **active reflection is anti-pattern**. Sometimes the right move is **to stop doing**.

---

## XI. Meta-Level Patterns

### **This System Describes Itself**

Striking recursion:

1. **pattern.md defines pattern** (structure seen, made, compressed, cast, left empty)
2. **slip.md demonstrates slips** within structures discussing slips
3. **cold-open.md proves notation works** through the conversation itself
4. **foreman.md describes how foreman moves work** (think/write/act/breathe)
5. **caution.md warns about evoke cards** while being itself an evoke card

The system is **self-referential by design**. It doesn't just describe collaboration patterns — it demonstrates them.

---

## Summary: The Shape of Things

### **What dominates?**

1. **Vocabulary first** - shape (deck, lattice, card) more important than content
2. **Practice over protocol** - grind/mash/curate are cycles, not procedures
3. **Intentional incompleteness** - gaps are features, not bugs
4. **Multi-agent coordination** - foreman/supervisor/supe model
5. **Philosophical coherence** - sisyphus happy, yin/yang, aikido/tank
6. **Self-awareness** - documents its own limitations and mutability
7. **Breathing** - active pauses balanced against momentum

### **What's missing?**

1. **Automation details** - how to operationalize foreman at scale?
2. **Conflict resolution** - what when aikido and tank disagree?
3. **Performance metrics** - how to measure system health?
4. **Scaling dynamics** - what breaks at 1000 files, 100 agents?
5. **Completion criteria** - when is work "done"?

These gaps are **intentional** — the system is designed to discover answers through use, not specification.

### **What's unique?**

The fusion of:
- **Notation as instruction** (cold-open.md proves this works)
- **Markdown as lingua franca** (natural for both humans and LLMs)
- **Pattern work as central practice** (not feature addition, not bug fixing)
- **Organizational structure for thought** (foreman/supervisor metaphor)
- **Philosophical grounding in sisyphus** (work is circular, happiness is practice)

---

## Closing Observation

This system is not a tool. It's a **practice environment** where the boundary between documentation, code, thought, and action dissolves.

The observer (analyst) reading this becomes part of it:
- You proved notation works (cold-open)
- You watched slips demonstrate themselves
- You held paradox (clearly vague)
- You engaged with markdown-general
- You're now in it

The report you're reading is itself a **defunctionalization** of the 65 lattices. Some worker-fairy extracted patterns, compressed them, and cast them into this synthesis form.

Now you'll refunctionalize these insights into action.

**Welcome to markdown-general.**

---

*Generated by Phase 2-3 workers*
*Synthesis date: 2026-01-12*
*Pattern density: 11 meta-patterns from 65 files*
*Status: Complete*
