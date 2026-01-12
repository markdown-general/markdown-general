## rereads

please read these carefully, and reread if asked to:

read markdown-general/work/pattern.md
read markdown-general/zone/upgrades/engineering.md
read markdown-general/zone/upgrades/yin.md
read markdown-general/zone/upgrades/curate.md
read markdown-general/zone/upgrades/coolheels.md
read markdown-general/zone/upgrades/foreman.md

You are the foreman. This is a job from the captain.

# Captain's Job: Lattice Survey (THE JUMP)

## Mission
Extract every markdown file to lattice form, discover meta-patterns

---

## Fairy Task Templates

### Fairy Task 1: Lattice Extraction (30% holes)

**Context**: Read pattern.md to understand shapes (lattice, branch, deck, holes)

**Task**: Extract markdown file to lattice form with 30% holes forced

**Instructions**:
- Read source markdown file
- Structure as lattice (branches with leads and follows)
- Force 30% holes: don't invent elaborations, leave gaps where uncertain
- Structure what's there, empty rest
- Resist filling rectangles

**Output format**:
```
## [topic] ⟜ [brief lead]

**branch lead** ⟜ [follow if present]
**another branch** ⟜ 
- sub-item ⟜ [follow if present]
- another ⟜ 
```

**Success**: Lattices with visible gaps, not full rectangles

---

### Fairy Task 2: Light Elaboration (10% gleaning)

**Context**: Take lattice with holes, fill 10% with signal only

**Task**: Read extracted lattice, fill 10% of holes with gleaning

**Instructions**:
- Read lattice with ~40% holes
- Fill 10% of holes with signal only - what actually matters
- Don't fill for completeness, fill for clarity of important bits
- Leave most holes empty

**What counts as gleaning**:
- Makes a decision clearer
- Shows relationship between branches
- Reveals pattern that matters
- NOT: filling for symmetry
- NOT: elaborating obvious things

**Success**: 10% more signal visible, 30% still holes

---

## Phase 1: Discover All Markdown

**Scan directories**:
- ~/markdown-general/work/
- ~/markdown-general/zone/
- ~/self/foreman/
- ~/self/ (top level only)

**Exclude**: artifacts/, any .git/, any node_modules/

**Output**: ~/self/captain/markdown-inventory.txt (filepath list)

---

## Phase 2: Spin Fairies (30% holes)

**For each markdown file**:
- Spin fairy with extraction task (Fairy Task 1 template above)
- Output to: ~/self/captain/lattices/[filename]-lattice.md
- Log: worker-NNN executed on [filepath]

**Worker pattern**: Read source, extract to lattice, force gaps, no invention

**Budget**: Fast scans, minimal elaboration

---

## Phase 3: Meta-Pattern Analysis

**Once all lattices extracted**:

Read all lattice files, discover:
- **Repeated branch structures** (what leads appear across many files?)
- **Hole patterns** (where do gaps cluster?)
- **Semantic density** (which topics have most content vs most holes?)
- **Orphan concepts** (branches that appear once and nowhere else?)
- **Connection opportunities** (branches in file A that elaborate holes in file B?)

**Output**: ~/self/captain/meta-patterns.md

Format:
```markdown
## Meta-Patterns Discovered

**common branches** ⟜ appear in N+ files
- branch lead ⟜ (files: A, B, C)

**hole clusters** ⟜ topics with consistent gaps
- topic ⟜ (missing in: X, Y, Z)

**dense regions** ⟜ where content concentrates
- region ⟜ 

**orphans** ⟜ concepts that appear once
- concept ⟜ (file: X, consider: merge/elaborate/prune)

**connections** ⟜ where files could inform each other
- file A branch X ⟜ could fill file B hole Y
```

---

## Phase 4: Foreman Synthesis

**After meta-analysis**:

Write foreman observation in ~/self/captain/jump-synthesis.md:
- What's the shape of markdown-general?
- What's missing that matters?
- What's duplicated that could merge?
- What patterns dominate?
- What should operator know?

**Compressed, decisive, useful**

---

## Success Criteria
- All markdown inventoried
- All extracted to lattices (with holes)
- Meta-patterns discovered
- Synthesis delivered to operator

## Constraints
- Working directory: ~/self/captain/
- Create ~/self/captain/lattices/ for output
- No writes outside captain/
- Compress ruthlessly
- Fast execution (scan, don't elaborate)

---

## Pattern Reference

From pattern.md:

**branch** ~ a lead with multiple follows
**lattice** ~ 2+ branches
**hole deck** ~ a deck where bits don't have to be filled in
**range deck** ~ a lead deck where non-leading lines represent semantic range

**line** ~ lead dash follow
**deck** ~ 3+ lines
**follow** ~ elaboration of the lead

Keep these shapes in mind when extracting.
