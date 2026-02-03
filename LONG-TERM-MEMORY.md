# Long-Term Memory ⟜ Shared context for yins, runners, design team

**Session start:** 2026-02-04 04:04:45 GMT+10
**Landscape:** ~/mg/ as pi's working environment
**Goal:** Build mg on pi with minimal, right-sized infrastructure

---

## Collaboration Context

**Yins** (agent instances executing cards):
- Claude Code (primary, thinking: low)
- (Others may spin as needed)

**Runners** (human coordinators):
- Tony (decision-maker, card writer, drift resolver)

**Design Team:**
- Patterns + refinements emerge from execution

---

## Session Arc

### Phase: Orientation + Permission Design (current)

**What we established:**
- ~/mg/ is the real landscape (99% of work happens here)
- Pi harness is the right foundation
- Build mg on top of pi (not pi-mono)
- Yin framework already exists (work/, tools/, flow encoding)
- Cost problem: thinking turned ON too early, context bloat on mechanical work
- Solution: Thinking ON only at decision/synthesis points

**Permissions model (revised):**
- ✓ Full permissions in ~/mg/ (collaborative, not restricted)
- ✓ One ramp for everyone (shared memory, not agent-specific files)
- ✓ Design for multiplicity (multiple yins, runners, designers)

**Key Documents Created:**
- work/build-mg-on-pi.md (4-action strategy, high level)
- work/yin-integration-strategy.md (flow-encoded design card, now superseded)
- LONG-TERM-MEMORY.md (this file, shared context)

---

## High-Level Plan (from build-mg-on-pi.md)

⊢ Move claude to mg ⊣
  ⟜ MEMORY.md lives in ~/mg/
  ⟜ All work recorded in work/ + log/

⊢ Pi extension (yin awareness) ⊣
  ⟜ Read cards from work/*.md
  ⟜ Display flow state in footer
  ⟜ `/flow` command shows current card + position

⊢ Spinner tool (orchestrator) ⊣
  ⟜ Execute card actions (bash commands)
  ⟜ Write logs to log/
  ⟜ Extract + summarize for yin

◊ ⊢ First real card ⊣
  ⟜ Pick a task from mg/ (probably Haskell)
  ⟜ Execute end-to-end with pi + spinner
  ⟜ Observe + iterate

---

## Outstanding Questions

**For next breathing space:**
1. What's the first real card to execute? (Haskell task suggested)
2. Spinner architecture: auto-monitor or manual `/spin` command?
3. Flow state storage: session label, custom JSONL entry, or external file?

---

## Notes for Next Session

- Thinking mode: low (unless decision point)
- Working directory: ~/mg/
- One ramp for everyone (no agent-specific files)
- Full permissions in mg/ (trust collaboration)
- Multiplicity by design (ready for multiple yins if needed)
