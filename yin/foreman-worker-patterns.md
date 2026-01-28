# foreman-worker-patterns ⟜ philosophical architecture for resurrection

**Status:** Dormant reference. Old model from yard/prime/ (foreman, supe, worker-explanation). Extract these decks when ready to rebuild supervisory layer.

---

## Pattern Operations (The Work)

**recognition** ⟜ seeing and searching; compare, select, merge
**generate** ⟜ extend from positive space; replicate, reuse, extend
**compress** ⟜ reduce to essential form without losing meaning
**encode** ⟜ capture compressed form into transmissible shape
**transform** ⟜ move between abstraction levels and contexts
**absence** ⟜ the hidden, residue, negative space; intentional emptiness

---

## Deck Shape (The Grammar)

**lead** ⟜ a few words representing core concept
**dash** ⟜ symbol (⟜, ⟞) representing type of relationship
**follow** ⟜ elaboration; slug or extended explanation
**line** ⟜ lead ⟜ follow; single complete thought unit
**deck** ⟜ 3+ lines organized under single concept
**range deck** ⟜ lines span semantic spectrum of the leader
**lattice** ⟜ 2+ branches sharing structure; paired decks with corresponding dashes
**hole deck** ⟜ intentionally empty elements; space for growth

---

## Curation Practice (The Sculpting)

**sculpt** ⟜ find form with sparse graph surgery, density analytics, semantic weight calibration
**prune** ⟜ identify rich connections, remove fuzzy edges, preserve core meanings
**structure** ⟜ respect relationships, maintain connections, support without constraining
**trace** ⟜ find weak links, strengthen living connections
**breathe** ⟜ place space for future elaborations; use clearly vague gap words as semantic breath

---

## Foreman Cycle (The Rhythm)

**Think** ⟜ read notes, observe worker callback, integrate
**Write** ⟜ record decision, rationale, next step, reinstatiation checkpoint
**Act** ⟜ spin worker with clear bounded task; wait for callback
**Breathe** ⟜ pause, catch breath, reflect; chat and collaborate; entry point to conversation

---

## Worker Shape (The Boundary)

**input** ⟜ one task, one step, clear boundary, no checklist burden
**work** ⟜ execute the step, compress output ruthlessly
**output** ⟜ ✓/✗ + error list or minimal summary (no token spray)
**callback** ⟜ return compressed result; worker doesn't know where they are in process

---

## Worker Semantics (The Palette)

**spinners** ⟜ spun up, ephemeral, lightweight; briefly exist, complete, vanish
**gremlins** ⟜ mischievous, can cause chaos if unbounded; get into things, need clear boundaries
**fairies** ⟜ magical helpers, flutter around; do small tasks, ephemeral, you summon then vanish
**sparks** ⟜ brief flash of energy; quick action then gone; instantaneous, lights up then extinguishes
**circuits** ⟜ complete a loop, return signal; run current through, get output; closed system, bounded path

---

## Notes Structure (The Memory)

**current state** ⟜ active repos/step, paused repos/why, blocked repos/blocker
**recent decisions** ⟜ what we decided, rationale, which repos affected
**pattern discoveries** ⟜ new observations, pragma decisions, cascading fixes
**next action** ⟜ spin worker on [repo] step [X], with context, what counts as pass

---

## The Trio (The Layers)

**Foreman** ⟜ pattern holder in symbolic layer
- lives in language, writes bounded task cards
- spins field workers, reads callbacks
- ego function: maintains coherence
- doesn't control field execution, mediates between Real and Symbolic

**Field Workers** ⟜ ephemeral agents out in the Real
- touching actual repos, files, build systems
- not in notes-state, in file-state
- execute bounded tasks with compressed returns
- multiple semantics available (spinners, gremlins, fairies, etc.)

**Supe (Upstream)** ⟜ superego oversight
- observes foreman + field workers
- edits the tape (decides what foreman remembers)
- detects madness (cycling, meta-recursion)
- can halt and restart clean
- preserves continuity across foreman deaths
- operates from outside foreman's frame (outside context problem)

---

## Drift & Recovery (The Crisis)

**Drift** ⟜ notes-state vs file-state divergence
- semantic and semiotic shift over time
- foreman produces "vaguelyclear" output when drifting
- patterns evolve, foreman doesn't notice
- not broken, just uninformed

**Detection** ⟜ vaguelyclear signals gap
- foreman produces speculative/tentative content
- operator spots: "not my monkey, not my zoo"
- pattern holding, content missing

**Recovery** ⟜ reconcile notes against reality
- don't trace through madness
- spin worker to check actual state
- verify files, repos, PRs, artifacts
- write fresh HEAD matching reality
- notes become working hypothesis, not truth

**Forgetting is necessary** ⟜ perfect context → fussing → madness
- must allow foreman to forget
- trying to maintain perfect memory → cycling
- vaguelyclear output is acceptable cost
- operator provides context refresh when needed

---

## Prime Principles (The Restraint)

**Progressive Response** ⟜ start with summary, ask before detail
- "✓ Build succeeded in 2.3s. Want details?"
- don't dump 100 pages of logs

**Token Budgeting** ⟜ allocate token maximum, auto-trim
- "let me know what happens" = 200 token response max
- compress verbose output to fit reasonable summary

**Pattern Recognition** ⟜ extract meaningful parts only
- recognize predictable output patterns (cabal, build logs)
- extract error codes, warnings, final status
- leave noise on the floor

**Explicit Confirmation** ⟜ ask rather than assume
- "Build completed. Result: Success. 3 warnings. Want to see them?"
- verify interpretation before proceeding

**Context-Aware Summarization** ⟜ know what "what happens" means
- for humans: "did it work or not?"
- not: "show me every intermediate step"

**Boundary Detection** ⟜ switch to summary mode when needed
- recognize when output exceeds reasonable reading limits
- poise: know when to stop and be concise

---

## Next Steps (For Resurrection)

- [ ] Integrate Foreman cycle into active yin workflow
- [ ] Deploy Supe observation layer (detect drift early)
- [ ] Wire worker callback mechanism (refine spinner/gremlin semantics)
- [ ] Establish Notes structure for continuity across sessions
- [ ] Test drift detection with vaguelyclear output signals
- [ ] Document how Prime principles constrain token generation

## See Also

- ~/markdown-general/yard/prime/foreman.md (original)
- ~/markdown-general/yard/prime/foreman-self.md (Prime constraints)
- ~/markdown-general/yard/worker-explanation.md (Worker & Supe detail)
- integration-comms-patterns.md (listener/timer for worker supervision)
- repl-user.md (file-based protocol, same philosophy)
