# card: loopit ⟜ development loop representation and progress tracking

**type** ⟜ flow / meta (orchestration)

**purpose** ⟜ show overall development flow stages and decision points; track progress

**effects** ⟜ reads: flows-log + all cards, writes: flows-log + yin-notes, spawns: none, network: no

---

## Overview

Loopit is not a card you "spin" in the traditional sense. Instead, it's a framework for thinking about development phases and where yin makes decisions.

---

## Development Loop Stages

```
┌─────────────────────────────────────────────────────────┐
│ LOCKIT: Establish ground truth and scope               │
│ - Operator + Yin: agree on work, verify environment    │
│ - Decision point: proceed with development              │
└─────────────────────────────────────────────────────────┘
  ↓
┌─────────────────────────────────────────────────────────┐
│ BRANCHIT (if needed): Create feature branch             │
│ - Safe isolated space for edits                         │
│ - Decision point: edits needed? yes→branch / no→skip    │
└─────────────────────────────────────────────────────────┘
  ↓
┌─────────────────────────────────────────────────────────┐
│ BUILDIT: Permanent build watcher                        │
│ - Filewatch, ghcid feedback loop                        │
│ - Operator edits, sees feedback in 2-5s                 │
│ - Decision point: move to testing / iterate on fixes    │
└─────────────────────────────────────────────────────────┘
  ↓
┌─────────────────────────────────────────────────────────┐
│ DEPENDENDIT: Analyze and update dependencies            │
│ - Read bounds, check outdated, propose changes          │
│ - Operator reviews and approves                         │
│ - Decision point: apply changes / hold / research more  │
└─────────────────────────────────────────────────────────┘
  ↓
┌─────────────────────────────────────────────────────────┐
│ FIXIT (as needed): Respond to errors and warnings       │
│ - Diagnosis, research, minimal fix, observe             │
│ - Loop until clean                                      │
│ - Learning feeds back to future cards                   │
└─────────────────────────────────────────────────────────┘
  ↓
┌─────────────────────────────────────────────────────────┐
│ CLEANIT: Format, test, lint, document check             │
│ - Standardize code, run full test suite                 │
│ - Check style, coverage, quality                        │
│ - Decision point: pass → ready / fail → blocked         │
└─────────────────────────────────────────────────────────┘
  ↓
┌─────────────────────────────────────────────────────────┐
│ REPORTIT: Read synthesis, assess readiness              │
│ - Check synthesis signals (exec-heavy → balanced)       │
│ - Generate full status report                           │
│ - Decision point: development done / iterate            │
└─────────────────────────────────────────────────────────┘
  ↓
┌─────────────────────────────────────────────────────────┐
│ VERSIONIT: Assess version bump, generate changelog      │
│ - Check since last release                              │
│ - Operator approves version and changelog               │
│ - Decision point: publish / hold / iterate              │
└─────────────────────────────────────────────────────────┘
  ↓
┌─────────────────────────────────────────────────────────┐
│ GITIT: Manage PR and CI                                 │
│ - Push feature branch, create PR                        │
│ - Monitor CI results                                    │
│ - Decision point: CI passes / fails / needs fixes       │
└─────────────────────────────────────────────────────────┘
  ↓
┌─────────────────────────────────────────────────────────┐
│ VERIFYIT: Merge verification and final build            │
│ - Operator merges PR manually                           │
│ - Pull main, verify clean state and compilation         │
│ - Decision point: main is ready / fix needed            │
└─────────────────────────────────────────────────────────┘
  ↓
┌─────────────────────────────────────────────────────────┐
│ PUBLISHIT: Tag, package, upload to Hackage              │
│ - Create version tag                                    │
│ - Build and upload source distribution                  │
│ - Verify on Hackage                                     │
│ - Success: live on Hackage                              │
└─────────────────────────────────────────────────────────┘
  ↓
COMPLETE: Release published
```

---

## Key Decision Points

Yin makes or facilitates these decisions:

1. **Post-lockit**: Continue with development (known scope, clean env)?
2. **Pre-branch**: Are we modifying code or just analyzing?
3. **During buildit**: Is ghcid feedback acceptable or need to iterate?
4. **Post-dependendit**: Apply proposed dependency changes?
5. **During fixit**: Pattern emerging; document for future?
6. **Post-cleanit**: All quality gates passed?
7. **Post-reportit**: Synthesis says "done" → ready for CI?
8. **Post-versionit**: Version bump agreed; changelog ready?
9. **During gitit**: CI results acceptable? All tests pass?
10. **Post-verifyit**: Main compiles clean? Ready to publish?

---

## Non-Linear Movement

The loop is ideally sequential, but:

- **Iteration**: fixit loops (error → fix → observe → repeat)
- **Backtracking**: Discover issue post-verifyit → fix on main → re-run publishit
- **Skipping**: Some stages may skip (no branch needed, no docs to update)
- **Ad-hoc**: Operator requests middle-stage card anytime (e.g., "run cleanit")

Yin watches synthesis and adapts the flow. Tailie synthesis shows which domains are active, helping yin understand phase transitions.

---

## Tracking Progress in yin-notes

Update yin-notes.md as loop progresses:

```markdown
# yin-notes ⟜ development loop progress

**current phase** ⟜ buildit
**status** ⟜ ghcid watching; 2 compile errors, fixing

**completed phases**
- ✓ lockit
- ✓ dependendit (5 bounds widened, approved)
- ✓ branchit

**upcoming phases**
- cleanit (next after fixes)
- reportit
- versionit
- gitit (PR+CI)
- verifyit
- publishit

**decisions made**
- Dependency bounds widening approved
- Version bump strategy: minor (new feature added)

**known issues**
- Deprecation warning in Network module; flagged for fixit

**synthesis status**
- exec-heavy phase (building/fixing)
- expect transition to balanced when cleanit runs
```

Update lightly; this is for context if yin is interrupted.

---

## Synthesis Integration

Tailie synthesis helps yin track overall progress:

```
[20:45:12] +8 lines | exec(3) struct(2) practice(1)
  ↑ exec domain high = buildit/cleanit phase active

[20:50:15] +5 lines | coord(2) struct(3) exec(1) practice(2)
  ↑ balanced = phase transitioning; operator decision time

[21:00:00] +12 lines | coord(4) struct(2) other(1)
  ↑ coord high = reviewit/gitit phase (decisions/review)
```

Yin reads these patterns and understands "where are we in the loop?"

---

## Using Loopit in Practice

When operator asks "what's next?":
1. Check yin-notes for current phase
2. Read synthesis to understand domain emphasis
3. Consult loopit diagram for next stage
4. Spin appropriate card or ask operator

When development feels stuck:
1. Check synthesis; pattern should be changing
2. Read recent flows-log entries
3. Identify which stage or loop point yin is at
4. Decide: continue current → advance → backtrack

---

## Notes

Loopit is not a card you call; it's a mental model. It's the structure that lets yin operate without getting lost or trying too many things at once.

The loop can be completed in one session or span many sessions. Context persists in yin-notes and flows-log.

