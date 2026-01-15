# card: fixit ⟜ error and warning response (agent-driven, learning-based)

**type** ⟜ flow / on-demand

**purpose** ⟜ respond to errors/warnings from build; fix issues; observe patterns for future learning

**effects** ⟜ reads: error/warning output + ghcid.txt, writes: *.hs (fixes) + flows-log + yin-notes, spawns: none, network: no

---

## Activation

Fixit is called when:
- Buildit reports compile error in ghcid.txt
- Cleanit test fails
- Synthesis indicates "blocked" status
- Operator observes error and requests fix

**No pre-baked error cards.** Each error is unique (at first). Agent solves, documents, pattern emerges.

---

## Workflow

### Phase 1: Diagnose

Agent (operator or yin acting as scout):
- Read ghcid.txt (or error output)
- Understand what's broken:
  - Compile error? Import issue? Type mismatch? Missing pragma?
  - Test failure? Which test? What's the assertion?
  - Deprecation warning? API change needed?

Log observation:
```
fixit-diagnose | [repo] | [error-type] | [brief-description]
```

### Phase 2: Understand Scope

- Is it a one-line fix?
- Does it require reading external docs (Haddock, Hackage)?
- Is it a dependency bound issue?
- Is it a pragma/language extension?

Log:
```
fixit-scope | [repo] | [error-type] | scope: [one-liner/moderate/complex]
```

### Phase 3: Research (If Needed)

Use available resources:
- hoogle lookup
- Hackage documentation
- GHC error messages
- Codebase search (grep for similar patterns)

Log:
```
fixit-research | [repo] | [search-terms] | [findings]
```

### Phase 4: Apply Minimal Fix

Make targeted change:
- Edit .cabal (bounds, pragma)
- Edit source (type fix, import, function call)
- Add language pragma if needed

Keep it minimal. One fix at a time.

Log:
```
fixit-fix | [repo] | [file] | [line-range] | change: [brief-description]
```

### Phase 5: Observe Feedback

Trigger rebuild (ghcid recompiles, 2-5s):
```bash
tail -f ghcid.txt
```

Check result:
- Did error move? (often indicates partial fix)
- Disappeared entirely? (fixed!)
- New error appeared? (side effect, tackle next)

Log:
```
fixit-observe | [repo] | result: [error-gone/moved/new-error] | [status]
```

### Phase 6: Repeat or Declare Success

If error resolved:
```
fixit-success | [repo] | [error-type] | fixed | [minimal-description]
```

If error moved or new error appeared:
```
fixit-continue | [repo] | next-error: [type] | [brief-description]
```

Loop back to Phase 1.

---

## Learning & Pattern Capture

As fixit resolves categories of errors, patterns emerge:

**Example patterns to watch for:**
- "Bounds issue": Specific version constraint needed
- "Missing pragma": `{-# LANGUAGE Xxx #-}` solves it
- "Deprecated API": Migration path known
- "Documentation mismatch": Haddock doesn't match code

Once a pattern is clear (same error recurs):
- Yin (scout) and operator discuss
- Encode pattern as a reference card
- Next time: faster resolution

**Log pattern discovery:**
```
fixit-pattern-found | [repo] | [error-type] | pattern: [description] | recommend-doc-card
```

---

## Calls to Action Cards

Fixit may call action cards as needed:
- action-cabal-update (if bounds issue)
- action-cabal-build (full rebuild, not just ghcid)
- action-read-cabal (extract current state)

But often fixit just edits and observes ghcid feedback.

---

## Notes

Fixit is where learning happens. Each error teaches yin something. Patterns emerge over time and become future cards.

Fixit is not a rigid checklist. It's a framework for responding to unexpected issues. The agent (human or future yin) diagnoses, researches, and learns.

Avoid:
- Panic fixes (multiple changes at once)
- Over-engineered solutions
- Addressing future hypothetical issues

Do:
- Minimal, targeted fixes
- Quick feedback observation
- Pattern recognition
- Documentation of learning

---

## Example Session

```
Buildit reports: ghcid shows "Could not find module Network.HTTP"
  ↓
Fixit diagnoses: Missing import or dependency
  ↓
Fixit research: Check if http-types is in dependencies
  ↓
Result: http-types is listed but http library is not
  ↓
Fixit fix: Add http to build-depends, update bounds
  ↓
Observe: ghcid recompiles, error gone
  ↓
Fixit success: Fixed missing dependency
  ↓
Yin note: "common pattern: missing transitive dependencies; watch imports"
```

