# card: dependendit ⟜ dependency analysis and bounds management

**type** ⟜ flow / analysis + operator review

**purpose** ⟜ understand current dependencies, identify outdated packages, propose bounds widening

**effects** ⟜ reads: *.cabal + hackage (remote), writes: flows-log + yin-notes (proposals only), spawns: none, network: yes

---

## Task Sequence

### Phase 1: Read Current Dependencies

Spin: action-read-cabal

- Extract all build-depends from .cabal file
- List with current version bounds
- Output:
  ```
  dependendit-read | [repo] | [N] dependencies | ready for analysis
  ```

### Phase 2: Check for Outdated Packages

Spin: action-cabal-update (ensure fresh index)

Then analyze:
```bash
cabal outdated
```

Output to flows-log:
```
dependendit-outdated | [repo] | [N] packages have newer versions | [needs-review]
```

### Phase 3: Detect Unused Dependencies

Spin custom cabal build with warnings:
```bash
cabal build --ghc-options=-Wunused-packages 2>&1
```

Capture unused warnings and report:
```
dependendit-unused | [repo] | [N] unused packages found | [list or needs-review]
```

### Phase 4: Propose Changes (Operator Review)

Analyze findings:
- Which packages should be updated?
- Which should be removed?
- Which bounds should be widened?

Log proposal to flows-log (this is a decision point):
```
dependendit-propose | [repo] | [N] bounds to widen | [M] packages to remove | needs-operator-review
```

Tailie picks this up; bloodhound may detect "review pattern".

Operator reads synthesis and logs decision in yin-notes:
```
**dependendit decisions**
- widen: [list]
- remove: [list]
- keep: [list]
```

### Phase 5: Apply Changes (If Approved)

Once operator approves:

Spin: action-edit-cabal (future card)

For each approved change:
- Update bound
- Remove unused package
- Verify .cabal syntax

Log each application:
```
dependendit-apply | [repo] | [package] | [old-bound] → [new-bound]
```

---

## Calls to Action Cards

- action-read-cabal (extract dependencies)
- action-cabal-update (ensure fresh index)

---

## Reusability

Dependendit can be run:
- At start of development cycle (lockit era)
- Between build phases (if synthesis suggests stale deps)
- Before release (ensure all bounds are justified)

Multiple runs are safe; results accumulate in flows-log, operator reviews proposals.

---

## Notes

Dependendit is analysis + decision + application. It's not automated; operator approves before bounds are modified. This ensures intentional dependency management.

The "unused packages" check can be noisy if pragmas suppress warnings. Document assumptions in flows-log.

