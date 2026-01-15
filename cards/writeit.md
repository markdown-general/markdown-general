# card: writeit ⟜ documentation verification and updates

**type** ⟜ flow / documentation

**purpose** ⟜ verify README accuracy, check documented files, assess doc quality

**effects** ⟜ reads: README.md + *.cabal, writes: flows-log + yin-notes, spawns: none, network: no

---

## Task Sequence

### Phase 1: Review README

**Read README.md:**
```bash
cat README.md
```

Check:
- Accuracy of project description
- Build instructions still valid
- Examples work
- Links not broken

If outdated:
```
writeit-readme | [repo] | needs-update | [specific-issues]
```

Update if needed:
```
writeit-readme | [repo] | ✓ | updated
```

### Phase 2: Extract Documented Files

From .cabal file:
```bash
grep "extra-doc-files:" [package].cabal
```

List of documented files (e.g., CHANGELOG.md, CONTRIBUTING.md, etc.)

### Phase 3: Verify Each Doc File

For each file:
```bash
ls -la [file]  # exists?
head -20 [file]  # readable?
```

Check quality:
- Is it up-to-date?
- Does it cover current state?
- Any TODO markers?

Output:
```
writeit-docfiles | [repo] | [N] files checked | [M] issues found
```

### Phase 4: Review Haddock Documentation

From cleanit output, get haddock coverage %.

If < 80%:
```
writeit-haddock-coverage | [repo] | [%]% | flag for improvement
```

If >= 80%:
```
writeit-haddock-coverage | [repo] | [%]% | ✓
```

### Phase 5: Propose Documentation Work

Summarize findings:

```
writeit-propose | [repo] | README: [✓/issues] | docs: [count] | coverage: [%] | needs-review
```

Operator reviews findings in flows-log and decides:
- Update README now
- Create issue for doc work
- Accept current state

---

## Calls to Action Cards

None directly; writeit is inspection and proposal.

---

## Notes

Writeit is often run after cleanit. Documentation can lag code; that's acceptable if tracked and prioritized.

Haddock coverage at 80%+ is a good target. Lower is OK if operator accepts it explicitly.

