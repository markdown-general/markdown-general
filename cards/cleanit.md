# card: cleanit ⟜ code formatting, testing, linting, quality checks

**type** ⟜ flow / quality assurance

**purpose** ⟜ standardize formatting, run comprehensive tests, check style and documentation

**effects** ⟜ reads: *.hs + *.cabal, writes: *.hs + *.cabal + dist-newstyle/, spawns: none, network: no

---

## Task Sequence

### Phase 1: Format .cabal File

Spin: action-cabal-gild

```bash
cabal-gild -m check --input=[package].cabal
cabal-gild --io=[package].cabal
```

Output:
```
cleanit-cabal-format | [repo] | ✓/✗ | changes: [Y/N]
```

### Phase 2: Format Haskell Source

Spin: action-ormolu

```bash
ormolu --mode inplace $(git ls-files '*.hs')
```

Output:
```
cleanit-ormolu | [repo] | ✓/✗ | files-formatted: [N]
```

### Phase 3: Run Unit Tests

Spin: action-cabal-test

```bash
cabal test all 2>&1
```

Output:
```
cleanit-test-unit | [repo] | [pass-count]/[total-count] | [✓/✗]
```

If failures:
```
cleanit-test-unit | [repo] | [fail-count] failures | blocked
```

### Phase 4: Run Documentation Tests

Spin: action-cabal-docspec (if present)

```bash
cabal-docspec 2>&1
```

Output:
```
cleanit-docspec | [repo] | [pass-count]/[total-count] | [✓/✗] (or: no doctests)
```

### Phase 5: Style Check

Spin: action-hlint

```bash
hlint src/ --hint=Default 2>&1
```

Output (always succeeds, just reports):
```
cleanit-hlint | [repo] | [suggestion-count] suggestions | [severity distribution]
```

### Phase 6: Documentation Coverage

Spin: action-haddock

```bash
haddock --check 2>&1
```

Output:
```
cleanit-haddock | [repo] | [coverage-percent]% | [warning-count] warnings | [✓/✗]
```

---

## Synthesis & Decision

After all cleanit phases complete:

Accumulate to flows-log. Tailie synthesizes:
```
cleanit-complete | [repo] | [tests: ✓/✗] [hlint: N suggestions] [haddock: ✓/✗]
```

**If all pass:** Repo is in clean state; ready for fixit/branchit/writeit work.

**If tests fail:** Blocked until fixed. Operator reviews failures and decides:
- Fixit spins to address test issues
- Or: decide to accept failures as known issues

**If doc coverage low:** Flag for writeit work.

---

## Idempotence

All cleanit actions are idempotent:
- Running ormolu twice = same formatting
- Running cabal test twice = same results
- Running hlint twice = same suggestions

Safe to run multiple times. Format changes accumulate naturally.

---

## Flexibility

Cleanit is a flexible grouping. If new tools are added (e.g., weeder, stan):

1. Create action-card for the tool
2. Add phase to cleanit
3. Integrate into synthesis

Example: adding weeder for unused imports:
```bash
# New phase after ormolu
weeder . 2>&1
```

---

## Calls to Action Cards

- action-cabal-gild (format .cabal)
- action-ormolu (format Haskell)
- action-cabal-test (run tests)
- action-cabal-docspec (doc tests)
- action-hlint (style check)
- action-haddock (doc coverage)

---

## Notes

Cleanit can run anytime:
- After dependendit (once bounds are updated)
- Before publishit (ensure quality gate)
- On-demand (operator: "spin cleanit")

It's also one of the first cards to run in a development cycle to establish baseline quality state.

