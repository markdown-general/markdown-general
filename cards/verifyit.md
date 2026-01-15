# card: verifyit ⟜ merge verification and main branch validation

**type** ⟜ flow / integration + operator action

**purpose** ⟜ verify PR is merged, pull latest main, confirm clean state and compilation

**effects** ⟜ reads: git main branch + GitHub, writes: git pull + dist-newstyle/ + flows-log, spawns: none, network: yes (git pull)

---

## Prerequisites

Before verifyit:
- PR merged to main on GitHub (operator action, manual)
- All checks on PR passed
- Feature branch has been completed

---

## Task Sequence

### Phase 1: Ensure Clean Repo

Verify pristine state:

```bash
git status  # must show: "nothing to commit, working tree clean"
git branch  # identify current branch
```

Output:
```
verifyit-pristine | [repo] | git-status: clean | on-branch: [branch]
```

If not clean:
```
verifyit-pristine | [repo] | ✗ | dirty-files: [N] | blocked
```

### Phase 2: Switch to Main (Manual Operator Action)

Operator (not automated):
```bash
git checkout main
git pull origin main
```

Confirm pull succeeded:
```bash
git log -1 --oneline
```

Log to flows-log:
```
verifyit-checkout-main | [repo] | ✓ | at-commit: [hash]
```

### Phase 3: Verify Pristine Again

```bash
git status  # must be clean
git branch  # confirm on main
```

Output:
```
verifyit-main-pristine | [repo] | ✓ | clean, on main
```

### Phase 4: Build Verification

Spin: action-cabal-build

```bash
cabal clean && cabal build all 2>&1
```

Full verification build on main:

Output:
```
verifyit-build | [repo] | ✓/✗ | [error-count] [warning-count]
```

If build fails:
```
verifyit-build | [repo] | ✗ | errors found, blocked
```

Operator and yin investigate; may need to revert PR or fix on main.

---

## Success Condition

All phases complete with ✓:

```
verifyit-complete | [repo] | ✓ | ready for tag + publish
```

This signals to publishit that main is clean and ready.

---

## Failure Recovery

If any phase fails:

1. Operator and yin investigate
2. Fix on main (if needed)
3. Re-run verifyit

Verifyit is idempotent; running multiple times is safe.

---

## Calls to Action Cards

- action-cabal-build (final verification)

---

## Notes

Verifyit is the safety check before publishing. It ensures:
- PR was actually merged
- Main branch pulls cleanly
- No stale files or uncommitted changes
- Code compiles without error

This is a critical gate. If verifyit fails, publishit should not proceed.

