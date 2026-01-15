# card: action-git-merge ⟜ merge feature branch to main

**type** ⟜ action / git

**execution** ⟜ bounded worker / 10s timeout

**input** ⟜ repo directory (must be on main), feature branch name, merge commit message

**task** ⟜ git merge [branch] --no-ff -m "[message]"; verify merge completed

**output** ⟜ ✓/✗ + [merge-commit-hash] or [conflict-description]

**idempotence** ⟜ NOT idempotent; only run once per merge. Check merge status first.

**effects** ⟜ reads: feature branch commits, writes: git main branch history, spawns: none, network: no

---

## Pre-Checks

**Verify we're on main:**
```bash
CURRENT=$(git branch --show-current)
if [ "$CURRENT" != "main" ]; then
  echo "✗ Not on main branch; currently on: $CURRENT"
  echo "Switch to main first: git checkout main"
  exit 1
fi
```

**Verify working tree is clean:**
```bash
git status --porcelain  # must be empty
if [ -n "$(git status --porcelain)" ]; then
  echo "✗ Working tree not clean; cannot merge"
  exit 1
fi
```

**Verify branch exists:**
```bash
if ! git rev-parse --verify [branch] > /dev/null 2>&1; then
  echo "✗ Branch does not exist: [branch]"
  exit 1
fi
```

---

## Merge Execution

```bash
# Perform merge with --no-ff to create merge commit
git merge [branch] --no-ff -m "[message]"
RESULT=$?

if [ $RESULT -eq 0 ]; then
  COMMIT=$(git rev-parse HEAD)
  echo "✓ MERGE SUCCESSFUL"
  echo "Merge commit: $COMMIT"
  echo "Merged branch: [branch]"
else
  # Check if merge was halted due to conflicts
  if git status | grep -q "both"; then
    echo "✗ MERGE CONFLICTS DETECTED"
    git status
    git merge --abort
    echo "Merge aborted; conflicts must be resolved manually"
  else
    echo "✗ MERGE FAILED"
    echo "Return code: $RESULT"
    git merge --abort
  fi
fi
```

---

## Output Format

Success:
```
action-git-merge | [repo] | [branch] → main | ✓ | commit: [hash]
```

Error (conflicts):
```
action-git-merge | [repo] | [branch] → main | ✗ | conflicts | abort
```

Error (not on main):
```
action-git-merge | [repo] | [branch] | ✗ | not-on-main | current: [branch]
```

Error (working tree dirty):
```
action-git-merge | [repo] | [branch] | ✗ | working-tree-dirty
```

---

## Message Format

Recommended merge commit message:
```
Merge [feature-branch] into main

- summary of changes
- key decision or feature
- issue reference (if applicable)
```

Example:
```
Merge feature/ghc-9.14-upgrade into main

- Updated bounds for GHC 9.14 compatibility
- All tests passing on macOS, Linux, Windows
- Fixes #42
```

---

## Notes

- **Not idempotent:** This is a one-time operation per release
- Uses `--no-ff` (no fast-forward) to create merge commit for history clarity
- Aborts automatically if conflicts detected (requires manual resolution)
- Only runs on main branch for safety
- Used by: gitit (after PR approved and CI passes), verifyit (confirmation)

If conflicts occur, operator must:
1. Manually resolve in editor
2. `git add [files]`
3. `git commit` to complete merge
4. Re-run action to verify merge state

