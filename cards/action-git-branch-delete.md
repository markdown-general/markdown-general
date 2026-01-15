# card: action-git-branch-delete ⟜ delete feature branch

**type** ⟜ action / git

**execution** ⟜ bounded worker / 5s timeout

**input** ⟜ repo directory (must be on main or different branch), branch name to delete

**task** ⟜ git branch -d [branch-name]; verify deletion

**output** ⟜ ✓/✗ + [branch-name] + status

**idempotence** ⟜ safe to call if branch doesn't exist (returns already-gone)

**effects** ⟜ reads: git state, writes: git branch (deleted), spawns: none, network: no

---

## Pre-Check

Verify we're not on the branch being deleted:
```bash
CURRENT_BRANCH=$(git branch --show-current)
if [ "$CURRENT_BRANCH" = "[branch-name]" ]; then
  echo "✗ Cannot delete branch while on it; checkout main first"
  exit 1
fi
```

---

## Deletion

Attempt safe deletion (only deletes if fully merged):
```bash
git branch -d [branch-name]
RESULT=$?

if [ $RESULT -eq 0 ]; then
  echo "✓ BRANCH DELETED: $branch-name"
else
  # Check if branch exists at all
  if git rev-parse --verify [branch-name] > /dev/null 2>&1; then
    echo "✗ BRANCH NOT FULLY MERGED: $branch-name"
    echo "Use -D flag to force delete (not recommended)"
  else
    echo "✓ BRANCH ALREADY GONE: $branch-name"
  fi
fi
```

---

## Output Format

Success (branch deleted):
```
action-git-branch-delete | [repo] | [branch-name] | ✓ | deleted
```

Success (branch already gone):
```
action-git-branch-delete | [repo] | [branch-name] | ✓ | already-deleted
```

Error (not fully merged):
```
action-git-branch-delete | [repo] | [branch-name] | ✗ | not-fully-merged
```

Error (still on branch):
```
action-git-branch-delete | [repo] | [branch-name] | ✗ | currently-on-branch
```

---

## Notes

Uses `git branch -d` (safe) not `-D` (force). If branch is not fully merged, deletion fails; this is intentional protection against accidental loss.

If operator really needs to force-delete unmerged branch, that's a manual operator decision (not automated).

