# card: action-git-branch-create ⟜ create feature branch

**type** ⟜ action / git

**execution** ⟜ bounded worker / 5s timeout

**input** ⟜ repo directory (must have clean working tree), branch name

**task** ⟜ git checkout -b [branch-name]; verify branch created and active

**output** ⟜ ✓/✗ + [branch-name] + [commit-hash]

**idempotence** ⟜ NOT idempotent if branch exists; check and error explicitly

**effects** ⟜ reads: git state, writes: git branch (new), spawns: none, network: no

---

## Pre-Check

Verify working tree is clean:
```bash
git status --porcelain  # must be empty
```

If dirty:
```
✗ Working tree not clean; cannot create branch
[list of dirty files]
```

---

## Creation

```bash
git checkout -b [branch-name]
RESULT=$?

if [ $RESULT -eq 0 ]; then
  COMMIT=$(git rev-parse HEAD)
  echo "✓ BRANCH CREATED: $branch-name"
  echo "Commit: $COMMIT"
else
  # Check if branch already exists
  if git rev-parse --verify [branch-name] > /dev/null 2>&1; then
    echo "✗ BRANCH ALREADY EXISTS: $branch-name"
  else
    echo "✗ BRANCH CREATION FAILED"
    echo "Return code: $RESULT"
  fi
fi
```

---

## Output Format

Success:
```
action-git-branch-create | [repo] | [branch-name] | ✓ | commit: [hash]
```

Error (branch exists):
```
action-git-branch-create | [repo] | [branch-name] | ✗ | already-exists
```

Error (working tree dirty):
```
action-git-branch-create | [repo] | [branch-name] | ✗ | working-tree-dirty
```

---

## Notes

Branch naming convention:
- `feature/[description]-[version]` (e.g., `feature/ghc-9.14-upgrade`)
- `release/[version]-prepare` (e.g., `release/v0.12.0-prepare`)
- `bounds/[refresh-scope]` (e.g., `bounds/refresh-jan2025`)

This action fails explicitly if branch exists; does not overwrite or checkout existing.

