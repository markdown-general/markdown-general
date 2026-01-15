# card: branchit ⟜ feature branch creation and management

**type** ⟜ flow / git

**purpose** ⟜ create safe isolated space for development; track branch lifecycle

**effects** ⟜ reads: git state, writes: git branch (new), spawns: none, network: no

---

## Task Sequence

### When to Create a Branch

Branch when:
- Repository edits are planned (not just bounds widening, but source changes)
- Lockit has established scope
- Operator requests feature branch creation

Do NOT create a branch if only reading/analyzing (e.g., dependendit analysis).

### Branch Creation

**Input:** Scope name from lockit (e.g., "ghc-9.14-upgrade")

**Output:**
```
branchit-create | [repo] | [branch-name] | ✓
```

Example branch naming:
```
upgrade/ghc-9.14-numhask-space
release/v0.12.0-prepare
bounds/refresh-jan2025
```

**Action:**

Spin: action-git-branch-create

```bash
git checkout -b [feature-name]-[version]
```

Verify clean working tree before branching:
```bash
git status  # must be clean
```

If not clean:
```
branchit-create | [repo] | [branch-name] | ✗ | working-tree-not-clean
```

### Branch Verification

Log the branch info:
```
branchit-verify | [repo] | [branch-name] | [commit-hash] | on-feature-branch: ✓
```

---

## Lifecycle

Once created:
- All subsequent work happens on the feature branch
- Buildit watches the branch
- Fixit makes edits on the branch
- Cleanit runs on the branch
- When ready: gitit manages PR creation

### Switching Back (Temporary)

If operator needs to switch to main for any reason:
```
git checkout main
```

Always remember the feature branch name; document in yin-notes.

### Branch Cleanup

When development is complete and merged to main:

Spin: action-git-branch-delete

```bash
git checkout main
git branch -d [feature-branch]
```

Output:
```
branchit-cleanup | [repo] | [branch-name] | deleted
```

---

## Calls to Action Cards

- action-git-branch-create (on scope definition)
- action-git-branch-delete (after merge to main)

---

## Notes

Branching is optional. If work is truly minimal (e.g., just bounds in .cabal), operator may choose to skip. Document the decision in yin-notes.

