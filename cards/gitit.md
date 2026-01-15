# card: gitit ⟜ git maintenance and PR management

**type** ⟜ flow / git

**purpose** ⟜ manage feature branch lifecycle, create and monitor PRs, handle CI

**effects** ⟜ reads: git state + GitHub, writes: git push + GitHub PR, spawns: none, network: yes (GitHub API)

---

## Task Sequence

### Phase 1: Pre-PR Verification

Before creating PR:

**Verify pristine repo:**
```bash
git status  # must be clean
git log -1 --oneline  # confirm on feature branch
```

Output:
```
gitit-pristine | [repo] | clean | on-branch: [feature-branch]
```

### Phase 2: Push Feature Branch

Operator (manual):
```bash
git push origin [feature-branch]
```

Confirm push succeeded:
```bash
git branch -vv  # shows tracking status
```

Output:
```
gitit-push | [repo] | [branch] → origin | ✓
```

### Phase 3: Create PR

Operator (manual, via GitHub UI):
- Open PR from [feature-branch] → main
- Add description (what was changed, why)
- Reference related issues
- Set labels if applicable

Log to flows-log:
```
gitit-pr-created | [repo] | [branch] | PR-#[N] | [url]
```

### Phase 4: Monitor CI

GitHub Actions (or CI system) runs on PR:
- Expected: tests on multiple OS
- Likely: some OS-specific failures (first run)

Operator monitors via GitHub Checks:
```bash
# Or check GitHub UI for workflow runs
```

Log observations:
```
gitit-ci-running | [repo] | PR-#[N] | [OS-list] | [status]
```

Common first-run issues:
- macOS-specific GHC behavior
- Windows path handling
- Linux glibc version

### Phase 5: CI Results & Feedback

If CI fails:
```
gitit-ci-failed | [repo] | PR-#[N] | failures: [OS-list] | blocked
```

Operator logs issues in yin-notes. Fixit may be needed.

If CI passes:
```
gitit-ci-passed | [repo] | PR-#[N] | ✓ | ready for review
```

---

## Calls to Action Cards

None directly; gitit is primarily operator action with yin observation and logging.

---

## Critical Operator Responsibility

**Gitit creates and monitors PRs. Gitit does NOT merge.**

- PR creation is operator action (via GitHub UI)
- CI monitoring is yin observation + operator review
- Merge to main is verifyit step (manual operator action)

**Merge responsibility:** If main branch receives unwanted merges, that is operator responsibility. The operator approved the scope at lockit and chose to create this PR. Merging is a conscious decision point at verifyit.

## Notes

Gitit is the bridge to external CI. First PR run on a new repo often reveals OS-specific issues. This is expected and valuable feedback.

Document all CI failures and subsequent fixes in yin-notes. These become intel for future development cycles.

**Note on test vs production:** For test runs, operator may close PR without merging to preserve main. For production releases, operator consciously merges when ready. The distinction is operator choice at lockit, not a special "test mode" in gitit.

