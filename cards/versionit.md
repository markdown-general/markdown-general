# card: versionit ⟜ version strategy and changelog management

**type** ⟜ flow / versioning (operator-driven)

**purpose** ⟜ assess version bump needs, generate changelog entry, document release

**effects** ⟜ reads: git tags + git log + CHANGELOG.md, writes: flows-log + yin-notes (proposals only), spawns: none, network: no

---

## Task Sequence

### Phase 1: Check Git Tags

Identify last release:
```bash
git tag -l | sort -V | tail -5
```

Last version: [e.g., v0.11.3]

Output:
```
versionit-last-tag | [repo] | last: [version] | date: [timestamp]
```

### Phase 2: Assess Changes Since Last Release

```bash
git diff [last-version-tag]..HEAD --stat
```

Summarize:
- How many files changed?
- What types of changes? (bug fix, feature, refactor, docs)

Output:
```
versionit-changes | [repo] | [files-changed] | types: [list]
```

### Phase 3: Analyze Public API Surface

Operator decision: Did API change?

- **No API change, only bug fixes** → patch bump (0.11.3 → 0.11.4)
- **New functions/types, backward compatible** → minor bump (0.11.3 → 0.12.0)
- **Breaking API change** → major bump (0.11.3 → 1.0.0)

Log:
```
versionit-api-assessment | [repo] | api-changed: [Y/N] | recommendation: [major/minor/patch/hold]
```

### Phase 4: Read Changelog Pattern

```bash
cat CHANGELOG.md
head -30 CHANGELOG.md
```

Understand formatting and style:
- How are entries structured?
- Date format?
- Version format?
- What detail level?

### Phase 5: Generate Changelog Entry

From git log and synthesis, write entry:

```
## [0.12.0] - 2025-01-15

### Added
- Support for new feature X
- Additional bound support

### Fixed
- Bug in Y causing Z
- Issue with deprecated API

### Changed
- Updated dependencies to latest stable

### Notes
- See PR #123 for details
```

Log to flows-log:
```
versionit-changelog | [repo] | version: [version] | entry-generated | needs-review
```

### Phase 6: Operator Approval

Operator reads proposed changelog and version in yin-notes:
```
**version decision**
- new version: [version]
- reason: [major/minor/patch rationale]
- changelog: approved/needs-edits
```

---

## Calls to Action Cards

None directly; versionit is analysis and proposal.

---

## Notes

Versionit is typically run before publishit. Version strategy should follow semantic versioning conventions.

Changelog should be a readable record of what changed and why. Keep it honest; don't over-sell features or hide breaking changes.

