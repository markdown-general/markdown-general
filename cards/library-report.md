# card: library-report ⟜ comprehensive library health assessment

**type** ⟜ analysis / reporting

**purpose** ⟜ generate detailed health report on library: dependencies, tests, docs, bounds, CI status

**scope** ⟜ on-demand or post-release documentation

---

## Report Sections

Generated report should include:

### 1. Package Metadata

```
Library: [name]
Version: [version]
License: [license]
Repository: [url]
Hackage: https://hackage.haskell.org/package/[package]-[version]
```

### 2. Tested-with & CI Status

```
Tested Against:
  - GHC 9.10.2 ✓
  - GHC 9.12.2 ✓
  - GHC 9.14.1 ✓

CI Platforms:
  - ubuntu-latest: ✓
  - macos-latest: ✓
  - windows-latest: ✓

CI Status (latest run): [passing/failing]
Last CI run: [date/time]
```

### 3. Dependency Health

```
Build Dependencies: [count]
  - Direct: [count]
  - Transitive: [count]
  - Outdated: [count] (list if any)

Dependency Bounds:
  - Specified correctly: [%]
  - Conservative (<): [N] packages
  - Permissive (any): [N] packages (⚠)

Unused Dependencies: [none/list if found]
```

### 4. Code Quality

```
Source Code:
  - Files: [count]
  - Lines of code: [estimate]
  - Haddock coverage: [%]

Formatting:
  - ormolu status: [passing/needs-fix]
  - hlint suggestions: [count] (severity breakdown)

Tests:
  - Unit tests: [count] (pass/fail)
  - Doc tests: [count] (pass/fail)
  - Coverage: [if available]
```

### 5. Documentation

```
Readme: [present/missing] [size]
Changelog: [present/missing] [lines]
Haddock docs: [%] coverage
Extra doc files: [count] [list]
Github Wiki: [present/missing]
```

### 6. Release Readiness

```
✓ Tests passing
✓ Bounds specified
✓ Haddock coverage >80%
✓ Changelog updated
✓ Version bumped
✓ CI green on all platforms
```

---

## Generation

### Automated Collection

```bash
# Package metadata
NAME=$(grep "^name:" *.cabal | awk '{print $2}')
VERSION=$(grep "^version:" *.cabal | awk '{print $2}')

# Tested-with
grep "^tested-with:" *.cabal

# CI status (query GitHub Actions API)
gh run list --json status --limit 1

# Dependency count
grep -c "^    [a-z]" *.cabal

# Haddock coverage
cabal haddock --enable-documentation 2>&1 | grep -i coverage

# Hlint
hlint src/ | wc -l

# Test results (from latest run or local run)
cabal test 2>&1 | tail -20
```

### Output Format

**Recommended:** Generate as markdown for easy embedding in GitHub releases.

```markdown
# [Package] [Version] - Library Report

Generated: [date]

## Status

✓ Production ready

## Tested Against

- GHC 9.10.2 (via CI)
- GHC 9.12.2 (via CI)
- GHC 9.14.1 (via CI)

## Dependencies

[...details...]

## Documentation

Haddock coverage: 92%
Readme: Present
Changelog: Up to date

## Quality

All tests passing.
No hlint violations.
CI green on all platforms.
```

---

## When to Generate

1. **Before release:** Verify all signals green
2. **After major version bump:** Document state at release boundary
3. **During troubleshooting:** Diagnose dependency or compatibility issues
4. **For team communication:** Share library health status

---

## Manual Checklist (Operator-Friendly)

Use this when automated report generation not available:

```
[ ] Metadata (name, version, license, repo URL)
[ ] Tested-with versions match CI matrix
[ ] CI platform matrix complete (ubuntu, macos, windows)
[ ] Latest CI run: green on all platforms
[ ] No outdated dependencies (or documented reason)
[ ] Unused packages: none
[ ] Haddock coverage: >80%
[ ] Tests: all passing
[ ] Changelog: recent entry
[ ] Readme: accurate and up-to-date
```

---

## Integration with Development Cycle

- **lockit:** Baseline report generated (shows starting state)
- **versionit:** Dependency section checked for bounds issues
- **publishit:** Final report generated before release
- **verifyit:** CI section verified (all platforms passed)

---

## Notes

Library reports serve as:
- **Release notes companion:** What changed? How healthy is the release?
- **Debugging aid:** When issues arise, report shows dependencies at snapshot
- **Team communication:** Share status with collaborators
- **User confidence:** Hackage maintainers can see CI/test/doc quality

---

**This card will likely be automated further. Currently, it's a structured checklist for manual generation or CI integration.**
