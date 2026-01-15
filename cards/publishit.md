# card: publishit ⟜ tagging, packaging, and Hackage publication

**type** ⟜ flow / release

**purpose** ⟜ create version tag, build source distribution, upload to Hackage

**effects** ⟜ reads: cabal file + source, writes: git tags + dist-newstyle/sdist/ + Hackage, spawns: none, network: yes (Hackage upload)

---

## Prerequisites

Before publishit:
- Verifyit passed (main is clean and builds)
- Version number agreed (from versionit)
- Hackage credentials configured (if auto-upload desired)

---

## Task Sequence

### Phase 1: Create Version Tag

Operator (manual or automated):

Spin: action-git-tag

```bash
git tag -a v[version] -m "Release v[version]: [summary]"
```

Verify tag created:
```bash
git tag -l | grep v[version]
```

Output:
```
publishit-tag | [repo] | v[version] | ✓ | on-commit: [hash]
```

### Phase 2: Build Source Distribution

```bash
cabal sdist
```

Output location:
```
dist-newstyle/sdist/[package]-[version].tar.gz
```

Log:
```
publishit-sdist | [repo] | [filename] | ✓ | size: [KB]
```

### Phase 3: Upload to Hackage

Option A: If credentials configured:
```bash
cabal upload dist-newstyle/sdist/[package].tar.gz
```

Option B: Manual upload (operator via Hackage web UI)

Output:
```
publishit-hackage-upload | [repo] | [✓/✗] | [hackage-url] or [manual]
```

### Phase 4: Verification

Check Hackage page:
```
[package] [version] on Hackage
```

Operator may download and spot-check:
```bash
cd /tmp
tar -xzf /path/to/downloaded.tar.gz
cd [package]-[version]
cabal build
```

Log verification:
```
publishit-verify | [repo] | ✓ | downloaded and built from Hackage
```

---

## Output

Flows-log accumulates:
```
publishit-tag | [repo] | v[version] | ✓
publishit-sdist | [repo] | [filename] | ✓
publishit-hackage-upload | [repo] | ✓ | https://hackage.haskell.org/package/[package]-[version]
publishit-verify | [repo] | ✓
```

When all complete:

```
publishit-complete | [repo] | released: [version] | live on Hackage
```

---

## Calls to Action Cards

- action-git-tag (create version tag)

---

## Notes

Publishit is the final step. Once complete:
- Version is live on Hackage
- Anyone can `cabal install [package]`
- Tag marks the release commit
- No edits to main after publishit until next development cycle

Publishit is typically run once per version. Re-running same version is not typical (would be re-release/patch).

