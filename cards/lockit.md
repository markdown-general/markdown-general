# card: lockit ⟜ establish ground truth and development scope

**type** ⟜ flow / setup + operator review

**purpose** ⟜ define the work scope, verify environment, establish CI strategy, set version policy

**effects** ⟜ reads: cabal.project + ghcup env, writes: flows-log + yin-notes, spawns: none, network: yes (calls cabal-update)

---

## Task Sequence

### 1. Scope Definition (Operator + Yin Scout)

**work scope** ⟜ name, type, and intended outcome

Examples:
- "ghc-9.14-upgrade" ⟜ upgrade all deps to 9.14 compatible
- "bounds-refresh" ⟜ widen version constraints for newer stable releases
- "release-prep" ⟜ prepare current state for publication

Document clearly in yin-notes.md:
```
**feature scope**
- name: [scope name]
- description: [what are we achieving?]
- expected changes: [bound updates? pragma adjustments? API changes?]
- decision points: [where does operator need to review?]
```

### 2. Environment Verification

**ghcup** ⟜ verify GHC, cabal, HLS versions

```bash
ghcup list -c set -r
```

Expected state must be agreed with operator before proceeding:
- GHC version (e.g., 9.14.1)
- Cabal version (e.g., 3.14.0.0)
- HLS version (if used)

**hackage index** ⟜ fresh cabal index

Spin: action-cabal-update

### 3. Project Configuration Check

**cabal.project** ⟜ extract and verify key settings

```bash
# Check for write-ghc-environment-files
grep "write-ghc-environment-files" cabal.project

# Check for allow-newer stanzas
grep "allow-newer" cabal.project

# Check for problematic global .ghc settings
ls ~/.ghc/environments/ 2>/dev/null
```

Document findings in flows-log:
```
lockit-cabal-project-check | [repo] | write-ghc: [yes/no] | allow-newer: [yes/no] | global-env-files: [count]
```

### 4. allow-newer Strategy (Operator Decision)

If `allow-newer` stanzas present:

```
lockit-allow-newer-review | [repo] | [current stanzas] | operator-decision: [keep/remove/modify]
```

Log operator's decision in yin-notes.md:
```
**allow-newer strategy**
- current state: [describe]
- operator decision: [keep/remove/modify]
- reason: [why?]
```

### 5. Version Strategy (Operator Decision)

Determine versioning approach for this development cycle:

```
lockit-version-strategy | [repo] | last-release: [version] | next-strategy: [major/minor/patch/hold]
```

### 6. CI Strategy (Operator Decision, Scout-Level)

Determine CI approach. May be:
- Copy from reference repo (e.g., numhask-space)
- Modify existing CI config
- Create new CI workflow

Document:
```
lockit-ci-strategy | [repo] | strategy: [copy/modify/create] | reference: [if applicable]
```

---

## Output

Flows-log accumulates:
- lockit-scope-definition
- lockit-env-verified
- lockit-cabal-project-check
- lockit-allow-newer-review (if applicable)
- lockit-version-strategy
- lockit-ci-strategy

All recorded with operator decisions and context.

---

## Idempotence & Reuse

Lockit is typically run once per development scope. Re-running should:
- Verify all assumptions still hold
- Check for environment changes
- Confirm operator decisions are still valid

Not destructive; purely informational and review-based.

---

## Calls to Action Cards

- action-cabal-update (verify index freshness)

---

## Next Step

Once lockit is complete and synthesis shows it in flows-log, buildit can start watching for changes.

