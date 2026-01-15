# haskell-upgrade-deck ⟜ full chain from environment prep to Hackage publication

**purpose** ⟜ complete card set for upgrading Haskell libraries to ghc-9.14; tests the yin system end-to-end with tailie synthesis and full observation

**structure** ⟜ sequential bounded workers + skip-step diagnostics; each card produces responses/ output for explicit decision points

---

## Phase 0: Environment Preparation ⟜ Bootstrap Infrastructure

### card: env-cabal-update

**type** ⟜ worker / bounded

**worker** ⟜ circuit / 30s

**task** ⟜ cabal update; verify index freshness

**output** ⟜ ✓/✗ + [timestamp] [index state]

**write-to** ⟜ ~/self/foreman/responses/worker-env-001-cabal-update.md

**notes** ⟜ must run first; all other cards depend on current hackage index

---

### card: env-ghcid-startup

**type** ⟜ skip-step / no tracking

**task** ⟜ start ghcid --command="cabal repl" --outputfile=ghcid.txt in target repo; log PID

**output** ⟜ append to flows-log: "env-ghcid-startup | [repo] | [PID] | [status]"

**notes** ⟜ run once per repo being upgraded; remains running through all phases

---

### card: env-cabal-project-verify

**type** ⟜ worker / bounded

**worker** ⟜ circuit / 10s

**input** ⟜ [repo directory]

**task** ⟜ check cabal.project; verify write-ghc-environment-files: always present; check for problematic global .ghc env files

**output** ⟜ ✓/✗ + [status] + [warnings if any]

**write-to** ⟜ ~/self/foreman/responses/worker-env-002-cabal-project.md

**notes** ⟜ warn if global GHC environment files detected; recommend --package-env .

---

## Phase 1: Feature Branch Preparation ⟜ Create Safe Upgrade Space

### card: branch-create-feature

**type** ⟜ worker / bounded

**worker** ⟜ circuit / 5s

**input** ⟜ [repo directory] [lib version]

**task** ⟜ git checkout -b upgrade/ghc-9.14-[lib-version]; verify branch created and clean

**output** ⟜ ✓/✗ + [branch name]

**write-to** ⟜ ~/self/foreman/responses/worker-branch-001-create.md

**do-not** ⟜ modify any files; report only

---

### card: bounds-read-cabal

**type** ⟜ worker / bounded

**worker** ⟜ circuit / 10s

**input** ⟜ [repo directory]

**task** ⟜ extract all version bounds from .cabal file; list build-depends with current range constraints

**output** ⟜ ✓ + [dependency] [current-lower] [current-upper] (one per line)

**write-to** ⟜ ~/self/foreman/responses/worker-bounds-001-read.md

**do-not** ⟜ modify files; report only

---

### card: bounds-check-outdated

**type** ⟜ worker / bounded

**worker** ⟜ circuit / 45s

**input** ⟜ [repo directory]

**task** ⟜ cabal outdated; report which dependencies have newer versions available on hackage

**output** ⟜ ✓ + [dependency] [installed] [latest-on-hackage] (one per line, or "all up-to-date" if none)

**write-to** ⟜ ~/self/foreman/responses/worker-bounds-002-outdated.md

**do-not** ⟜ modify files; report only

---

### card: bounds-propose-widening

**type** ⟜ skip-step / no tracking

**input** ⟜ responses/worker-bounds-001-read.md + responses/worker-bounds-002-outdated.md + ~/self/orders/repo-list.md

**task** ⟜ compare outdated report against repo-list.md versions; identify which bounds should widen for each dependency; generate proposal summary

**output** ⟜ append to flows-log: "bounds-propose | [repo] | [N] dependencies to widen | [needs-review]"

**notes** ⟜ human decision point; tailie will flag this for operator review

---

## Phase 2: Dependency Update ⟜ Apply and Test Changes

### card: bounds-update-cabal

**type** ⟜ worker / bounded

**worker** ⟜ circuit / 20s

**input** ⟜ [repo directory] [dependency] [new-lower] [new-upper]

**task** ⟜ sed or edit .cabal file; update single dependency bound; verify syntax

**output** ⟜ ✓/✗ + [dependency] [old-bound] [new-bound]

**write-to** ⟜ ~/self/foreman/responses/worker-bounds-003-update-[dep].md

**notes** ⟜ spun multiple times, once per dependency to update; idempotent (update same bound multiple times = same result)

---

### card: build-clean

**type** ⟜ worker / bounded

**worker** ⟜ circuit / 15s

**input** ⟜ [repo directory]

**task** ⟜ cabal clean; remove dist-newstyle; clear ghcid cache

**output** ⟜ ✓/✗ + [status]

**write-to** ⟜ ~/self/foreman/responses/worker-build-001-clean.md

**do-not** ⟜ modify source code; clean only

---

### card: build-update

**type** ⟜ worker / bounded

**worker** ⟜ circuit / 30s

**input** ⟜ [repo directory]

**task** ⟜ cabal update

**output** ⟜ ✓/✗ + [status]

**write-to** ⟜ ~/self/foreman/responses/worker-build-002-update.md

---

### card: build-configure

**type** ⟜ worker / bounded

**worker** ⟜ circuit / 20s

**input** ⟜ [repo directory]

**task** ⟜ cabal configure; check if dependencies resolve and compile plan is valid

**output** ⟜ ✓/✗ + [error count or "plan valid"]

**write-to** ⟜ ~/self/foreman/responses/worker-build-003-configure.md

**notes** ⟜ fails if bounds are still broken; informs next iteration

---

### card: build-compile

**type** ⟜ worker / bounded

**worker** ⟜ circuit / 120s (cabal build is slower)

**input** ⟜ [repo directory]

**task** ⟜ cabal build all; capture full output including warnings

**output** ⟜ ✓/✗ + [error count] [warning count] + [last 20 lines of output]

**write-to** ⟜ ~/self/foreman/responses/worker-build-004-compile.md

**capture** ⟜ save full output to /tmp/cabal-build-[repo].log for inspection

---

## Phase 3: Testing & Linting ⟜ Quality Assurance

### card: test-unit

**type** ⟜ worker / bounded

**worker** ⟜ circuit / 90s

**input** ⟜ [repo directory]

**task** ⟜ cabal test all 2>&1; run all unit tests

**output** ⟜ ✓/✗ + [test count] [pass count] [fail count]

**write-to** ⟜ ~/self/foreman/responses/worker-test-001-unit.md

**notes** ⟜ fails if any test fails; captures all output

---

### card: test-doctest

**type** ⟜ worker / bounded

**worker** ⟜ circuit / 60s

**input** ⟜ [repo directory]

**task** ⟜ cabal test --enable-tests doctest 2>&1 (if present); run doctests

**output** ⟜ ✓/✗ + [doctest count] [pass] [fail] (or "no doctests")

**write-to** ⟜ ~/self/foreman/responses/worker-test-002-doctest.md

---

### card: lint-hlint

**type** ⟜ worker / bounded

**worker** ⟜ circuit / 30s

**input** ⟜ [repo directory]

**task** ⟜ hlint src/ --hint=Default 2>&1; report style suggestions (warnings only, not failures)

**output** ⟜ ✓ + [suggestion count] [severity distribution]

**write-to** ⟜ ~/self/foreman/responses/worker-lint-001-hlint.md

**notes** ⟜ always succeeds (just reports suggestions); does not block progress

---

### card: lint-haddock

**type** ⟜ worker / bounded

**worker** ⟜ circuit / 30s

**input** ⟜ [repo directory]

**task** ⟜ haddock src/ --check 2>&1; verify documentation coverage

**output** ⟜ ✓/✗ + [warning count] [missing coverage]

**write-to** ⟜ ~/self/foreman/responses/worker-lint-002-haddock.md

---

## Phase 4: Synthesis & Validation ⟜ Operator Decision Point

### card: synthesis-build-status

**type** ⟜ skip-step / no tracking

**input** ⟜ responses/worker-build-004-compile.md + /tmp/cabal-build-*.log

**task** ⟜ summarize build results: did it compile cleanly? any warnings? compare against baseline

**output** ⟜ append to flows-log: "synthesis-build-status | [repo] | [status] | [warnings-count] | [decision-ready]"

---

### card: synthesis-test-status

**type** ⟜ skip-step / no tracking

**input** ⟜ responses/worker-test-*.md

**task** ⟜ summarize all test results: pass/fail ratio, which tests failed if any

**output** ⟜ append to flows-log: "synthesis-test-status | [repo] | [all-pass or N-failures] | [ready-for-merge]"

---

### card: synthesis-report-generate

**type** ⟜ skip-step / no tracking

**task** ⟜ compile full upgrade report: bounds changes, build status, test results, lint suggestions, ready/blocked status

**output** ⟜ write ~/self/foreman/responses/upgrade-report-[repo].md with formatted summary

**format** ⟜
```
# Upgrade Report: [repo] to ghc-9.14

## Bounds Changed
[list]

## Build Status
[✓/✗ + details]

## Test Results
[summary]

## Lint Results
[hlint + haddock summary]

## Decision
[READY FOR MERGE / BLOCKED: reason]
```

---

## Phase 5: Integration ⟜ Merge & Publish

### card: merge-verify-main

**type** ⟜ worker / bounded

**worker** ⟜ circuit / 5s

**input** ⟜ [repo directory]

**task** ⟜ verify main branch exists and is reachable; fetch latest main

**output** ⟜ ✓/✗ + [main commit hash]

**write-to** ⟜ ~/self/foreman/responses/worker-merge-001-verify-main.md

**do-not** ⟜ modify; report only

---

### card: merge-check-conflicts

**type** ⟜ worker / bounded

**worker** ⟜ circuit / 15s

**input** ⟜ [repo directory] [feature branch]

**task** ⟜ git merge-base main [feature]; check if merge would have conflicts

**output** ⟜ ✓/✗ + [conflict-free or "N conflicts: list files"]

**write-to** ⟜ ~/self/foreman/responses/worker-merge-002-check-conflicts.md

---

### card: merge-execute

**type** ⟜ worker / bounded

**worker** ⟜ circuit / 10s

**input** ⟜ [repo directory] [feature branch]

**task** ⟜ git checkout main && git merge [feature] --no-ff -m "Upgrade to ghc-9.14: [summary]"

**output** ⟜ ✓/✗ + [merge commit hash or conflict description]

**write-to** ⟜ ~/self/foreman/responses/worker-merge-003-execute.md

**notes** ⟜ should only run if conflicts check passed

---

### card: verify-main-compiles

**type** ⟜ worker / bounded

**worker** ⟜ circuit / 120s

**input** ⟜ [repo directory] (now on main)

**task** ⟜ cabal clean && cabal build all; verify main branch compiles after merge

**output** ⟜ ✓/✗ + [error count] [warning count]

**write-to** ⟜ ~/self/foreman/responses/worker-verify-001-main-compile.md

**notes** ⟜ safety check; if fails, indicates merge issue

---

### card: tag-version

**type** ⟜ worker / bounded

**worker** ⟜ circuit / 5s

**input** ⟜ [repo directory] [new version number]

**task** ⟜ git tag -a v[new-version] -m "Release ghc-9.14 upgrade"; verify tag created

**output** ⟜ ✓/✗ + [tag name] + [commit]

**write-to** ⟜ ~/self/foreman/responses/worker-publish-001-tag.md

---

### card: publish-hackage

**type** ⟜ worker / bounded

**worker** ⟜ circuit / 45s

**input** ⟜ [repo directory]

**task** ⟜ cabal sdist && cabal upload dist-newstyle/sdist/[package].tar.gz (if credentials configured)

**output** ⟜ ✓/✗ + [hackage URL or "requires manual upload"]

**write-to** ⟜ ~/self/foreman/responses/worker-publish-002-hackage.md

**notes** ⟜ may require operator intervention (hackage credentials, manual confirmation)

---

## Phase 6: Cleanup & Documentation ⟜ Close Loop

### card: branch-cleanup

**type** ⟜ worker / bounded

**worker** ⟜ circuit / 5s

**input** ⟜ [repo directory] [feature branch name]

**task** ⟜ git checkout main && git branch -d [feature]; verify branch deleted

**output** ⟜ ✓/✗ + [status]

**write-to** ⟜ ~/self/foreman/responses/worker-cleanup-001-branch-delete.md

**notes** ⟜ run only after merge and verification complete

---

### card: doc-update-changelog

**type** ⟜ skip-step / no tracking

**task** ⟜ append to CHANGELOG.md or HISTORY.md: upgraded to ghc-9.14, listed dependencies changed, current date

**output** ⟜ append to flows-log: "doc-update-changelog | [repo] | ✓"

---

### card: final-status-report

**type** ⟜ skip-step / no tracking

**task** ⟜ read all worker responses and final state; generate completion summary for operator

**output** ⟜ write ~/self/foreman/responses/final-status-[repo].md with:
- upgrade date
- dependencies changed (count + list)
- build: passed
- tests: passed
- published: ✓ [hackage URL] or ○ [reason]
- ready for next repo

---

## Execution Model ⟜ How Yin Spins This Deck

**Phase 0-1**: Sequential bounded workers (must complete in order; each waits for previous)

```
env-cabal-update → env-cabal-project-verify → branch-create-feature
         ↓
   branch waits for env-cabal-project-verify to pass
```

**Phase 1-2**: Parallel bounds reading; then sequential updates

```
bounds-read-cabal ──┐
bounds-check-outdated ├→ bounds-propose-widening (skip-step, waits for operator review in flows-log)
                    │
    [operator reads synthesis, reviews proposal]
                    │
              ↓ bounds-propose-widening logged to flows-log
              tailie picks up; bloodhound triggers review pattern
```

**Phase 2**: Sequential compile pipeline

```
build-clean → build-update → build-configure → build-compile
              (each waits for previous ✓)
```

**Phase 3**: Parallel tests + lints

```
build-compile ✓
     ↓
test-unit ──────────┐
test-doctest ───────├→ [all complete, move to synthesis]
lint-hlint ─────────┤
lint-haddock ───────┘
```

**Phase 4**: Skip-step synthesis accumulation

```
synthesis-build-status ──┐
synthesis-test-status ───├→ flows-log accumulates
synthesis-report-generate ┘    tailie synthesizes
                           ↓
                    operator reads report
                    decides: merge or iterate
```

**Phase 5**: Conditional merge pipeline

```
[operator approves in yin-notes or responds to patterns]
     ↓
merge-verify-main → merge-check-conflicts → merge-execute (if no conflicts)
                         ↓ [conflicts detected]
                    [blocked; operator decides next]
```

---

## Full Card Dependency Graph

```
env-cabal-update (must pass)
    ↓
env-cabal-project-verify (must pass)
    ↓
env-ghcid-startup (skip-step, parallel to branch creation)
    ↓
branch-create-feature (must pass)
    ├→ bounds-read-cabal ──────┐
    ├→ bounds-check-outdated ──┤→ bounds-propose-widening (skip-step)
    │                         │    ↓
    │                    [operator reviews]
    │                         │
    └→ [waits for operator decision]
         ↓
    bounds-update-cabal (spun N times for N dependencies)
         ↓
    build-clean → build-update → build-configure → build-compile
         ↓ [all pass]
    test-unit ──────┐
    test-doctest ───┤
    lint-hlint ─────┤→ synthesis-build-status ──┐
    lint-haddock ───┤   synthesis-test-status ──┤→ synthesis-report-generate
                    │   [parallel skip-steps]   │    ↓
                    └───────────────────────────┘ [operator reviews report]
                                                      ↓
                                    [operator approves merge]
                                                      ↓
                    merge-verify-main → merge-check-conflicts → merge-execute
                                                      ↓
                                    verify-main-compiles (safety check)
                                                      ↓
                                    tag-version → publish-hackage
                                                      ↓
                                    branch-cleanup → doc-update-changelog
                                                      ↓
                                    final-status-report
```

---

## Integration with Tailie/Yin System

**Skip-step cards log to flows-log.md:**
- bounds-propose-widening
- synthesis-build-status
- synthesis-test-status
- synthesis-report-generate
- doc-update-changelog
- final-status-report

**Tailie picks these up every 3s and synthesizes:**
```
[20:45:12] +8 lines | exec(3) struct(2) practice(1) other(2)
[20:45:15] +12 lines | exec(3) struct(3) practice(2) other(4)
```

**Bloodhound detects patterns:**
```
[20:45:18] exec-heavy | e=3 s=3 p=2 (building/testing phase active)
[20:46:00] balanced | e=2 s=3 p=3 (completed; synthesis showing)
```

**Yin reads synthesis and decides next action:**
- If "exec-heavy stable" → continue watching
- If pattern shifts to "balanced" + synthesis shows blocks → read report, make merge decision
- If report ready → make decision; if merge approved → spin merge pipeline

---

## Notes for Full Test

This deck tests:
✓ Sequential bounded workers (dependency chaining)
✓ Parallel bounded workers (tests + lints)
✓ Skip-step accumulation (synthesis cards)
✓ Tailie synthesis in background (executor doesn't block on synthesis)
✓ Operator decision points (flows-log review patterns)
✓ Conditional branching (merge only if tests pass)
✓ Full end-to-end: environment prep → upgrade → test → merge → publish

**Success criteria:**
- All workers complete with ✓ or explicit ✗ (not timeout)
- Flows-log accumulates all skip-step results
- Tailie synthesis updates every 3-5s with domain counts
- Bloodhound detects phase transitions (exec-heavy → balanced)
- Operator can read synthesis and make informed decisions
- Full upgrade chain completes from env-setup to final-status-report
