# haskell-development-cards

Cards for Haskell development workflows. These cards represent composable patterns discovered through first doing with an operator, then encoded for reuse.

---

## Card Organization

Cards are organized by function:

### Flow Cards
Cards that represent stages of development. These typically call action cards.

- **lockit.md** ⟜ establish ground truth; scope work, verify versions, set CI strategy
- **buildit.md** ⟜ permanent build watcher; filewatch with ghcid feedback loop
- **fixit.md** ⟜ error/warning response; agent-driven fixing based on ghcid output
- **branchit.md** ⟜ feature branch creation and management
- **versionit.md** ⟜ version strategy and changelog management
- **dependendit.md** ⟜ dependency analysis and bounds management
- **cleanit.md** ⟜ code formatting, testing, linting, quality checks
- **writeit.md** ⟜ documentation verification and updates
- **reportit.md** ⟜ synthesis and status reporting
- **gitit.md** ⟜ git maintenance and branch hygiene
- **loopit.md** ⟜ development loop representation and progress tracking
- **verifyit.md** ⟜ merge verification and main branch validation
- **publishit.md** ⟜ tagging, packaging, and Hackage publication

### Action Cards
Atomic operations called by flow cards or spinnable individually.

- **action-cabal-update.md** ⟜ cabal update; refresh hackage index
- **action-cabal-build.md** ⟜ cabal build all; full compilation
- **action-cabal-test.md** ⟜ cabal test all; run unit tests
- **action-cabal-clean.md** ⟜ cabal clean; remove build artifacts
- **action-ghcid-start.md** ⟜ start ghcid with cabal repl
- **action-ghcid-stop.md** ⟜ stop ghcid process
- **action-read-cabal.md** ⟜ extract metadata from .cabal file
- **action-hlint.md** ⟜ style suggestions via hlint
- **action-haddock.md** ⟜ documentation coverage check
- **action-ormolu.md** ⟜ format Haskell source files
- **action-cabal-gild.md** ⟜ format .cabal file
- **action-git-branch-create.md** ⟜ create feature branch
- **action-git-branch-delete.md** ⟜ delete feature branch
- **action-git-merge.md** ⟜ merge feature branch to main
- **action-git-tag.md** ⟜ create version tag

---

## How Cards Compose

Flow cards call action cards. For example:

- **cleanit** might call:
  - action-cabal-gild
  - action-ormolu
  - action-cabal-test
  - action-hlint
  - action-haddock

- **buildit** might call:
  - action-cabal-build (continuously, watching)
  - action-ghcid-start / action-ghcid-stop

- **dependendit** might call:
  - action-read-cabal
  - action-cabal-update
  - (then propose changes)

---

## Reusability & Idempotence

Multiple flow cards may call the same action card. Ensure action cards are idempotent:
- Running twice should produce the same result or safe no-op
- Document assumptions about state (what directory? what branch?)
- Clearly specify input and output

Examples:
- **action-cabal-update** is idempotent (running twice = fresh index both times)
- **action-cabal-build** is idempotent (building twice = same result, or cached)
- **action-git-branch-create** is NOT idempotent if branch exists (should check and skip or error explicitly)

---

## Calling Cards

Cards are spun via the yin cycle. Example flow in yin-self.md:

```bash
# Spin a flow card (which calls multiple actions internally)
echo "[$(date +%s)] spinning cleanit for repo X" >> ~/self/foreman/flows-log.md

# Spin an action card directly for adhoc work
echo "[$(date +%s)] adhoc: cabal update" >> ~/self/foreman/flows-log.md
```

When yin reads synthesis and decides the next action, it either:
1. Spins a flow card (which orchestrates several actions)
2. Spins a specific action card (for operator-requested tasks)
3. Calls a specialized card (like fixit) on-demand when synthesis reveals an error

---

## Development Steady State

The goal of using these cards is to achieve **development steady state**:
- Project compiles cleanly (or with known warnings)
- Tests pass
- Documentation is in progress (may have quirks; that's OK)
- Ready for CI, merge, or iteration
- Operator can spin up a build to check health anytime
- Rebuild is instant if source changes

This is not a rigid gate. It's a rhythm.

---

## Evolution

As new patterns emerge:
- Scout work (operator + yin) discovers the pattern
- Encode it as a new card
- Add it to the appropriate section above
- Update this README

Cards are meant to grow and evolve. Start minimal, expand based on observed needs.

