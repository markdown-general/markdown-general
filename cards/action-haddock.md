# card: action-haddock ⟜ documentation coverage check

**type** ⟜ action / linting

**execution** ⟜ bounded worker / 30s timeout

**input** ⟜ repo directory (must have src/ or lib/ with .hs files)

**task** ⟜ haddock --check src/ 2>&1; verify documentation coverage

**output** ⟜ ✓/✗ + [coverage-%] + [warning-count]

**idempotence** ⟜ fully idempotent (read-only analysis)

**effects** ⟜ reads: src/*.hs + cabal file, writes: ~/repos/[repo]/dist-newstyle/, spawns: none, network: no

---

## Pre-Check

Verify haddock is available:
```bash
which haddock > /dev/null 2>&1
if [ $? -ne 0 ]; then
  echo "✗ haddock not found in PATH"
  echo "Install with: cabal install haddock"
  exit 1
fi
```

Identify source directory:
```bash
if [ -d "src" ]; then
  SOURCE_DIR="src"
elif [ -d "lib" ]; then
  SOURCE_DIR="lib"
else
  echo "✗ No src/ or lib/ directory"
  exit 1
fi
```

---

## Run Documentation Check

```bash
timeout 30 haddock --check "$SOURCE_DIR" 2>&1 | tee /tmp/haddock-[repo].log
CHECK_RESULT=$?

# Extract coverage data
COVERAGE=$(grep -oP 'Documentation coverage: \K[\d.]+' /tmp/haddock-[repo].log || echo "0")
MISSING=$(grep -c "Warning:" /tmp/haddock-[repo].log || echo "0")

if [ $CHECK_RESULT -eq 0 ]; then
  echo "✓ Documentation check passed"
  echo "Coverage: ${COVERAGE}%"
  echo "Warnings: $MISSING"
else
  echo "⚠ Documentation warnings detected"
  echo "Coverage: ${COVERAGE}%"
  echo "Warnings: $MISSING"

  # Show sample warnings
  echo ""
  echo "Sample warnings:"
  grep "Warning:" /tmp/haddock-[repo].log | head -5
fi
```

---

## Output Format

Good coverage (80%+):
```
action-haddock | [repo] | ✓ | [coverage]% coverage | [warning-count] warnings
```

Low coverage (<80%):
```
action-haddock | [repo] | ⚠ | [coverage]% coverage | [warning-count] warnings | flag-for-improvement
```

---

## Notes

**Coverage targets:**
- 80%+ ⟜ good (all public API documented)
- 60-79% ⟜ acceptable (most documented, some gaps)
- <60% ⟜ flag for review (significant gaps in documentation)

Warnings typically indicate:
- Missing documentation on exported functions/types
- Broken cross-references
- Malformed Haddock syntax

Action always reports but doesn't fail on low coverage. Writeit may follow up with documentation work.

Used by:
- cleanit (documentation quality check)
- writeit (assess documentation state)

Common improvements:
- Add Haddock comments to all public functions
- Fix broken links in documentation
- Add examples to complex types

