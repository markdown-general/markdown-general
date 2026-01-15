# card: action-ormolu ⟜ format Haskell source files

**type** ⟜ action / formatting

**execution** ⟜ bounded worker / 30s timeout

**input** ⟜ repo directory (must be git repo; uses git ls-files)

**task** ⟜ ormolu --mode inplace $(git ls-files '*.hs'); reformat all Haskell source

**output** ⟜ ✓ + [file-count] + [changes-made or "already-formatted"]

**idempotence** ⟜ fully idempotent (running twice produces same result)

**effects** ⟜ reads: *.hs files, writes: *.hs files (formatted), spawns: none, network: no

---

## Pre-Check

Verify ormolu is available:
```bash
which ormolu > /dev/null 2>&1
if [ $? -ne 0 ]; then
  echo "✗ ormolu not found in PATH"
  echo "Install with: cabal install ormolu"
  exit 1
fi
```

Verify git repo:
```bash
git rev-parse --git-dir > /dev/null 2>&1
if [ $? -ne 0 ]; then
  echo "✗ Not a git repository"
  exit 1
fi
```

---

## Find Source Files

```bash
HS_FILES=$(git ls-files '*.hs')
FILE_COUNT=$(echo "$HS_FILES" | wc -l)

if [ $FILE_COUNT -eq 0 ]; then
  echo "✓ No Haskell source files found"
  exit 0
fi

echo "Found $FILE_COUNT .hs files"
```

---

## Format In-Place

```bash
ormolu --mode inplace $(git ls-files '*.hs') 2>&1
FORMAT_RESULT=$?

if [ $FORMAT_RESULT -eq 0 ]; then
  # Check if anything changed
  CHANGED=$(git status --porcelain | grep '\.hs$' | wc -l)

  if [ $CHANGED -eq 0 ]; then
    echo "✓ All files already properly formatted"
  else
    echo "✓ Formatted $CHANGED files"
    echo ""
    echo "Changed files:"
    git status --porcelain | grep '\.hs$'
  fi
else
  echo "✗ ormolu formatting failed"
  echo "Return code: $FORMAT_RESULT"
fi
```

---

## Output Format

Success (no changes):
```
action-ormolu | [repo] | ✓ | [file-count] files | already-formatted
```

Success (formatted):
```
action-ormolu | [repo] | ✓ | [file-count] files | [N]-formatted
```

Error:
```
action-ormolu | [repo] | ✗ | formatting-failed
```

---

## Notes

ormolu standardizes:
- Indentation and alignment
- Import organization
- Operator spacing
- Line length (wrapping)

Ormolu is opinionated; once run, all Haskell source is in consistent style.

Safe to run multiple times; idempotent.

Used by:
- cleanit (second phase, after cabal-gild)

Note: ormolu requires ghc to be available (it parses files to reformat them).

