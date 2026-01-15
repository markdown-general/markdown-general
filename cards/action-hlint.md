# card: action-hlint ⟜ style suggestions and refactoring advice

**type** ⟜ action / linting

**execution** ⟜ bounded worker / 30s timeout

**input** ⟜ repo directory (must have src/ or lib/ with .hs files)

**task** ⟜ hlint src/ --hint=Default 2>&1; report style suggestions (non-blocking)

**output** ⟜ ✓ + [suggestion-count] + [severity distribution]

**idempotence** ⟜ fully idempotent (read-only analysis)

**effects** ⟜ reads: *.hs files, writes: none (unless --refactor), spawns: none, network: no

---

## Pre-Check

Verify hlint is available:
```bash
which hlint > /dev/null 2>&1
if [ $? -ne 0 ]; then
  echo "✗ hlint not found in PATH"
  echo "Install with: cabal install hlint"
  exit 1
fi
```

Verify source directory exists:
```bash
if [ ! -d "src" ] && [ ! -d "lib" ]; then
  echo "✗ No src/ or lib/ directory found"
  exit 1
fi
```

---

## Run Analysis

```bash
SOURCE_DIR="src"
if [ ! -d "src" ]; then
  SOURCE_DIR="lib"
fi

timeout 30 hlint "$SOURCE_DIR" --hint=Default 2>&1 | tee /tmp/hlint-[repo].log
RESULT=$?

if [ $RESULT -eq 0 ]; then
  echo "✓ No hlint suggestions found"
else
  # hlint returns non-zero if suggestions found (not an error)
  TOTAL=$(grep -c "^.*:" /tmp/hlint-[repo].log || echo "0")
  INFO=$(grep -c "^.*: Warning:" /tmp/hlint-[repo].log || echo "0")
  WARN=$(grep -c "^.*: Error:" /tmp/hlint-[repo].log || echo "0")

  echo "✓ Style analysis complete"
  echo "Suggestions: $TOTAL total"
  echo "  Warnings: $INFO"
  echo "  Errors: $WARN"
fi
```

---

## Output Format

No suggestions:
```
action-hlint | [repo] | ✓ | no-suggestions
```

With suggestions:
```
action-hlint | [repo] | ✓ | [N]-suggestions | [info] info [warn] warnings
```

Example:
```
action-hlint | [repo] | ✓ | 7-suggestions | 5-info 2-warnings
```

---

## Sample Output

Typical hlint output:
```
src/Data/Foo.hs:23:1: Warning: Eta reduce
  foo = \x -> bar x
  Why not:
    foo = bar

src/Data/Bar.hs:45:10: Suggestion: Use zipWith
  map snd (zip xs ys)
  Why not:
    zipWith (,) xs ys
```

---

## Notes

**Important:** hlint suggestions are NOT failures. Action always succeeds (✓).

Categories of suggestions:
- **Warning** ⟜ likely refactoring opportunity
- **Error** ⟜ probable bug (but still suggestion, not enforced)
- **Suggestion** ⟜ style improvement

Fixit or operator may choose to:
- Apply suggestions (improve code quality)
- Ignore suggestions (if disagree with recommendation)
- Disable specific hints (via .hlint.yaml if needed)

Used by:
- cleanit (optional quality check; never blocks progress)

hlint is opinionated but optional. Suggestions improve code, but not applying them is OK.

**Directory vs File Mode:**
- `hlint src/` ⟜ analysis mode (reports suggestions, non-blocking) ✓
- `hlint src/ --refactor` ⟜ NOT SUPPORTED on directories (requires individual files)
- For batch refactoring: iterate over files or use --refactor with glob: `find src -name '*.hs' -exec hlint --refactor {} \;`

For cleanit workflow, use analysis mode (no --refactor flag) since we want operator visibility of suggestions before auto-applying changes.

