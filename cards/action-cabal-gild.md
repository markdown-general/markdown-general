# card: action-cabal-gild ⟜ format .cabal file to standard style

**type** ⟜ action / formatting

**execution** ⟜ bounded worker / 15s timeout

**input** ⟜ repo directory (must contain *.cabal file)

**task** ⟜ cabal-gild check and reformat .cabal file; verify syntax

**output** ⟜ ✓/✗ + [status] + [changes-count or errors]

**idempotence** ⟜ fully idempotent (running twice produces same result)

**effects** ⟜ reads: *.cabal, writes: *.cabal (formatted), spawns: none, network: no

---

## Pre-Check

Verify cabal-gild is available:
```bash
which cabal-gild > /dev/null 2>&1
if [ $? -ne 0 ]; then
  echo "✗ cabal-gild not found in PATH"
  echo "Install with: cabal install cabal-gild"
  exit 1
fi
```

Locate .cabal file:
```bash
CABAL_FILE=$(find . -maxdepth 1 -name "*.cabal" | head -1)
if [ -z "$CABAL_FILE" ]; then
  echo "✗ No .cabal file found"
  exit 1
fi
```

---

## Check Phase

Run check first (non-destructive):
```bash
cabal-gild -m check --input="$CABAL_FILE" 2>&1
CHECK_RESULT=$?

if [ $CHECK_RESULT -eq 0 ]; then
  echo "✓ .cabal file is already formatted correctly"
else
  echo "⚠ .cabal file needs formatting"
fi
```

---

## Format Phase

Apply formatting via stdin redirection (cabal-gild reads stdin, writes stdout):

```bash
# Method: stdin → cabal-gild → stdout → temp file → move to original
cat "$CABAL_FILE" | cabal-gild > "$CABAL_FILE.new" 2>&1
GILD_RESULT=$?

if [ $GILD_RESULT -eq 0 ]; then
  # Formatting succeeded; replace original
  mv "$CABAL_FILE.new" "$CABAL_FILE"
  echo "✓ .cabal file reformatted"

  # Check git diff to see what changed
  git diff --stat "$CABAL_FILE" 2>/dev/null | grep -q "$CABAL_FILE"
  if [ $? -eq 0 ]; then
    LINES=$(git diff "$CABAL_FILE" | wc -l)
    echo "Changes: $LINES lines modified"
  fi
else
  # Formatting failed; clean up temp file and error
  rm -f "$CABAL_FILE.new"
  echo "✗ cabal-gild formatting failed"
  echo "Return code: $GILD_RESULT"
fi
```

---

## Output Format

Success (no changes needed):
```
action-cabal-gild | [repo] | ✓ | already-formatted
```

Success (reformatted):
```
action-cabal-gild | [repo] | ✓ | reformatted | [N] lines changed
```

Error:
```
action-cabal-gild | [repo] | ✗ | gild-failed
```

---

## Stdin/Stdout Pattern

**Important:** cabal-gild uses stdin/stdout, not file mode:
- Input: reads from stdin (piped or redirected)
- Output: writes formatted result to stdout
- Usage: `cat file | cabal-gild > file.new && mv file.new file`

Do NOT use file mode flags like `--io="file"` or direct file arguments.

## Notes

cabal-gild standardizes:
- Indentation and spacing
- Field ordering
- Line wrapping
- Stanza organization

This is a stylistic cleanup, not a functional change. Safe to run multiple times.

Used by:
- cleanit (first phase of code standardization)

