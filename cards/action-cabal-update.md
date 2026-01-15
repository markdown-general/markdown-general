# card: action-cabal-update ⟜ fresh hackage index

**type** ⟜ action / execution

**execution** ⟜ bounded worker / 30s timeout

**task** ⟜ cabal update; refresh hackage index

**output** ⟜ ✓/✗ + [index state] [timestamp]

**idempotence** ⟜ running twice produces fresh index both times; safe to re-run

**effects** ⟜ reads: hackage remote, writes: ~/.cabal/packages/, spawns: none, network: yes

---

## Usage

Called by:
- lockit (establish fresh baseline)
- dependendit (before checking outdated packages)
- buildit (if dependencies change)

---

## Bash Example

```bash
timeout 30 cabal update 2>&1
RESULT=$?

if [ $RESULT -eq 0 ]; then
  echo "✓ CABAL UPDATE SUCCESS"
  cabal outdated 2>&1 | head -1  # Show index state line if available
else
  echo "✗ CABAL UPDATE FAILED"
  echo "Return code: $RESULT"
fi
```

---

## Notes

- Updates Hackage index from remote
- Fast operation (typically 2-5s)
- Network-dependent (may retry if connectivity issues)
- Safe to run frequently

