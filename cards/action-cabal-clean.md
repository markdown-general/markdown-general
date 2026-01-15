# card: action-cabal-clean ⟜ clean build artifacts

**type** ⟜ action / build management

**execution** ⟜ bounded worker / 15s timeout

**input** ⟜ repo directory

**task** ⟜ cabal clean; remove dist-newstyle; clear ghcid cache; reset build state

**output** ⟜ ✓ + [status]

**idempotence** ⟜ fully idempotent (cleaning when already clean is safe)

**effects** ⟜ reads: none, writes: deletes ~/repos/[repo]/dist-newstyle/, spawns: none, network: no

---

## Task

```bash
# Remove cabal build artifacts
cabal clean

# Remove dist directory if it exists
rm -rf dist-newstyle

# Remove ghcid cache/output files
rm -f ghcid.txt .ghcid

# Remove any leftover build logs
rm -f /tmp/cabal-build-[repo].log /tmp/cabal-test-[repo].log
```

---

## Verification

After clean:
```bash
# Verify build directory is gone
if [ ! -d "dist-newstyle" ]; then
  echo "✓ CLEAN SUCCESSFUL"
  echo "Build artifacts removed"
else
  echo "⚠ dist-newstyle still present (may be readonly)"
fi
```

---

## Output Format

Success:
```
action-cabal-clean | [repo] | ✓ | cleaned
```

Partial (some files remain):
```
action-cabal-clean | [repo] | ✓ | partially-cleaned | [warnings if any]
```

---

## Notes

- Safe to run multiple times
- Running clean before rebuild ensures fresh state (no cached artifacts interfering)
- Used by: buildit (between full builds), cleanit (before test suite), fixit (before retry)
- Typically called before important quality checks to ensure accurate results

This is not destructive to source code; only removes build artifacts.

