# card: action-cabal-build ⟜ full build compilation

**type** ⟜ action / execution

**execution** ⟜ bounded worker / 120s timeout (longer for large projects)

**input** ⟜ repo directory (must be clean)

**task** ⟜ cabal build all 2>&1; capture full output including warnings

**output** ⟜ ✓/✗ + [error count] [warning count] + [last 20 lines of output]

**capture** ⟜ save full output to /tmp/cabal-build-[repo].log for inspection

**idempotence** ⟜ builds are cached; running twice is safe (second may use cache)

**effects** ⟜ reads: src/*.hs + cabal file, writes: ~/repos/[repo]/dist-newstyle/, spawns: none, network: no

---

## Usage

Called by:
- buildit (initial build)
- cleanit (post-format build)
- fixit (rebuild after fix)
- verifyit (final verification before publish)

Called individually for adhoc: `echo "cabal build needed" >> flows-log && action-cabal-build`

---

## Bash Example

```bash
cd [repo-dir]
timeout 120 cabal build all 2>&1 | tee /tmp/cabal-build-[repo].log
RESULT=$?

if [ $RESULT -eq 0 ]; then
  echo "✓ BUILD SUCCESS"
  tail -5 /tmp/cabal-build-[repo].log
else
  echo "✗ BUILD FAILED"
  echo "Return code: $RESULT"
  tail -20 /tmp/cabal-build-[repo].log
fi
```

---

## Notes

- Timeout 120s is typical; may need adjustment for large projects
- If timeout occurs, log as "BUILD TIMEOUT" not failure
- Warnings are informational; does not fail if warnings present
- Errors cause build failure; must be resolved

