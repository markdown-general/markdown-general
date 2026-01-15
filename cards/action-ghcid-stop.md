# card: action-ghcid-stop ⟜ stop ghcid process

**type** ⟜ action / process management

**execution** ⟜ bounded worker / 5s timeout

**input** ⟜ repo directory (must have ghcid process running)

**task** ⟜ pkill -f "ghcid.*[repo]"; verify process stopped

**output** ⟜ ✓/✗ + [status]

**idempotence** ⟜ safe to call if ghcid not running (returns already-stopped)

**effects** ⟜ reads: PID from ghcid.txt, writes: none, spawns: kills ghcid process, network: no

---

## Pre-Check

Identify if ghcid is running:
```bash
GHCID_PID=$(pgrep -f "ghcid.*$PWD" 2>/dev/null)

if [ -z "$GHCID_PID" ]; then
  echo "✓ GHCID NOT RUNNING"
  exit 0
fi
```

---

## Stop Process

```bash
GHCID_PID=$(pgrep -f "ghcid.*$PWD" 2>/dev/null)

if [ -z "$GHCID_PID" ]; then
  echo "✓ GHCID ALREADY STOPPED"
else
  # Kill ghcid process
  kill $GHCID_PID 2>/dev/null
  sleep 1

  # Verify it's gone
  if pgrep -p $GHCID_PID > /dev/null 2>&1; then
    # Force kill if it didn't die gracefully
    kill -9 $GHCID_PID
    echo "✓ GHCID FORCE STOPPED"
  else
    echo "✓ GHCID STOPPED"
  fi
fi
```

---

## Output Format

Success (stopped):
```
action-ghcid-stop | [repo] | ✓ | ghcid-stopped
```

Success (already stopped):
```
action-ghcid-stop | [repo] | ✓ | already-stopped
```

---

## Clean Up

Optional: Remove ghcid.txt output file:
```bash
rm -f ghcid.txt
echo "Cleaned up ghcid.txt"
```

---

## Notes

- Safe to call even if ghcid is not running
- Idempotent (can be called multiple times)
- Used by: buildit (on termination), manually by operator

This is a cleanup action. Normally runs when:
- Development cycle complete
- Switching projects
- Troubleshooting stuck process

