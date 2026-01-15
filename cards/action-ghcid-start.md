# card: action-ghcid-start ⟜ start ghcid with cabal repl

**type** ⟜ action / execution

**execution** ⟜ skip-step / no timeout (persistent process)

**input** ⟜ repo directory (must be clean, build must pass)

**task** ⟜ start ghcid --command="cabal repl" --outputfile=ghcid.txt; log PID

**output** ⟜ ✓ + [PID] | ghcid-ready and watching

**idempotence** ⟜ check if ghcid already running; skip if present

**effects** ⟜ reads: src/*.hs + cabal file, writes: ~/repos/[repo]/ghcid.txt, spawns: ghcid process, network: no

---

## Usage

Called by:
- buildit (after successful initial build)

---

## Bash Example

```bash
cd [repo-dir]

# Check if ghcid already running for this repo
if pgrep -f "ghcid.*$PWD" > /dev/null; then
  echo "✓ GHCID ALREADY RUNNING"
  pgrep -f "ghcid.*$PWD"
else
  # Start ghcid
  nohup ghcid --command="cabal repl" --outputfile=ghcid.txt > /dev/null 2>&1 &
  GHCID_PID=$!
  sleep 1

  if ps -p $GHCID_PID > /dev/null; then
    echo "✓ GHCID STARTED: $GHCID_PID"
  else
    echo "✗ GHCID FAILED TO START"
  fi
fi
```

---

## Notes

- Ghcid runs in background continuously
- Outputs to ghcid.txt in repo directory
- Yin watches ghcid.txt for changes
- Do not start multiple ghcid processes for same repo
- Idempotent: safe to call multiple times

