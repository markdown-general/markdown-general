#!/bin/bash
# repl-startup: Spawn cabal repl with file-based stdio
# Wire stdin, stdout, stderr to /tmp/ghci-*.txt

PROJECT_DIR="${1:-.}"

echo "repl-startup: spawning cabal repl"
echo "  project: $PROJECT_DIR"
echo "  stdin:  /tmp/ghci-in.txt"
echo "  stdout: /tmp/ghci-out.txt"
echo "  stderr: /tmp/ghci-err.txt"

# Initialize files
> /tmp/ghci-in.txt
> /tmp/ghci-out.txt
> /tmp/ghci-err.txt

# Change to project directory
cd "$PROJECT_DIR" || exit 1

# Spawn cabal repl with stdin fed from input file, output to files
# Use tail -f to keep reading as input file grows
(tail -f /tmp/ghci-in.txt 2>/dev/null) | cabal repl > /tmp/ghci-out.txt 2> /tmp/ghci-err.txt &

REPL_PID=$!
echo "  PID: $REPL_PID"

# Store PID for later cleanup
echo "$REPL_PID" > /tmp/ghci-repl.pid

# Wait for process to exit (or until killed)
wait $REPL_PID
