#!/usr/bin/env python3
"""
ghci-repl ⟜ robust persistent cabal repl wrapper (ghcid-inspired)

Based on ghcid's approach:
- Inject marker functions into ghci at startup
- Send query + marker command atomically
- Read response until marker appears
- Incrementing counter ensures marker uniqueness
- No prompt parsing, no buffering ambiguity

Watches /tmp/ghci-q.txt for queries, writes /tmp/ghci-a.txt for answers.

Usage:
    ./ghcid-style.py /path/to/project
"""

import pexpect
import sys
import time
import re
from pathlib import Path
from threading import Thread, Lock, Event
from queue import Queue

QUERY_FILE = Path("/tmp/ghci-q.txt")
ANSWER_FILE = Path("/tmp/ghci-a.txt")

class GhciRepl:
    def __init__(self, project_dir=None):
        self.project_dir = project_dir
        self.ghci = None
        self.marker_counter = 0
        self.marker_lock = Lock()
        self.exec_lock = Lock()  # Enforce single-threaded execution

    def start(self):
        """Start cabal repl with proper initialization."""
        cmd = 'cabal repl'
        if self.project_dir:
            cmd = f'cd {self.project_dir} && cabal repl'

        print(f"starting: {cmd}")

        try:
            # Spawn via bash to ensure proper shell setup
            self.ghci = pexpect.spawn(
                '/bin/bash',
                ['-c', cmd],
                encoding='utf-8',
                timeout=60
            )

            # Wait for ghci to be ready (look for GHCi banner)
            self.ghci.expect(['GHCi, version', 'ghci>', 'Prelude> '], timeout=60)
            print("cabal repl started")

            # Inject marker functions: import System.IO with custom name
            self._send_command('import System.IO as INTERNAL_GHCID')

            # Set custom prompt to avoid ambiguity
            self._send_command(':set prompt "#~GHCID-START~#"')
            self._send_command(':set prompt-cont "#~GHCID-START~#"')

            print("ghci ready")
            return True

        except pexpect.exceptions.TIMEOUT:
            print("error: cabal repl startup timeout")
            return False
        except Exception as e:
            print(f"error: {e}")
            return False

    def _send_command(self, cmd):
        """Send a command and wait for prompt (initialization only)."""
        self.ghci.sendline(cmd)
        # Expect the next prompt
        self.ghci.expect(['#~GHCID-START~#', 'ghci>', 'Prelude> '], timeout=30)

    def _next_marker(self):
        """Generate next unique marker."""
        with self.marker_lock:
            self.marker_counter += 1
            return f"#~GHCID-FINISH-{self.marker_counter}~#"

    def ask(self, query):
        """
        Send query with marker, read until marker appears.
        Uses ghcid's approach: inject marker command into ghci.
        Ensures proper synchronization and no off-by-one lags.
        """
        query = query.strip()
        if not query:
            return ""

        # Enforce single-threaded execution
        if not self.exec_lock.acquire(blocking=False):
            return "error: ghci is busy (not single-threaded)"

        try:
            marker = self._next_marker()

            # Send query
            self.ghci.sendline(query)

            # Send marker command on a fresh line
            # This prints the marker, guaranteeing completion detection
            self.ghci.sendline(f'INTERNAL_GHCID.putStrLn "{marker}"')

            # Read output until marker appears
            # Accumulate in buffer
            output = ""
            start_time = time.time()
            timeout_secs = 30

            while time.time() - start_time < timeout_secs:
                try:
                    chunk = self.ghci.read_nonblocking(size=1024, timeout=0.1)
                    output += chunk

                    # Check if both query and marker are in output
                    # (ensures we have the full response for this query)
                    if marker in output:
                        break

                except pexpect.exceptions.TIMEOUT:
                    # No more data available right now, but keep trying
                    pass
                except pexpect.exceptions.EOF:
                    return "error: ghci process ended"

            if marker not in output:
                return "error: query timeout waiting for marker"

            # Split output by marker to isolate this query's response
            # The marker is our boundary
            before_marker, after_marker = output.split(marker, 1)

            # Now extract just the result: look backward from marker for the query echo
            # Then extract everything between query echo and marker
            lines = before_marker.split('\n')

            # Find the LAST occurrence of the query (most recent)
            query_line_idx = -1
            for i in range(len(lines) - 1, -1, -1):
                if query in lines[i]:
                    query_line_idx = i
                    break

            if query_line_idx < 0:
                # Query echo not found, shouldn't happen
                return "error: query echo not found in output"

            # Extract lines after the query, up to the marker
            result_lines = lines[query_line_idx + 1:]

            # Clean: remove empty lines, prompts, and internal command echoes
            cleaned = []
            for line in result_lines:
                stripped = line.strip()
                # Skip empty lines, prompts, and internal commands
                if not stripped:
                    continue
                if stripped == '#~GHCID-START~#':
                    continue
                if 'INTERNAL_GHCID.putStrLn' in line:
                    continue
                cleaned.append(line)

            # Remove trailing empty lines
            while cleaned and not cleaned[-1].strip():
                cleaned.pop()

            result = '\n'.join(cleaned).strip()

            # Remove ANSI codes
            ansi = re.compile(r'\x1b\[[0-9;?]*[a-zA-Z]|\x1b[^[\(\s]')
            result = ansi.sub('', result)

            return result

        except Exception as e:
            return f"error: {e}"
        finally:
            self.exec_lock.release()

    def watch(self, poll_interval=0.1):
        """Watch query file, process queries."""
        print(f"watching {QUERY_FILE}")
        print(f"answers → {ANSWER_FILE}")
        print("ctrl-c to stop\n")

        last_mtime = 0

        while True:
            try:
                if QUERY_FILE.exists():
                    mtime = QUERY_FILE.stat().st_mtime
                    if mtime > last_mtime:
                        last_mtime = mtime
                        query = QUERY_FILE.read_text().strip()

                        if query:
                            print(f"← {query}")
                            response = self.ask(query)
                            print(f"→ {response[:80]}{'...' if len(response) > 80 else ''}\n")
                            ANSWER_FILE.write_text(response)

                time.sleep(poll_interval)

            except KeyboardInterrupt:
                print("\nstopping")
                break
            except Exception as e:
                print(f"error: {e}")
                ANSWER_FILE.write_text(f"error: {e}")

    def interactive(self):
        """Interactive test mode."""
        print("interactive mode (ctrl-d to exit)\n")
        try:
            while True:
                query = input("> ")
                if query.strip():
                    result = self.ask(query)
                    print(result)
                    print()
        except EOFError:
            print("\ndone")

    def close(self):
        """Clean shutdown."""
        if self.ghci:
            try:
                self.ghci.sendline(':quit')
                self.ghci.close()
            except:
                pass

def main():
    project_dir = sys.argv[1] if len(sys.argv) > 1 else None

    repl = GhciRepl(project_dir)

    try:
        if not repl.start():
            sys.exit(1)

        # Mode selection
        if sys.stdin.isatty():
            mode = input("mode? [w]atch files / [i]nteractive: ").strip().lower()
            if mode.startswith('i'):
                repl.interactive()
            else:
                repl.watch()
        else:
            repl.watch()
    finally:
        repl.close()

if __name__ == "__main__":
    main()
