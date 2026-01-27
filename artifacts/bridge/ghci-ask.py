#!/usr/bin/env python3
"""
ghci-ask ⟜ one-shot ghci query

Usage:
    echo ":t fmap" | ./ghci-ask.py
    ./ghci-ask.py ":t fmap"
    ./ghci-ask.py -p /path/to/project ":t myFunction"

For persistent sessions, use ghci-bridge.py instead.
"""

import subprocess
import sys
import argparse
import re

def strip_ansi(text):
    """Remove ANSI escape sequences from terminal output."""
    ansi = re.compile(r'\x1b\[[0-9;?]*[a-zA-Z]|\x1b[^[\(\s]')
    return ansi.sub('', text)

def ask_ghci(query, project_dir=None):
    """Use ghci -e to execute query directly."""

    # Build the command
    if project_dir:
        cmd = f'cd {project_dir} && ghci -e "{query}"'
    else:
        cmd = f'ghci -e "{query}"'

    try:
        result = subprocess.run(
            cmd,
            shell=True,
            capture_output=True,
            text=True,
            timeout=30
        )

        # Clean output: remove ANSI codes and extra whitespace
        output = strip_ansi(result.stdout + result.stderr)

        # Extract just the result line (skip ghci startup messages)
        lines = output.split('\n')

        # Find lines that look like results (contain :: or error text)
        result_lines = []
        for line in lines:
            line = line.strip()
            if not line:
                continue
            # Skip ghci banner/startup messages (they start with common ghci text)
            if any(x in line for x in ['Prelude', 'ghci>', 'GHCi', 'type', ':', 'Loaded']):
                if '::' not in line and not line.startswith(':'):
                    continue
            result_lines.append(line)

        return '\n'.join(result_lines).strip() if result_lines else output.strip()

    except subprocess.TimeoutExpired:
        return "error: query timeout"
    except Exception as e:
        return f"error: {e}"

def main():
    parser = argparse.ArgumentParser(description='One-shot ghci query')
    parser.add_argument('query', nargs='?', help='ghci command')
    parser.add_argument('-p', '--project', help='project directory')
    args = parser.parse_args()
    
    if args.query:
        query = args.query
    elif not sys.stdin.isatty():
        query = sys.stdin.read().strip()
    else:
        print("usage: ghci-ask ':t expression'", file=sys.stderr)
        sys.exit(1)
    
    result = ask_ghci(query, args.project)
    print(result)

if __name__ == "__main__":
    main()
