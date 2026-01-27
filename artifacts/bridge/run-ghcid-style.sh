#!/bin/bash
# run-ghcid-style.sh ⟜ wrapper for ghcid-style.py

cd "$(dirname "$0")" || exit 1
./venv/bin/python3 ghcid-style.py "$@"
