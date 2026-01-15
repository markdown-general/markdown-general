# card: action-read-cabal ⟜ extract metadata from .cabal file

**type** ⟜ action / analysis

**execution** ⟜ bounded worker / 10s timeout

**input** ⟜ repo directory (must contain *.cabal file)

**task** ⟜ parse .cabal file; extract key metadata and dependencies

**output** ⟜ ✓ + [metadata] + [dependency list with bounds]

**idempotence** ⟜ fully idempotent (read-only)

---

## Extraction

Locate .cabal file:
```bash
CABAL_FILE=$(find . -maxdepth 1 -name "*.cabal" | head -1)
if [ -z "$CABAL_FILE" ]; then
  echo "✗ No .cabal file found"
  exit 1
fi
```

Extract key fields:
```bash
# Name and version
NAME=$(grep "^name:" "$CABAL_FILE" | awk '{print $2}')
VERSION=$(grep "^version:" "$CABAL_FILE" | awk '{print $2}')

# Build-depends section
# Extract all build-depends entries with version bounds
echo "Package: $NAME $VERSION"
echo ""
echo "Dependencies:"
grep -A 100 "^build-depends:" "$CABAL_FILE" | \
  grep -v "^build-depends:" | \
  grep -v "^$" | \
  head -n 50 | \
  sed 's/^[ \t]*//' | \
  while read dep; do
    echo "  $dep"
  done
```

---

## Output Format

```
action-read-cabal | [repo] | package: [name] | version: [version]

Dependencies:
[package] [bound]
[package] [bound]
...
```

Example:
```
action-read-cabal | numhask-space | package: numhask | version: 0.11.2

Dependencies:
  base >=4.14 && <5
  containers >=0.6 && <0.7
  ghc-prim >=0.6.1 && <0.11
```

---

## Extended Metadata (Optional)

If needed, extract additional sections:
```bash
# cabal version
CABAL_VERSION=$(head -1 "$CABAL_FILE" | grep -oP 'cabal-version:\s+\K[\d.]+' || echo "unknown")

# license
LICENSE=$(grep "^license:" "$CABAL_FILE" | awk '{print $2}')

# ghc-options, language pragmas, extensions
grep "^language:" "$CABAL_FILE"
grep "^default-extensions:" "$CABAL_FILE"
```

---

## Notes

This action is read-only; it never modifies the .cabal file.

**effects** ⟜ reads: *.cabal metadata, writes: none, spawns: none, network: no

Output format is designed to be human-readable and parseable downstream (for dependendit or fixit analysis).

If .cabal is malformed (syntax errors), parsing may fail; that's OK—error is reported.

Used by:
- dependendit (extract current bounds for comparison)
- fixit (understand current state before editing)
- versionit (read version before proposing bump)

