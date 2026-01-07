# haskell-build-bootstrap.md

One-time setup for the Haskell build directory used by haskell-api.md.

Creates artifacts/haskell-build/ with initial cabal project. haskell-api.md will add executable sections as cards are installed.

## what it does

Creates the foundation for Haskell card compilation:
- artifacts/haskell-build/ directory
- haskell-build.cabal project file
- markdown-unlit as build tool dependency
- Empty executable list (populated by card-api install)

## why separate from haskell-api

**One-time setup** ⟜ run once, used by all Haskell cards
**Foundation** ⟜ cabal project that haskell-api modifies
**Clean separation** ⟜ bootstrap creates, haskell-api populates

## extraction

To extract and run the bootstrap script:

```bash
# Extract script from this markdown file
sed -n '/^```bash$/,/^```$/p' ~/markdown-general/zone/tools/haskell-build-bootstrap.md | sed '1d;$d' > /tmp/bootstrap.sh

# Run it from sisyphus directory
cd ~/markdown-general
bash /tmp/bootstrap.sh
```

Or copy/paste the script directly from the run section below.

## run

Save and execute this script from ~/markdown-general:

```bash
#!/bin/bash
# Bootstrap script for sisyphus haskell-build system
# Run from ~/markdown-general directory

set -e  # Exit on any error

echo "=== Sisyphus Haskell Build Bootstrap ==="
echo

# Ensure we're in sisyphus directory
if [ ! -d "content/tools" ]; then
    echo "ERROR: Run this from ~/markdown-general directory"
    exit 1
fi

# Create build directory
echo "Creating build directories..."
mkdir -p artifacts/haskell-build
mkdir -p artifacts/bin

# Create initial cabal project
echo "Creating haskell-build.cabal..."
cat > artifacts/haskell-build/haskell-build.cabal << 'EOF'
cabal-version: 3.0
name: haskell-build
version: 0.1.0.0
build-type: Simple

common deps
  default-language: Haskell2010
  ghc-options: -Wall -pgmL markdown-unlit
  build-tool-depends: markdown-unlit:markdown-unlit

-- Executables added by haskell-api install
EOF

echo "✓ Created artifacts/haskell-build/haskell-build.cabal"

# Phase 2: Bootstrap haskell-api manually
echo
echo "Bootstrapping haskell-api..."

# Create symlink
cd artifacts/haskell-build
ln -sf ../../zone/tools/haskell-api.md card-api.lhs

# Add card-api executable to cabal file
cat >> haskell-build.cabal << 'EOF'

executable card-api
  import: deps
  main-is: card-api.lhs
  build-depends: base, text, optparse-applicative, directory, filepath, process
EOF

# Build
echo "Building card-api..."
if ! cabal build card-api; then
    echo "ERROR: cabal build failed"
    exit 1
fi

# Find and copy executable
echo "Installing to artifacts/bin/..."
CARD_API=$(find dist-newstyle -type f -name card-api -path "*/build/card-api/card-api" | head -1)
if [ -z "$CARD_API" ]; then
    echo "ERROR: Could not find built card-api executable"
    exit 1
fi

cp "$CARD_API" ../bin/card-api

cd ../..

echo
echo "✓ Bootstrap complete!"
echo
echo "Add to PATH:"
echo "  export PATH=\$HOME/markdown-general/artifacts/bin:\$PATH"
echo
echo "Then use haskell-api to install other cards:"
echo "  card-api install ~/markdown-general/zone/tools/flatten-md.md"
echo
```

## structure

Initial cabal file structure:

```cabal
cabal-version: 3.0
name: haskell-build
version: 0.1.0.0
build-type: Simple

common deps
  default-language: Haskell2010
  ghc-options: -Wall -pgmL markdown-unlit
  build-tool-depends: markdown-unlit:markdown-unlit

-- Executables added by haskell-api install
```

**haskell-api install** will append executable sections:

```cabal
executable flatten-md
  import: deps
  main-is: flatten-md.lhs
  build-depends: base, text, optparse-applicative, directory, filepath

executable card-api
  import: deps
  main-is: card-api.lhs
  build-depends: base, text, optparse-applicative, directory, filepath, process
```

## verification

After running bootstrap:

```bash
ls -la ~/markdown-general/artifacts/haskell-build/
cat ~/markdown-general/artifacts/haskell-build/haskell-build.cabal
```

Expected:
- haskell-build.cabal exists
- Contains common deps section
- No executables yet

## next steps

After bootstrap:

1. Install first card:
```bash
card-api install ~/markdown-general/zone/tools/flatten-md.md
```

2. Verify executable section added:
```bash
cat ~/markdown-general/artifacts/haskell-build/haskell-build.cabal
```

3. Check binary created:
```bash
ls ~/markdown-general/artifacts/bin/
```
