# rebuilding-tools.md

Instructions for rebuilding the card system tools after updates.

## what it is

When you update haskell-api.md, discover-executables.md, or other core tools, they need to be rebuilt manually since they can't rebuild themselves via haskell-api.

## bootstrap sequence

### Initial bootstrap (from scratch)

```bash
cd ~/markdown-general/artifacts/haskell-build

# 1. Bootstrap discover-executables
markdown-unlit main -h ../../zone/tools/discover-executables.md \
  ../../zone/tools/discover-executables.md discover-executables.hs

# Create minimal cabal file
cat > haskell-build.cabal << 'EOF'
cabal-version: 2.4
name: haskell-build
version: 0
build-type: Simple

executable discover-executables
  main-is: discover-executables.hs
  build-depends: base, directory, filepath
  ghc-options: -Wall
  default-language: Haskell2010
EOF

# Build and install
cabal build discover-executables
cabal install discover-executables --installdir=../bin --overwrite-policy=always

# 2. Bootstrap haskell-api
markdown-unlit main -h ../../zone/tools/haskell-api.md \
  ../../zone/tools/haskell-api.md card-api.hs

# Regenerate cabal file (now has both)
discover-executables

# Build and install
cabal build card-api
cabal install card-api --installdir=../bin --overwrite-policy=always
```

### Rebuild after updates

When you update haskell-api.md or discover-executables.md:

```bash
cd ~/markdown-general/artifacts/haskell-build

# Re-extract the updated tool
markdown-unlit main -h ../../zone/tools/haskell-api.md \
  ../../zone/tools/haskell-api.md card-api.hs

# Or for discover-executables
markdown-unlit main -h ../../zone/tools/discover-executables.md \
  ../../zone/tools/discover-executables.md discover-executables.hs

# Rebuild and reinstall
cabal build <tool-name>
cabal install <tool-name> --installdir=../bin --overwrite-policy=always
```

## why manual rebuilds

**Chicken-and-egg problem:**
- haskell-api installs cards
- But haskell-api is itself a card
- Can't use haskell-api to update itself (would use old version to install new version)

**Solution:** Manual extraction and rebuild for system tools.

## system tools

Tools that need manual rebuilds:
- **discover-executables** - generates cabal files
- **haskell-api** - installs/tests cards

Regular cards can be updated via `haskell-api install`.

## verification

After rebuild:

```bash
# Check version updated
<tool-name> --help

# For haskell-api specifically
cd ~/markdown-general
haskell-api install zone/tools/flatten-md.md --verbose
```

## future improvement

Could create a `bootstrap.sh` script that automates the rebuild sequence.
