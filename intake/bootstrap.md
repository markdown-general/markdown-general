#!/bin/bash
# bootstrap.sh
# Bootstrap the card system from scratch

set -e  # Exit on any error

echo "=== Card System Bootstrap ==="
echo ""

# Configuration
SISYPHUS_DIR="${SISYPHUS_DIR:-$HOME/sisyphus}"
BUILD_DIR="$SISYPHUS_DIR/artifacts/haskell-build"
BIN_DIR="$SISYPHUS_DIR/artifacts/bin"
TOOLS_DIR="$SISYPHUS_DIR/content/tools"

# Colors
GREEN='\033[0;32m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

step() {
    echo -e "${BLUE}→${NC} $1"
}

success() {
    echo -e "${GREEN}✓${NC} $1"
}

# Create directories
step "Creating directories..."
mkdir -p "$BUILD_DIR"
mkdir -p "$BIN_DIR"
mkdir -p "$TOOLS_DIR"
success "Directories created"

cd "$BUILD_DIR"

# Step 1: Bootstrap discover-executables
step "Step 1: Bootstrapping discover-executables..."

# Extract discover-executables
markdown-unlit main -h "$TOOLS_DIR/discover-executables.md" \
  "$TOOLS_DIR/discover-executables.md" discover-executables.hs

# Create minimal cabal.project
cat > cabal.project << 'EOF'
packages: .
EOF

# Create minimal cabal file with just discover-executables
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

# Build and install discover-executables
cabal build discover-executables
cabal install discover-executables --installdir="$BIN_DIR" --overwrite-policy=always

success "discover-executables bootstrapped"

# Step 2: Bootstrap card-api
step "Step 2: Bootstrapping card-api..."

# Extract card-api main executable
markdown-unlit main -h "$TOOLS_DIR/card-api.md" \
  "$TOOLS_DIR/card-api.md" card-api.hs

# Extract card-api test executables
step "Extracting card-api test executables..."
markdown-unlit noop -h "$TOOLS_DIR/card-api.md" \
  "$TOOLS_DIR/card-api.md" card-api-noop.hs

markdown-unlit bench-syscall -h "$TOOLS_DIR/card-api.md" \
  "$TOOLS_DIR/card-api.md" card-api-bench-syscall.hs

# Regenerate cabal file with all executables
"$BIN_DIR/discover-executables"

# Build and install all card-api executables
cabal build card-api card-api-noop card-api-bench-syscall
cabal install card-api card-api-noop card-api-bench-syscall \
  --installdir="$BIN_DIR" --overwrite-policy=always

success "card-api bootstrapped"

# Step 3: Verify installation
step "Step 3: Verifying installation..."

if ! command -v card-api &> /dev/null; then
    echo "ERROR: card-api not found in PATH"
    echo "Add $BIN_DIR to your PATH:"
    echo "  export PATH=\"$BIN_DIR:\$PATH\""
    exit 1
fi

card-api --help > /dev/null
success "card-api is working"

# Step 4: Test card-api
step "Step 4: Testing card-api..."
cd "$SISYPHUS_DIR"
card-api test "$TOOLS_DIR/card-api.md"

echo ""
echo "=== Bootstrap Complete ==="
echo ""
echo "The card system is ready to use!"
echo ""
echo "Next steps:"
echo "  1. Install a card:"
echo "     card-api install content/tools/flatten-md.md"
echo ""
echo "  2. Test a card:"
echo "     card-api test content/tools/flatten-md.md"
echo ""
echo "  3. Uninstall a card:"
echo "     card-api uninstall content/tools/flatten-md.md"
echo ""
