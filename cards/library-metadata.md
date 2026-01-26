library-metadata ⟜ extract package name and version from cabal file

**instruction**
NAME=$(grep "^name:" *.cabal | awk '{print $2}')
VERSION=$(grep "^version:" *.cabal | awk '{print $2}')
echo "name: $NAME version: $VERSION"
