## recovery to here:

# Test 1: Create actual cache
cache flatten
# Should create: ~/markdown-general-cache/cache-current-260106.md

# Verify creation
ls -la ~/markdown-general-cache/cache-current-260106.md

# Test 2: Round-trip identity
mkdir -p test-restore
cache split ~/markdown-general-cache/cache-current-260106.md --output-dir test-restore

# Verify (should be identical or minimal differences)
diff -r . test-restore/ || echo "Check for differences..."

# Test 3: Edge case - HTML comment handling
cat > test-edge.md << 'EOF'
# Test Document

<!-- This is a comment, not a delimiter -->
Some content here.

More content.
<!-- FILE: something.md -->
This looks like a delimiter but isn't.
EOF

cache flatten --include "test-edge.md" --output edge-cache.md
cache split edge-cache.md --output-dir restored/
cat restored/test-edge.md


# session-cache.md


**session** ⟜ install and test the new cache tool

**task** ⟜ get tools/cache.md working and verify it creates proper caches

## preparation

Read these files to understand the system:
1. **work/sisyphus.md** - sisyphus system overview
2. **work/card.md** - how cards work  
3. **zone/tools/cache.md** - the new cache tool implementation

## installation

### install cache card
```bash
# Install cache.md using existing card-api
card-api install ~/markdown-general/tools/cache.md

# Verify installation
cache --help
```

### add to path
```bash
# For current session
export PATH="$HOME/markdown-general/artifacts/bin:$PATH"

# Or add to profile for persistence
echo 'export PATH="$HOME/markdown-general/artifacts/bin:$PATH"' >> ~/.bashrc
source ~/.bashrc
```

## testing

### test 1: basic functionality
```bash
cd ~/markdown-general

# Dry run to see what would be included
cache flatten --dry-run
```

### test 2: create actual cache
```bash
# Create cache from work directory
cd ~/markdown-general/work
cache flatten

# Verify creation
ls -la ~/markdown-general-cache/cache-work-200106.md
```

### test 3: round-trip identity
```bash
# Test structure survives flatten/unflatten
cd ~/markdown-general

mkdir -p test/a/b
echo "# File A" > test/a/file.md
echo "# File B" > test/a/b/file.md

cache flatten --dir test --output test-cache.md
cache split test-cache.md --output-dir restored/

# Verify no differences
diff -r test/ restored/test/
```

### test 4: html comment edge case
```bash
# Test HTML comment handling
cat > test-edge.md << 'EOF'
# Test Document

<!-- This is a comment, not a delimiter -->
Some content here.

More content.
<!-- FILE: something.md -->
This looks like a delimiter but isn't.
EOF

cache flatten --include "test-edge.md" --output edge-cache.md
cache split edge-cache.md --output-dir restored/
cat restored/test-edge.md
```

## verification checklist

- [ ] `cache --help` works
- [ ] `cache flatten --dry-run` shows expected files
- [ ] `cache flatten` creates cache file in ~/markdown-general-cache/
- [ ] Round-trip test passes
- [ ] HTML comment edge case handled correctly

## cleanup
```bash
rm -rf test/ restored/
rm -f test-cache.md edge-cache.md
```

## reporting

After testing, report:
1. Any installation issues
2. Test results (especially round-trip and edge cases)
3. Whether cache tool works as expected
4. Any problems encountered

## note

If card-api breaks during installation, halt the job and we'll fix the Haskell issues manually.
