# Manual Haskell-API Rebuild

When you update haskell-api.md, follow these steps to rebuild it.

## Why Manual?

haskell-api can't install itself (chicken-and-egg problem). System tools need manual extraction and rebuild.

## Quick Rebuild (after changes)

```bash
cd ~/markdown-general/artifacts/haskell-build

# 1. Extract main executable
markdown-unlit main -h ../../zone/tools/haskell-api.md \
  ../../zone/tools/haskell-api.md card-api.hs

# 2. Extract test executables (if they exist)
markdown-unlit noop -h ../../zone/tools/haskell-api.md \
  ../../zone/tools/haskell-api.md card-api-noop.hs

markdown-unlit bench-syscall -h ../../zone/tools/haskell-api.md \
  ../../zone/tools/haskell-api.md card-api-bench-syscall.hs

# 3. Regenerate cabal file
discover-executables

# 4. Build and install
cabal build card-api card-api-noop card-api-bench-syscall
cabal install card-api card-api-noop card-api-bench-syscall \
  --installdir=../bin --overwrite-policy=always

# 5. Verify
card-api --help
```

## Full Bootstrap (from scratch)

See `bootstrap.sh` for complete system bootstrap.

## Checking What Got Extracted

```bash
# List all .hs files in build directory
ls -la ~/markdown-general/artifacts/haskell-build/*.hs

# Check what executables are in cabal file
grep "^executable" ~/markdown-general/artifacts/haskell-build/haskell-build.cabal
```

## Common Issues

**"executable not found"**
- You extracted but didn't build/install
- Run `cabal build <name>` then `cabal install <name>`

**"noop not found when running bench-syscall"**
- card-api-noop wasn't installed
- Rebuild card-api with all executables

**Old test results in Status**
- Run `card-api uninstall` then reinstall to reset
