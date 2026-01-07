# code-learnings.md

Lessons learned from building the card system in Haskell.

## deck: dependency-tattoo

**Always Check Dependencies Before Import** ⟜ prevent build failures

**Pattern:**
```haskell
import System.Clock (Clock(Monotonic))  -- STOP! Is 'clock' in build-depends?
import Data.Map.Strict as Map          -- STOP! Is 'containers' in build-depends?
```

**The Rule:**
1. Write import line
2. Immediately check package name (Hackage/hoogle)
3. Check build-depends in discover-executables.md
4. Add if missing
5. Rebuild discover-executables
6. Regenerate cabal file

**Why It Matters:**
- Hit us 3 times in one session: `clock`, `containers`, `Map`
- Causes cryptic "hidden package" errors
- Wastes time rebuilding multiple times

**Tattoo on your brain:** `import` → `check deps` → `add to discover-executables`

## deck: haskell-type-checking

**Type Signatures Are Documentation** ⟜ read them on Hackage first

**Examples from session:**

```haskell
-- WRONG: Assumed Tick was a type
flattenDirPerf :: FilePath -> FilePath -> PerfT IO Tick ()

-- RIGHT: Checked Hackage, it's polymorphic
flattenDirPerf :: (Semigroup t) => FilePath -> FilePath -> PerfT IO t ()
```

```haskell
-- WRONG: Assumed reportGolden takes FilePath
reportOpts { reportGolden = goldenPath }

-- RIGHT: Checked Hackage, it's a record
reportOpts { reportGolden = (reportGolden reportOpts) { golden = goldenPath } }
```

**The Pattern:**
1. Type error appears
2. Don't guess - **read the Hackage docs**
3. Find the actual type definition
4. Understand the structure (record? newtype? sum type?)
5. Write correct code

**Shortcuts that failed:**
- Guessing type names (Tick doesn't exist)
- Guessing field names (reportFilePath doesn't exist)
- Assuming simple types (Golden is a record, not a String)

## deck: repl-vs-build

**REPL Catches What Build Misses** ⟜ use repl to verify

**The Problem:**
```bash
cabal build flatten-md-bench-perf    # Succeeds!
cabal repl flatten-md-bench-perf     # Fails with "Not in scope: fromString"
```

**Why:**
- `cabal build` sometimes caches or doesn't fully type-check
- `cabal repl` does complete type-checking
- OverloadedStrings needs imports in scope

**The Fix:**
```haskell
{-# LANGUAGE OverloadedStrings #-}
import Data.String  -- Now fromString is in scope
```

**Lesson:** When build succeeds but something seems off, try `cabal repl` to get real type errors.

## deck: tagging-discipline

**All Code Blocks Must Be Tagged** ⟜ no untagged blocks allowed

**The Rule:**
```markdown
❌ ```haskell
   module Main where

✅ ```haskell main
   module Main where

✅ ```haskell bench-perf
   module Main where
```

**Why:**
- Untagged blocks get extracted to wrong files
- Causes duplicate module declarations
- Parser errors that are hard to debug

**Tag Convention:**
- `main` → cardname executable
- `bench-*` → cardname-bench-* executable
- `tick` → cardname-tick test executable
- NO untagged blocks

**Check before sharing:**
```bash
grep '^```haskell$' your-card.md  # Should return nothing!
```

## deck: perf-library-patterns

**Read Perf Docs Before Using** ⟜ types are not obvious

**Key Learnings:**

```haskell
-- Golden is a RECORD, not a string
data Golden = Golden 
  { golden :: FilePath
  , check :: CheckGolden  
  , record :: RecordGolden
  }

-- Update only the path, preserve flags
opts { reportGolden = (reportGolden opts) { golden = path } }
```

```haskell
-- PerfT is polymorphic in measurement type
flattenDirPerf :: (Semigroup t) => FilePath -> FilePath -> PerfT IO t ()

-- statify transforms keys: Map Text a -> Map [Text] a
statify StatMedian m  -- Keys already become [Text], no Map.mapKeys needed!
```

**Pattern:**
1. Find example code (like explore.hs)
2. Read Hackage docs for actual types
3. Understand the structure before using
4. Test in REPL first

**Don't assume:** Check the types!

## deck: bootstrap-sequence

**System Tools Need Manual Bootstrap** ⟜ can't use card-api to install itself

**The Chicken-and-Egg:**
```
card-api installs cards
→ but card-api is itself a card
→ can't use card-api to install card-api
→ need manual extraction and build
```

**The Sequence:**
1. **discover-executables** (manual: extract → build → install)
2. **card-api** (manual: extract → discover-executables → build → install)
3. **Everything else** (automated: use card-api install)

**When to Manual Bootstrap:**
- Initial setup from scratch
- After updating discover-executables.md or card-api.md
- Never for regular cards like flatten-md

**The Script:**
```bash
# Use bootstrap.sh for full setup
~/markdown-general/bootstrap.sh

# Or follow manual-card-api-rebuild.md for updates
```

**Remember:** System tools = manual, Regular cards = card-api install

## deck: status-section-updates

**Multi-Line Results Need Special Handling** ⟜ indent continuation lines

**The Pattern:**
```markdown
**Tests:**
- tick: ✓ tick: passed
- bench-perf: ✓
    label1          label2          results
    concat          time            2.00e3
    find-files      time            1.92e5
```

**Implementation:**
```haskell
-- Detect multi-line output
case outputLines of
  []       -> TestTimed seconds     -- No output
  [single] -> TestPassed single     -- One line
  multi    -> TestMultiLine multi   -- Multiple lines

-- Format with indentation
formatTestResult (TestMultiLine lines) = 
  T.pack $ "✓\n" ++ unlines (map ("    " ++) lines)

-- Remove old continuation lines when replacing
let restWithoutContinuation = dropWhile isContinuationLine rest
```

**The Rule:**
- Single line: `- test: ✓ result`
- Multi-line: `- test: ✓` followed by indented lines
- Continuation = indented >2 spaces
- Old results must be cleaned up before new ones added

**Why It Matters:**
- Perf reports are inherently multi-line
- Without cleanup, stale results accumulate
- Status section becomes unreadable

## deck: filepath-handling

**Working Directory Matters** ⟜ paths are relative to CWD

**The Traps:**
```haskell
-- WRONG: ~ doesn't expand in Haskell
flattenDirPerf "~/markdown-general/content/base" "/tmp/out.md"
-- Error: ~/markdown-general/content/base: does not exist

// RIGHT: Use getHomeDirectory
home <- getHomeDirectory
flattenDirPerf (home ++ "/markdown-general/content/base") "/tmp/out.md"
```

```haskell
// WRONG: Absolute path conflicts with perf's directory handling
replaceDefaultFilePath "artifacts/haskell-golden/file.golden" opts
-- Creates: other/artifacts/haskell-golden/file.golden.perf

// RIGHT: Update record field directly
opts { reportGolden = (reportGolden opts) { golden = path } }
```

**The Rules:**
1. **No tilde expansion** - use `getHomeDirectory` from `System.Directory`
2. **CWD aware** - executables run from wherever card-api was called
3. **Relative paths** - prefer relative to project root when possible
4. **Library assumptions** - some libraries add prefixes/suffixes (like perf's `other/`)

**Pattern:**
```haskell
-- For home directory
home <- getHomeDirectory
let path = home </> "markdown-general/content/base"

// For project relative
let path = "artifacts/haskell-golden/file.golden"  -- From ~/markdown-general

// For libraries with path handling
-- Read the docs, don't assume!
```

**Remember:** Test paths in REPL with `:!pwd` to see where you are!

## status

**Last updated:** 2025-01-01
