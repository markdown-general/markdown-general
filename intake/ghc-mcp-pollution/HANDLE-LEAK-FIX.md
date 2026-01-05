# CRITICAL BUG FIX: Handle Leak in stopGhci

## Date
2026-01-04

## Bug Found (Via Local Testing)

**Test Results:**
- ✓ status - works correctly
- ✓ start - works correctly
- ✓ send - works correctly
- ✗ stop - HANGS indefinitely
- ✗ read_output - HANGS indefinitely

## Root Cause

`stopGhci` in Process.hs closes stdin and terminates the process, but **never closes stdout/stderr handles**:

```haskell
stopGhci :: GhciState -> IO Bool
stopGhci state = do
  mhandle <- atomically $ readTVar state
  case mhandle of
    Nothing -> return False
    Just handle -> do
      hClose (ghciStdin handle)          -- ✓ Closes stdin
      terminateProcess (ghciProcessHandle handle)  -- ✓ Terminates
      atomically $ writeTVar state Nothing
      return True
      -- ❌ BUG: stdout and stderr handles never closed!
      -- ❌ BUG: Doesn't wait for process to exit!
```

**Why this causes hangs:**
- Open file handles remain after process termination
- When MCP server tries to clean up or when other operations touch these handles, they block
- `read_output` might also hang trying to read from orphaned handles

## The Fix

Close all handles and wait for process to terminate:

```haskell
stopGhci :: GhciState -> IO Bool
stopGhci state = do
  mhandle <- atomically $ readTVar state
  case mhandle of
    Nothing -> return False
    Just handle -> do
      -- Close stdin first (signals ghci to exit)
      hClose (ghciStdin handle)

      -- Wait for process to actually terminate
      _ <- waitForProcess (ghciProcessHandle handle)

      -- Now close output handles
      hClose (ghciStdout handle)
      hClose (ghciStderr handle)

      -- Clear state
      atomically $ writeTVar state Nothing
      return True
```

## Testing Methodology

Applied "human out of loop" principle from `content/evoke/human-out-of-loop.md`:

1. **Identified slow loop:** Testing through Claude Code MCP interface
2. **Created minimal local test:** Direct JSON-RPC via echo | ghc-mcp
3. **Reproduced locally:** Confirmed tools hang without human in loop
4. **Read source:** Found unclosed handles in Process.hs
5. **Documented fix:** This file

## Files to Change

- `/ghci-mcp/src/GhcMcp/Process.hs`
  - Fix `stopGhci` to close all handles
  - Add `waitForProcess` call
  - Ensure proper cleanup order

## Next Steps

1. Apply fix to Process.hs
2. Rebuild: `cd ~/repos/ghc-mcp && cabal build ghc-mcp`
3. Test locally with test script
4. Install: `cabal install ghc-mcp`
5. Restart Claude Code
6. Test through MCP interface

## Related Issues

This may also fix `read_output` hanging if it's related to handle state.

## Confidence Level

**HIGH** - Handle leak is visible in code, fix is straightforward, testing methodology is solid.
