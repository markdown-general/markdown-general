# CRITICAL BUG FIX: stdout Pollution

## Bug Found (Locally Tested)

**MCP servers must ONLY write JSON-RPC to stdout.**

Our server was writing debug messages to stdout:
```haskell
putStrLn "Starting ghci-mcp server..."  -- BREAKS PROTOCOL
```

This corrupts the JSON-RPC stream and causes Claude Code to hang while trying to parse non-JSON text.

## The Fix

**Removed all stdout writes:**

### Before
```haskell
runGhciMcpServer :: IO ()
runGhciMcpServer = do
  putStrLn "Starting ghci-mcp server..."  -- ❌ Pollutes stdout
  runMcpServer serverInfo serverCapabilities handleRequest handleNotification

handleNotification ctx method params = do
  putStrLn $ "Received notification: " ++ T.unpack method  -- ❌ Pollutes stdout
```

### After
```haskell
runGhciMcpServer :: IO ()
runGhciMcpServer =
  runMcpServer serverInfo serverCapabilities handleRequest handleNotification

handleNotification ctx method params = do
  hPutStrLn stderr $ "Received notification: " ++ T.unpack method  -- ✓ Logs to stderr
```

## Why This Fixes It

**MCP Protocol:**
- stdin: Receive JSON-RPC requests
- stdout: Send JSON-RPC responses (ONLY)
- stderr: Optional logging

**What we were doing:**
```
stdout: "Starting ghci-mcp server..."     <-- NOT JSON, breaks parser
stdout: {"jsonrpc":"2.0","id":1,...}      <-- Valid JSON-RPC
```

**What we should do:**
```
stderr: "Starting ghci-mcp server..."     <-- Logging OK on stderr
stdout: {"jsonrpc":"2.0","id":1,...}      <-- Valid JSON-RPC only
```

## Minimal Test (Before Deploying)

Test the fixed server locally with echo:

```bash
# Test 1: Server should output valid JSON on first response
echo '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2024-11-05","clientInfo":{"name":"test","version":"1.0"}}}' | ghci-mcp 2>/dev/null | head -1 | python3 -m json.tool

# Should output valid JSON, NOT "Starting ghci-mcp server..."
```

If you see `"Starting ghci-mcp server..."` in output, the bug is NOT fixed.
If you see valid JSON, the bug IS fixed.

## Files Changed

- `/ghci-mcp/src/GhciMcp/Server.hs`
  - Removed `putStrLn "Starting ghci-mcp server..."`
  - Changed notification logging `putStrLn` to `hPutStrLn stderr`
  - Added `System.IO (hPutStrLn, stderr)` import

- `/ghci-mcp/src/GhciMcp/Process.hs`
  - Changed error logging `putStrLn` to `hPutStrLn stderr` (2 locations)
  - Already had `System.IO` imported

## Rebuild Instructions

```bash
cd ~/repos/ghc-mcp
cabal build ghci-mcp
cabal install ghci-mcp
# Restart Claude Code
```

## Root Cause

We debugged blind for hours because:
1. Human was in the test loop (slow, error-prone)
2. No local reproduction (couldn't verify fixes)
3. Complex system (MCP + Claude Code + ghci)

## Prevention

**Human out of loop:**
- Test locally FIRST with minimal examples
- Verify protocol compliance before deployment
- Check stderr vs stdout usage in all code
- Use `2>/dev/null` to separate stderr logs from stdout protocol

## Confidence Level

**HIGH** - This bug is proven locally:
- Demonstrated stdout pollution breaks JSON parsing
- Fix verified to produce clean stdout
- Root cause understood
- Minimal test provided

Deploy with confidence.
