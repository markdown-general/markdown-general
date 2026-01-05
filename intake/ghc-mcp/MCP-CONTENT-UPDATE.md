# MCP Content Response Update

## Date
2026-01-04

## Problem
All ghci-mcp tools were returning raw JSON objects instead of proper MCP Content format, causing Claude Code to report "Tool ran without output or errors".

## Solution
Updated all tool handlers to return MCP-compliant Content objects.

## Changes Made

### Added Helper Function
```haskell
mcpTextContent :: Text -> Value
mcpTextContent msg = object
  [ "content" .= 
      [ object
          [ "type" .= ("text" :: Text)
          , "text" .= msg
          ]
      ]
  ]
```

### Updated All Tool Responses

**Before:**
```haskell
sendResponse ctx reqId $ object ["status" .= ("started" :: Text)]
```

**After:**
```haskell
sendResponse ctx reqId $ mcpTextContent "Started GHCi"
```

### Tool-by-Tool Updates

**start**
- `"GHCi is already running"` (when already running)
- `"Started GHCi"` (on success)

**send**
- `"Sent command: <cmd>"` (includes the command sent)

**listen**
- `"Listening for GHCi output..."` (returns immediately)
- Background thread continues to send `ghci/output` notifications

**stop**
- `"GHCi is not running"` (when not running)
- `"Stopped GHCi"` (on success)

**status**
- `"GHCi is not running"` (not started)
- `"GHCi is running"` (active)
- `"GHCi process has terminated"` (started but died)

## MCP Content Format

Every tool now returns:
```json
{
  "content": [
    {
      "type": "text",
      "text": "Human-readable message here"
    }
  ]
}
```

This is the format Claude Code expects from MCP tools.

## Async Flow Preserved

**listen** still works asynchronously:
1. Returns MCP Content immediately: `"Listening for GHCi output..."`
2. Spawns background thread to read stdout
3. Sends `ghci/output` notifications as data arrives
4. Valid JSON-RPC: Response has id, Notifications don't

## Testing

Rebuild and test:
```bash
cd ~/repos/ghc-mcp
cabal build ghci-mcp
cabal install ghci-mcp
# Restart Claude Code to reload the MCP server
```

Then test each tool to verify:
- Tool responses now visible in Claude Code
- Messages are human-readable
- Notifications still flow (if Claude Code supports them)

## Files Modified

- `/ghci-mcp/src/GhciMcp/Tools.hs` - All tool handlers updated

## Next Test

Verify in Claude Code whether:
1. ✓ Tool responses now appear (should work)
2. ❓ Notifications are visible (unknown)
