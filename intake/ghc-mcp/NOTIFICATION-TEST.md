# ghc-mcp Notification Flow Test

## Date
2026-01-04

## Objective
Test whether Claude Code properly receives MCP notifications from ghc-mcp async tools.

## Test Sequence

### First Attempt (Wrong Order - Race Condition)
1. `start` → returned silently
2. `status` → returned silently
3. `send :t map` → returned silently
4. `listen` → returned silently, no notifications visible

### Second Attempt (Clean Slate)
1. `status` → returned silently
2. `stop` → returned silently

## Observations

**All tools return "Tool ran without output or errors"**
- No success messages
- No error messages
- No status information
- No notification content visible to Claude Code

## Analysis

### Possible Issues

**1. Tool Result Content Missing**
ghc-mcp tools may not be returning any content in their tool results. MCP protocol expects tools to return some content, even if it's just `{"status": "ok"}`.

**2. Claude Code Notification Handling**
- Notifications may be arriving but not surfaced to the assistant
- Claude Code may not support MCP notifications yet
- No way for assistant to observe notification stream

**3. Race Condition (First Test)**
Output from `:t map` may have been generated before `listen` spawned the reader thread. The implementation might not handle "catching up" to already-buffered output.

## Key Finding

**Even synchronous operations return nothing.** This suggests the primary issue is not notification flow, but basic tool result handling. Tools like `status` and `stop` should return immediate, synchronous results but are coming back empty.

## Next Steps

### Fix Immediate Tool Results
1. Update all ghc-mcp tools to return content in their results
2. `status` should return `{"running": true/false}`
3. `start` should return `{"status": "started"}` or error
4. `stop` should return `{"status": "stopped"}` or error
5. `send` should return `{"status": "sent", "command": "..."}`

### Fix Listen Flow
1. `listen` should return `{"status": "listening"}` immediately
2. Notifications flow separately (may not be visible to Claude)
3. Consider alternative: `listen` could block and return accumulated output synchronously
4. Or: Add `read` tool that synchronously reads available output

### Test Outside Claude Code
Use `mcp-client` or manual JSON-RPC to verify:
1. Tool results contain proper content
2. Notifications are actually being sent
3. Notification format is correct

## Architectural Decision Point

**Should ghc-mcp be async at all?**

Given Claude Code may not surface notifications, consider:
- Make tools synchronous and blocking
- `send` command could wait for prompt and return output
- Simpler for clients that don't handle notifications
- Trade latency for compatibility

OR

- Keep async design
- Add synchronous alternatives (`send_sync`, `read`)
- Better for clients that do support async
- More complex API

## References

- ghc-mcp source: `~/repos/ghc-mcp/`
- This conversation: Testing notification flow in Claude Code
- Related: `zone/tools/ghc-mcp.md`
