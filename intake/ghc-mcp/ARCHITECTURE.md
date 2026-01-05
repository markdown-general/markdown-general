# ghci-mcp Architecture Summary

## Overview

We've built two Haskell packages:

1. **mcp-async** - Generic asynchronous MCP server library
2. **ghci-mcp** - Concrete MCP server for GHCi using mcp-async

## The Problem We Solved

Existing Haskell MCP libraries (`mcp`, `mcp-server`) force synchronous patterns:

```haskell
-- Must block until complete result is ready
handleToolCall :: Params -> IO Result
```

This doesn't work for long-running processes like GHCi where we want to:
- Start the process (return immediately)
- Send commands (return acknowledgment, not wait for output)
- Stream output as it arrives (via notifications)

## The Solution: JSON-RPC 2.0 Notifications

JSON-RPC 2.0 spec allows:
- **Requests** (with `id`) → eventually get one **Response**
- **Notifications** (no `id`) → fire-and-forget, sent anytime
- **No timing constraints** on when responses arrive

## mcp-async Library Architecture

```
mcp-async/
├── src/MCP/Async/
│   ├── JsonRpc.hs       # JSON-RPC 2.0 types (Request, Response, Notification)
│   ├── Protocol.hs      # MCP protocol types (Tool, Content, etc.)
│   ├── Server.hs        # Async server (sends/receives messages)
│   └── Async.hs         # Main module (re-exports + high-level API)
```

### Key Types

```haskell
-- Request handler - can send notifications before responding
type RequestHandler = 
  ServerContext -> RequestId -> Text -> Maybe Value -> IO ()

-- Send notification while processing
sendNotification :: ServerContext -> Text -> Maybe Value -> IO ()

-- Send eventual response
sendResponse :: ServerContext -> RequestId -> Value -> IO ()
```

### Message Flow

```haskell
main = runMcpServer serverInfo caps handleRequest handleNotification

handleRequest ctx reqId "tools/call" params = do
  -- Parse params
  sendNotification ctx "tool/started" Nothing  -- immediate update
  -- Do work
  result <- performWork
  sendNotification ctx "tool/progress" (Just progress)  -- progress update
  -- More work
  sendResponse ctx reqId (toJSON result)  -- final response
```

## ghci-mcp Server Architecture

```
ghci-mcp/
├── src/GhciMcp/
│   ├── Process.hs    # ghci process management (start, stop, send, check)
│   ├── Tools.hs      # MCP tool definitions and handlers
│   └── Server.hs     # Main server (ties everything together)
└── app/Main.hs       # Executable entry point
```

### Tool Design

We provide 5 tools for ghci interaction:

1. **start** - Spawn ghci process, store handle in TVar
2. **send** - Write command to ghci stdin, return immediately
3. **listen** - Spawn reader thread, stream output via notifications
4. **stop** - Terminate ghci process
5. **status** - Check if process is running

### Example Session

```json
// 1. Start ghci
→ {"id":1, "method":"tools/call", "params":{"name":"start"}}
← {"id":1, "result":{"status":"started"}}
← {"method":"ghci/started"}  // notification

// 2. Start listening for output
→ {"id":2, "method":"tools/call", "params":{"name":"listen"}}
← {"id":2, "result":{"status":"listening"}}  // returns immediately

// 3. Send command
→ {"id":3, "method":"tools/call", "params":{"name":"send", "arguments":{"command":":t map"}}}
← {"method":"ghci/command_sent", "params":{"command":":t map"}}
← {"id":3, "result":{"status":"sent"}}

// 4. Receive output asynchronously
← {"method":"ghci/output", "params":{"text":"Prelude> ", "stream":"stdout"}}
← {"method":"ghci/output", "params":{"text":"map :: (a -> b) -> [a] -> [b]", "stream":"stdout"}}
← {"method":"ghci/output", "params":{"text":"Prelude> ", "stream":"stdout"}}
```

## Key Insights

### 1. Async is about decoupling, not threads
- Synchronous: Request → block → complete response
- Asynchronous: Request → acknowledge → notifications → eventual response

### 2. JSON-RPC doesn't mandate sync
- Libraries chose sync wrappers for simplicity
- We dropped down to raw JSON-RPC for flexibility

### 3. Notifications are the key primitive
- No `id` field → no response expected
- Can be sent anytime (before, during, after request processing)
- Perfect for streaming output

### 4. Process lifecycle vs. command lifecycle
- Process: long-lived (start → use → stop)
- Commands: ephemeral (send → acknowledge)
- Output: continuous (stream via notifications)

## Advantages Over Synchronous Approach

**Synchronous (would block):**
```haskell
handleToolCall "send" params = do
  hPutStrLn ghciStdin cmd
  output <- hGetLine ghciStdout  -- BLOCKS until output ready
  return $ Result output
```

**Asynchronous (returns immediately):**
```haskell
handleToolCall ctx reqId "send" params = do
  hPutStrLn ghciStdin cmd
  sendResponse ctx reqId (object ["status" .= "sent"])
  -- Output arrives later via 'listen' tool notifications
```

## Future Enhancements

### For mcp-async:
- Server-Sent Events (SSE) transport
- WebSocket transport  
- Better error handling
- Logging framework integration
- Request timeout handling

### For ghci-mcp:
- Multiple concurrent ghci sessions
- Session management (name sessions, switch between them)
- Better prompt detection
- Error stream handling
- File-based output capture
- Integration with card system for persistent logs

## Collaboration Patterns

With this architecture, we can explore:

1. **Agent-orchestrated**: Claude calls tools, decides when to listen, composes output
2. **Human-directed**: User tells Claude "start ghci, run these commands, save output to card"
3. **Hybrid**: Claude explores interactively, human reviews and directs next steps

The async design supports all of these because:
- Tools return immediately (no blocking)
- Output streams independently (via notifications)
- State persists (process lives across tool calls)
- Composition happens at the agent level (not in the server)

## Build Instructions

```bash
cd /home/claude

# Build both packages
cabal build all

# Run ghci-mcp server
cabal run ghci-mcp
```

## Dependencies

**mcp-async:**
- base, aeson, text, bytestring, containers, stm, async

**ghci-mcp:**
- mcp-async, base, aeson, text, process, stm, async

## Files Created

```
/home/claude/
├── cabal.project
├── mcp-async/
│   ├── mcp-async.cabal
│   ├── LICENSE
│   ├── README.md
│   └── src/MCP/Async/
│       ├── Async.hs
│       ├── JsonRpc.hs
│       ├── Protocol.hs
│       └── Server.hs
└── ghci-mcp/
    ├── ghci-mcp.cabal
    ├── LICENSE
    ├── README.md
    ├── app/Main.hs
    └── src/GhciMcp/
        ├── Server.hs
        ├── Tools.hs
        └── Process.hs
```
