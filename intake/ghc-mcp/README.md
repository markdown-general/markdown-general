# mcp-async

An asynchronous Model Context Protocol (MCP) server library for Haskell.

## Overview

Unlike synchronous MCP libraries that require handlers to block until a complete response is ready, `mcp-async` allows servers to:

- Send notifications while processing requests
- Respond to requests asynchronously  
- Stream output incrementally
- Maintain long-running processes

## Key Differences from Synchronous MCP

**Synchronous approach:**
```haskell
handleToolCall :: Params -> IO Result
handleToolCall params = do
  output <- runProcess  -- blocks until complete
  return (Result output)
```

**Async approach:**
```haskell
handleToolCall :: ServerContext -> RequestId -> Params -> IO ()
handleToolCall ctx reqId params = do
  sendNotification ctx "process/started" Nothing
  output <- runProcess  -- while running, send updates:
  sendNotification ctx "process/output" (Just output)
  sendResponse ctx reqId (Result "complete")
```

## Architecture

Built on JSON-RPC 2.0, which allows:
- **Requests** (with `id`) → eventually get one **Response** 
- **Notifications** (no `id`) → fire-and-forget, can be sent anytime
- No timing constraints on when responses arrive

## Usage

```haskell
import MCP.Async

main :: IO ()
main = runMcpServer serverInfo caps handleRequest handleNotification
  where
    serverInfo = Implementation "my-server" "1.0.0"
    caps = ServerCapabilities (Just $ ToolsCapability Nothing)
    
    handleRequest ctx reqId "tools/call" (Just params) = do
      -- Parse tool call
      sendNotification ctx "tool/executing" Nothing
      -- Do work, sending notifications as needed
      result <- performWork
      sendResponse ctx reqId (toJSON result)
    
    handleNotification ctx method params =
      -- Handle client notifications
      return ()
```

## Modules

- `MCP.Async` - Main module, re-exports everything
- `MCP.Async.JsonRpc` - JSON-RPC 2.0 types and parsing
- `MCP.Async.Protocol` - MCP protocol types (minimal)
- `MCP.Async.Server` - Async server implementation

## License

MIT
