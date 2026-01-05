# ghci-mcp

An MCP server that provides asynchronous access to GHCi REPL sessions.

## Overview

`ghci-mcp` is a Model Context Protocol server that lets AI assistants interact with GHCi (Glasgow Haskell Compiler Interactive). Unlike traditional MCP servers that block until a command completes, ghci-mcp uses asynchronous notifications to stream output as it becomes available.

## Architecture

Built on `mcp-async`, which provides:
- Non-blocking request handlers
- Notification streaming while processing
- Persistent process management

## Tools

### `start`
Start a ghci REPL process.

**Returns:** `{"status": "started"}` or `{"status": "already_running"}`

### `send`
Send a command to ghci.

**Parameters:**
- `command` (string): GHCi command to execute (e.g., `:t map`, `:info Functor`)

**Returns:** `{"status": "sent"}`

**Sends notification:** `ghci/command_sent` with the command

### `listen`
Listen for output from ghci. Returns immediately and streams output via notifications.

**Returns:** `{"status": "listening"}`

**Sends notifications:** 
- `ghci/output` with `{"text": "...", "stream": "stdout"}` for each line
- `ghci/stopped` when process exits

### `stop`
Stop the ghci process.

**Returns:** `{"status": "stopped"}` or `{"status": "not_running"}`

**Sends notification:** `ghci/stopped`

### `status`
Check if ghci process is running.

**Returns:** `{"running": true/false}`

## Usage Example

```json
// Start ghci
→ {"jsonrpc":"2.0", "id":1, "method":"tools/call", "params":{"name":"start"}}
← {"jsonrpc":"2.0", "id":1, "result":{"status":"started"}}
← {"jsonrpc":"2.0", "method":"ghci/started"}

// Start listening (returns immediately)
→ {"jsonrpc":"2.0", "id":2, "method":"tools/call", "params":{"name":"listen"}}
← {"jsonrpc":"2.0", "id":2, "result":{"status":"listening"}}

// Send command
→ {"jsonrpc":"2.0", "id":3, "method":"tools/call", "params":{"name":"send", "arguments":{"command":":t map"}}}
← {"jsonrpc":"2.0", "method":"ghci/command_sent", "params":{"command":":t map"}}
← {"jsonrpc":"2.0", "id":3, "result":{"status":"sent"}}

// Receive output via notifications
← {"jsonrpc":"2.0", "method":"ghci/output", "params":{"text":"map :: (a -> b) -> [a] -> [b]", "stream":"stdout"}}
```

## Installation

```bash
cd mcp-async
cabal install

cd ../ghci-mcp
cabal install
```

## Running

```bash
ghci-mcp
```

The server communicates via stdin/stdout using JSON-RPC 2.0.

## Configuration

Add to your MCP client config (e.g., Claude Desktop):

```json
{
  "mcpServers": {
    "ghci": {
      "command": "ghci-mcp"
    }
  }
}
```

## Design Notes

- Uses `mcp-async` for asynchronous notification support
- Maintains a single ghci process per server instance
- Output is streamed via notifications, not returned in responses
- Process lifecycle managed independently of individual commands

## License

MIT
