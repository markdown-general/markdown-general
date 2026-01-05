# ghc-mcp

## broken

The Three Critical Rules

NO stdout pollution - Only writeMessage writes to stdout, only JSON
MCP Content format - All tool responses use mcpTextContent
Server stays alive - Main loop keeps reading stdin, doesn't exit

What Claude Code Must Not Do
❌ putStrLn "anything"
❌ print anything
❌ hPutStrLn stdout anything (except in writeMessage)
❌ Exit the main loop
❌ Return object ["status" .= "ok"] from tools
What Claude Code Must Do
✓ hPutStrLn stderr "debug info" (for logging)
✓ Return mcpTextContent "message" from all tools
✓ Keep server loop running forever
✓ Write only JSON-RPC to stdout via writeMessage

## original message

MCP server providing async access to GHCi REPL sessions for AI assistants.

## What It Does

Runs GHCi as a persistent process, allowing AI assistants to send commands and receive output asynchronously via the Model Context Protocol. Unlike synchronous MCP servers that block until completion, ghc-mcp returns immediately and streams output via notifications.

## Architecture

**Two Haskell packages:**
- **mcp-async** - Generic async MCP server library
- **ghc-mcp** - GHCi-specific server implementation

### The Problem We Solved

Existing Haskell MCP libraries (`mcp`, `mcp-server`) force synchronous patterns:

```haskell
-- Must block until complete result is ready
handleToolCall :: Params -> IO Result
```

This doesn't work for long-running processes like GHCi where we want to:
- Start the process (return immediately)
- Send commands (return acknowledgment, not wait for output)
- Stream output as it arrives (via notifications)

### The Solution: JSON-RPC 2.0 Notifications

JSON-RPC 2.0 spec allows:
- **Requests** (with `id`) → eventually get one **Response**
- **Notifications** (no `id`) → fire-and-forget, sent anytime
- **No timing constraints** on when responses arrive

Built around this notification pattern: requests return immediately, output streams via notifications.

### mcp-async Library

Generic framework for async MCP servers in Haskell.

**Modules:**
```
mcp-async/src/MCP/Async/
├── JsonRpc.hs    # JSON-RPC 2.0 types (Request, Response, Notification)
├── Protocol.hs   # MCP protocol types (Tool, Content, etc.)
├── Server.hs     # Async server (sends/receives messages)
└── Async.hs      # Main module (re-exports + high-level API)
```

**Key types:**
```haskell
-- Request handler - can send notifications before responding
type RequestHandler =
  ServerContext -> RequestId -> Text -> Maybe Value -> IO ()

-- Send notification while processing
sendNotification :: ServerContext -> Text -> Maybe Value -> IO ()

-- Send eventual response
sendResponse :: ServerContext -> RequestId -> Value -> IO ()
```

**Message flow:**
```haskell
handleRequest ctx reqId "tools/call" params = do
  -- Parse params
  sendNotification ctx "tool/started" Nothing  -- immediate update
  -- Do work
  result <- performWork
  sendNotification ctx "tool/progress" (Just progress)  -- progress update
  -- More work
  sendResponse ctx reqId (toJSON result)  -- final response
```

### ghc-mcp Server

**Modules:**
```
ghc-mcp/src/GhciMcp/
├── Process.hs    # ghci process management (start, stop, send, check)
├── Tools.hs      # MCP tool definitions and handlers
└── Server.hs     # Main server (ties everything together)
```

**Tool design:**
1. **start** - Spawn ghci process, store handle in TVar
2. **send** - Write command to ghci stdin, return immediately
3. **listen** - Spawn reader thread, stream output via notifications
4. **stop** - Terminate ghci process
5. **status** - Check if process is running

**Process lifecycle:**
- TVar holds Maybe GhciHandle
- Process is long-lived (start → use → stop)
- Commands are ephemeral (send → acknowledge)
- Output is continuous (stream via notifications)

### Example Message Flow

```json
// 1. Start ghci
→ {"id":1, "method":"tools/call", "params":{"name":"start"}}
← {"id":1, "result":{"content":[{"type":"text","text":"Started GHCi"}]}}
← {"method":"ghci/started"}  // notification

// 2. Start listening for output
→ {"id":2, "method":"tools/call", "params":{"name":"listen"}}
← {"id":2, "result":{"content":[{"type":"text","text":"Listening for GHCi output..."}]}}

// 3. Send command
→ {"id":3, "method":"tools/call", "params":{"name":"send","arguments":{"command":":t map"}}}
← {"method":"ghci/command_sent", "params":{"command":":t map"}}
← {"id":3, "result":{"content":[{"type":"text","text":"Sent command: :t map"}]}}

// 4. Receive output asynchronously
← {"method":"ghci/output", "params":{"text":"Prelude> ", "stream":"stdout"}}
← {"method":"ghci/output", "params":{"text":"map :: (a -> b) -> [a] -> [b]", "stream":"stdout"}}
← {"method":"ghci/output", "params":{"text":"Prelude> ", "stream":"stdout"}}
```

### Key Insights

**Async is about decoupling, not threads:**
- Synchronous: Request → block → complete response
- Asynchronous: Request → acknowledge → notifications → eventual response

**JSON-RPC doesn't mandate sync:**
- Libraries chose sync wrappers for simplicity
- We dropped down to raw JSON-RPC for flexibility

**Notifications are the key primitive:**
- No `id` field → no response expected
- Can be sent anytime (before, during, after request processing)
- Perfect for streaming output

## Tools Available

**start** - Spawn a GHCi process
**stop** - Terminate GHCi
**status** - Check if GHCi is running
**send** - Send command to GHCi (returns immediately)
**listen** - Stream output via notifications

## Installation

```bash
cd ~/repos/ghc-mcp
cabal build all
cabal install ghc-mcp
```

Executable installs to `~/.cabal/bin/ghc-mcp`

## Configuration

Add to Claude Code:

```bash
claude mcp add --transport stdio --scope user ghc-mcp -- ghc-mcp
```

Verify:
```bash
claude mcp list
```

Should show:
```
ghc-mcp: ghc-mcp - ✓ Connected
```

## Usage Example

```
User: "Start GHCi and check the type of map"
Assistant: [Uses start tool]
Assistant: [Uses send tool with ":t map"]
Assistant: [Uses listen tool, receives notifications with output]
Assistant: "map :: (a -> b) -> [a] -> [b]"
```

## Project Location

**Source:** `~/repos/ghc-mcp/`
```
~/repos/ghc-mcp/
├── cabal.project          # Lists both packages
├── mcp-async/            # Generic async MCP library
│   ├── mcp-async.cabal   # Library only
│   └── src/MCP/Async/
└── ghc-mcp/              # GHCi-specific MCP server
    ├── ghc-mcp.cabal     # Library + executable
    ├── src/GhciMcp/      # Library modules
    └── app/Main.hs       # Executable entry point
```

**Documentation:** `~/sisyphus/intake/ghc-mcp/`
- `ARCHITECTURE.md` - Design decisions (consolidated here)
- `BUILD-LEARNINGS.md` - Build notes (consolidated here)
- `MCP-CONTENT-UPDATE.md` - Content format fix (consolidated here)
- `NOTIFICATION-TEST.md` - Historical test observations

## Build Notes

**Language:** GHC2024
**Compiler:** GHC 9.12.2

### Multi-Package Setup

**cabal.project:**
```
packages:
  mcp-async
  ghc-mcp

write-ghc-environment-files: always
```

The `write-ghc-environment-files` setting ensures ghcid can find dependencies.

**Build commands:**
```bash
cd ~/repos/ghc-mcp

# Build both packages
cabal build all

# Build specific package
cabal build mcp-async
cabal build ghc-mcp

# Run executable
cabal run ghc-mcp

# Install executable to ~/.cabal/bin
cabal install ghc-mcp
```

### Package Organization

**mcp-async:**
- Library only (no executable, no tests initially)
- Generic framework for async MCP servers
- Exposed modules: MCP.Async, MCP.Async.JsonRpc, MCP.Async.Protocol, MCP.Async.Server
- Dependencies: base, aeson, text, bytestring, containers, stm, async

**ghc-mcp:**
- Both library and executable
- Library: GhciMcp.Server, GhciMcp.Tools, GhciMcp.Process
- Executable: ghc-mcp (Main.hs just calls runGhciMcpServer)
- Depends on mcp-async library
- Additional dependencies: process (for spawning ghci)
- Executable gets -threaded flag for concurrent operations

### Module Organization

**mcp-async modules:**
- JsonRpc: JSON-RPC 2.0 types (Request with id, Notification without id, Response, ErrorObject)
- Protocol: MCP protocol types (Tool, ServerCapabilities, Implementation, Content)
- Server: Server runtime (sendNotification, sendResponse, runMcpServer, ServerContext)
- Async: Main module that re-exports everything for convenience

**ghc-mcp modules:**
- Process: GHCi process lifecycle (start, stop, send commands, read output, check status)
- Tools: 5 MCP tools (start, stop, status, send, listen) and their handlers
- Server: Top-level (runGhciMcpServer, request routing, initialize/tools/list/tools/call)

### Implementation Patterns

**Async via notifications:**
- Requests (with id) get one eventual Response
- Notifications (no id) sent anytime during processing
- Tools return immediately, stream output via notifications
- ServerContext provides sendNotification/sendResponse

**Process management:**
- TVar holds Maybe GhciHandle for shared state
- Process lifecycle independent of individual commands
- Non-blocking reads via hReady
- Background thread for listen loop

**Tool handlers:**
- start/stop: manage process lifecycle
- status: check if running
- send: write command to stdin, acknowledge immediately
- listen: spawn background thread that streams output via notifications

### Common Issues

**Import organization:**
- Put imports at top, not scattered
- Avoid duplicate imports

**Qualified imports:**
- Use `qualified as` for name collisions (Data.Aeson.KeyMap)
- Pattern for text: `Data.Text qualified as T`, `Data.Text.IO qualified as TIO`

**aeson KeyMap:**
- Object lookups need Data.Aeson.KeyMap.toList for pattern matching
- Remember to import qualified

**Process handling:**
- Set buffering modes (LineBuffering for stdin, NoBuffering for JSON-RPC)
- Set binary mode for JSON-RPC communication
- Use hReady for non-blocking reads

**Type issues:**
- ByteString.Lazy.Char8 doesn't export hGetLine - use ByteString.Char8.hGetLine
- Object pattern matching in GHC2024 - use Data.Aeson.KeyMap.lookup directly
- Missing FromJSON instance for Notification

### Patterns That Worked

**Multi-package with internal dependency:**
- cabal.project lists both packages
- ghc-mcp just adds mcp-async to build-depends
- Cabal figures out build order

**Library + executable split:**
- Most logic in library modules
- Executable is minimal entry point
- Easier to test library separately later

**Re-export module:**
- MCP.Async re-exports sub-modules
- Users can `import MCP.Async` for everything
- Or import specific modules for clarity

## Critical Fixes

Two major bugs were discovered and fixed during development.

### Fix 1: stdout Pollution (2026-01-04)

**Problem:**
MCP servers must ONLY write JSON-RPC to stdout. Initial version polluted stdout with debug messages:

```haskell
putStrLn "Starting ghci-mcp server..."  -- ❌ Pollutes stdout
```

This corrupted the JSON-RPC stream and caused Claude Code to hang while trying to parse non-JSON text.

**The fix:**
Moved all logging to stderr:

```haskell
-- Before
putStrLn "Starting ghci-mcp server..."

-- After
hPutStrLn stderr "Starting ghci-mcp server..."  -- ✓ Logs to stderr
```

**Why this fixes it:**
```
MCP Protocol:
- stdin: Receive JSON-RPC requests
- stdout: Send JSON-RPC responses (ONLY)
- stderr: Optional logging
```

**Files changed:**
- `ghci-mcp/src/GhciMcp/Server.hs` - Removed startup message, changed notification logging
- `ghci-mcp/src/GhciMcp/Process.hs` - Changed error logging (2 locations)

**Test:**
```bash
echo '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2024-11-05","clientInfo":{"name":"test","version":"1.0"}}}' | ghc-mcp 2>/dev/null | head -1 | python3 -m json.tool
```

Should output valid JSON only, NOT "Starting ghci-mcp server..."

**See:** `~/sisyphus/intake/ghc-mcp-pollution/STDOUT-POLLUTION-FIX.md`

### Fix 2: MCP Content Format (2026-01-04)

**Problem:**
All ghci-mcp tools were returning raw JSON objects instead of proper MCP Content format:

```haskell
sendResponse ctx reqId $ object ["status" .= ("started" :: Text)]  -- ❌ Wrong format
```

This caused Claude Code to report "Tool ran without output or errors" - no messages visible.

**The fix:**
Return MCP-compliant Content objects:

```haskell
-- Helper function
mcpTextContent :: Text -> Value
mcpTextContent msg = object
  [ "content" .=
      [ object
          [ "type" .= ("text" :: Text)
          , "text" .= msg
          ]
      ]
  ]

-- Usage
sendResponse ctx reqId $ mcpTextContent "Started GHCi"  -- ✓ Proper MCP format
```

**MCP Content format:**
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

**Tool responses updated:**
- **start** - "Started GHCi" or "GHCi is already running"
- **send** - "Sent command: <cmd>"
- **listen** - "Listening for GHCi output..."
- **stop** - "Stopped GHCi" or "GHCi is not running"
- **status** - "GHCi is running" / "GHCi is not running" / "GHCi process has terminated"

**Files changed:**
- `ghci-mcp/src/GhciMcp/Tools.hs` - All tool handlers updated

**Async flow preserved:**
`listen` still works asynchronously:
1. Returns MCP Content immediately: "Listening for GHCi output..."
2. Spawns background thread to read stdout
3. Sends `ghci/output` notifications as data arrives
4. Valid JSON-RPC: Response has id, Notifications don't

**See:** `~/sisyphus/intake/ghc-mcp/MCP-CONTENT-UPDATE.md`

## Status

**Installation:** ✓ Installed to ~/.cabal/bin/ghc-mcp
**Configuration:** ✓ Added to Claude Code user config
**Connection:** ✓ Connected and ready
**Critical Fixes:** ✓ Both applied (stdout pollution + content format)
**Testing:** ⚠ In progress - need systematic tool testing

**Test Plan:**
- [ ] start - Verify GHCi process spawns
- [ ] status - Check running state detection
- [ ] send - Test command execution
- [ ] listen - Verify notification streaming (may not be visible in Claude Code)
- [ ] stop - Confirm clean shutdown

Need to test each tool from beginning to verify:
1. Tools return proper MCP Content (should work after fix 2)
2. Async notification pattern works correctly (notifications may not be surfaced by Claude Code)

## Known Issues

**Notification visibility in Claude Code:**
- Claude Code may not surface MCP notifications to the assistant
- `listen` tool sends `ghci/output` notifications but these may not be visible
- Tool responses are visible (after content format fix)
- This is a Claude Code limitation, not a ghc-mcp bug

**iterm-mcp incompatibility:**
- The iterm-mcp MCP server doesn't work properly for testing ghc-mcp
- `mcp__iterm-mcp__write_to_terminal` writes to Claude's running terminal, not the target iTerm window
- Cannot be used to test ghc-mcp interactively

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

## Future Enhancements

**For mcp-async:**
- Server-Sent Events (SSE) transport
- WebSocket transport
- Better error handling
- Logging framework integration
- Request timeout handling

**For ghc-mcp:**
- Multiple concurrent ghci sessions
- Session management (name sessions, switch between them)
- Better prompt detection
- Error stream handling
- File-based output capture
- Integration with card system for persistent logs
- Synchronous alternatives (`send_sync`, `read`) for clients without notification support

## Last Updated

2026-01-04
