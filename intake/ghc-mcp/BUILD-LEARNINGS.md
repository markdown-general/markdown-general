# Build Learnings: ghc-mcp Project

First traditional Haskell project build together.

## Project Structure

**Multi-package setup:**
```
~/repos/ghc-mcp/
├── cabal.project          # Lists both packages
├── mcp-async/            # Generic async MCP library
│   ├── mcp-async.cabal   # Library only
│   └── src/MCP/Async/
└── ghc-mcp/              # GHCi-specific MCP server
    ├── ghc-mcp.cabal     # Library + executable
    ├── src/GhcMcp/       # Library modules
    └── app/Main.hs       # Executable entry point
```

## Package Decisions

**mcp-async:**
- Library only (no executable, no tests initially)
- Generic framework for async MCP servers
- Exposed modules: MCP.Async, MCP.Async.JsonRpc, MCP.Async.Protocol, MCP.Async.Server
- Dependencies: base, aeson, text, bytestring, containers, stm, async

**ghc-mcp:**
- Both library and executable
- Library: GhcMcp.Server, GhcMcp.Tools, GhcMcp.Process
- Executable: ghc-mcp (Main.hs just calls runGhcMcpServer)
- Depends on mcp-async library
- Additional dependencies: process (for spawning ghci)
- Executable gets -threaded flag for concurrent operations

## Module Organization

**mcp-async modules:**
- JsonRpc: JSON-RPC 2.0 types (Request with id, Notification without id, Response, ErrorObject)
- Protocol: MCP protocol types (Tool, ServerCapabilities, Implementation, Content)
- Server: Server runtime (sendNotification, sendResponse, runMcpServer, ServerContext)
- Async: Main module that re-exports everything for convenience

**ghc-mcp modules:**
- Process: GHCi process lifecycle (start, stop, send commands, read output, check status)
- Tools: 5 MCP tools (start, stop, status, send, listen) and their handlers
- Server: Top-level (runGhcMcpServer, request routing, initialize/tools/list/tools/call)

## Key Implementation Patterns

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

## cabal.project Configuration

```
packages:
  mcp-async
  ghc-mcp

write-ghc-environment-files: always
```

The write-ghc-environment-files setting ensures ghcid can find dependencies.

## Build Commands

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

## Next Steps for Testing

**Manual testing:**
- Run `cabal run ghc-mcp` to start server
- Feed JSON-RPC messages via stdin
- Observe responses on stdout

**With ghcid:**
- `ghcid --command="cabal repl ghc-mcp" --outputfile=ghcid.txt`
- Watch for type errors in ghcid.txt

**Integration testing:**
- Configure Claude Desktop to use ghc-mcp server
- Test tool calls through MCP client

## Common Issues to Watch For

**Import organization:**
- Put imports at top, not scattered
- Avoid duplicate imports (forkIO appeared twice initially)

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

## Patterns That Worked

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

## File Creation Order

1. Directory structure (mkdir -p)
2. .cabal files (dependencies, modules, executables)
3. Implementation modules (bottom-up: JsonRpc → Protocol → Server)
4. Re-export module (MCP.Async)
5. Application modules (Process → Tools → Server)
6. Executable entry point (Main.hs)
7. cabal.project

This order means dependencies exist when referenced.
