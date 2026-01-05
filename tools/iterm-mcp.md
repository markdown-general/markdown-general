# iterm-mcp.md

> **⚠️ DO NOT USE ⚠️**
>
> This MCP server should not be used. Prefer alternative approaches.

Model Context Protocol server for iTerm2 terminal control - enables AI to read/write terminal, send control characters, interact with REPLs.

## what it does

**Terminal Access** ⟜ AI can control your active iTerm2 terminal session

**Three Tools**:
- `write_to_terminal` ⟜ write text/commands, returns line count of output
- `read_terminal_output` ⟜ read N lines from terminal  
- `send_control_character` ⟜ send ctrl-c, ctrl-z, etc.

**Use Cases**:
- REPL interaction (ghci, python, node)
- Command execution with feedback
- Terminal debugging and troubleshooting
- Task delegation with real-time visibility

**Token Efficient** ⟜ read only what you need, not entire scrollback

## installation

Verify npm is available:
```bash
npm --version
npx --version
```

Test iterm-mcp runs:
```bash
npx -y iterm-mcp
```

This should start the MCP server (it will wait for JSON-RPC messages on stdin).
Press ctrl-c to exit.

## configuration

### For Claude Desktop

Edit: `~/Library/Application Support/Claude/claude_desktop_config.json`

Add iterm-mcp server:
```json
{
  "mcpServers": {
    "iterm-mcp": {
      "command": "npx",
      "args": ["-y", "iterm-mcp"]
    }
  }
}
```

Restart Claude Desktop to load the configuration.

### For Claude Code

**Configuration Scopes:**

Claude Code supports three MCP server scopes:
- **User scope (global):** `~/.claude.json` - available across all projects
- **Project scope (shareable):** `.mcp.json` at project root - can be committed to git
- **Local scope (private):** `.claude.json` at project root - git-ignored

**Installation Steps:**

1. Edit `~/.claude.json` (recommended for personal tools like iterm-mcp)
2. Add the `mcpServers` configuration if not present
3. Add iterm-mcp server definition:

```json
{
  "mcpServers": {
    "iterm-mcp": {
      "command": "npx",
      "args": ["-y", "iterm-mcp"]
    }
  }
}
```

4. Restart Claude Code for changes to take effect

**Verification:**

After restart, check that iterm-mcp tools are available:
- Open a new Claude Code session
- The MCP server should auto-connect on startup
- Check `/mcp` command to see iterm-mcp status
- Three tools should be available: `write_to_terminal`, `read_terminal_output`, `send_control_character`

## verification

After configuring Claude Desktop:

1. Open iTerm2
2. Start Claude Desktop
3. Check that iterm-mcp appears in available tools
4. Test: "Write 'echo hello' to the terminal and read the output"

Expected: Claude uses write_to_terminal, then read_terminal_output, shows you "hello"

## usage patterns

### REPL Interaction

Start ghci in iTerm:
```bash
ghci
```

Then ask Claude:
- "What's the type of map?"
- "Evaluate foldr (+) 0 [1,2,3]"
- "Load the Main module"

Claude will write commands to ghci and read responses.

### Project Context

Load a cabal/stack project first:
```bash
cd ~/sisyphus
cabal repl
```

Then Claude has access to your project modules and can query types, test functions, etc.

### Token Management

For long outputs, ask Claude to read incrementally:
- "Read the first 10 lines of output"
- "Is there more? Read another 10 lines"
- "Skip to the end, read last 5 lines"

This avoids dumping large outputs into context.

## safety considerations

**No Restrictions** ⟜ iterm-mcp executes whatever you ask Claude to write

**User Responsibility**:
- Monitor the terminal to see what's happening
- Interrupt (ctrl-c in chat) if Claude goes off track
- Start with small tasks to understand behavior

**Visibility** ⟜ you see everything in the terminal in real-time

## limitations

**macOS Only** ⟜ requires iTerm2, uses AppleScript API

**Single Terminal** ⟜ controls the active iTerm tab only

**No Process Management** ⟜ doesn't spawn/manage processes, just controls existing terminal

## status

**Installation:** ✓ npx available, iterm-mcp server tested
**Configuration:** ✓ added to ~/.claude.json (user scope)
**Hooks:** ✓ auto-approval configured for all three iterm-mcp tools
**Tests:** ✓ passed - write/read and ghci REPL interaction verified

**Last updated:** 2026-01-03

**Configuration verified:**
- npm/npx version: 11.2.0
- iterm-mcp server: launches successfully via `npx -y iterm-mcp`
- Claude Code config: added to `~/.claude.json` at user scope
- Hooks: configured in `~/.claude/hooks.json` with auto-approval

**Test Results (2026-01-03):**
- ✓ `write_to_terminal`: successfully wrote command to active iTerm terminal
- ✓ `read_terminal_output`: successfully read terminal output (N lines configurable)
- ✓ ghci REPL interaction: tested with `putStrLn` command, executed and returned output correctly
- ✓ hooks auto-approval: iterm-mcp tools approved automatically per configuration

**Working Patterns:**
- Terminal commands execute successfully with feedback loop
- ghci REPL accepts Haskell expressions via write_to_terminal
- read_terminal_output provides efficient incremental reading without context bloat
- Tool chain: write → read → parse → respond works reliably
