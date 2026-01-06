# claude.md

**State discovery** ⟜ understand Claude's entry configuration, global settings, project overrides
**Configuration layers** ⟜ global ~/.claude → project ~/.claude → permissions, settings, hooks
**Entry ritual** ⟜ system context, user instructions (CLAUDE.md), then project discovery
**Ephemerality** ⟜ state is dynamic; processes and users can mutate configurations between sessions
**Caching problem** ⟜ configuration changed without awareness; need entry-point validation

## Statement

When Claude Code (the CLI tool) instantiates in ~/sisyphus/, it reads configuration from multiple layered sources. This card documents the discovery process, configuration file locations, permission structures, and how to validate state at entry.

The problem: Multiple actors (user, processes, other Claude sessions) can modify configuration files. When you return to a project, you never know what state you're entering without explicit inspection.

Solution: Document the complete entry ritual so you can cache expectations and validate assumptions.

## Main examples

**Check current state on entry:**
```bash
# 1. Global configuration
cat ~/.claude/settings.json              # Model, plugins, status line
cat ~/.claude/settings.local.json        # Global permissions whitelist
cat ~/.claude/hooks.json                 # Global hook handlers

# 2. Project overrides
cat ~/sisyphus/.claude/settings.local.json  # Project-specific permissions

# 3. Active configuration
claude mcp list                          # Show active MCP servers
```

**Understand what changed:**
```bash
# Check git history of settings
git -C ~/sisyphus log -p --follow -- .claude/settings.local.json | head -50
git -C ~ log -p --follow -- .claude/settings.json | head -50
```

**Validate MCP server state:**
```bash
claude mcp list  # Shows: name, command, connection status (✓ or ✗)
```

## Configuration layers

**Global layer** (~/.claude/):
- `settings.json` — model choice, enabled plugins, status line command
- `settings.local.json` — global permissions whitelist
- `hooks.json` — global hook handlers (pre-submit, post-tool, etc.)
- `plugins/` — installed plugins and skills
- `stats-cache.json` — cache data (generated, not config)

**Project layer** (~/sisyphus/.claude/):
- `settings.local.json` — project-specific permissions whitelist (overrides/extends global)
- `.claude-plugin/` — local plugin definitions
- `.mcp.json` files — MCP server declarations (if present)

**User instructions** (~/.claude/CLAUDE.md):
- Global system prompt applied to all projects
- Read at entry time
- Affects behavior across all sessions

**Project user instructions** (~/sisyphus/CLAUDE.md or .claude/CLAUDE.md):
- Project-specific system prompt
- Overrides or augments global instructions
- Not currently observed in sisyphus/

## Entry ritual on instantiation

**1. Context assembly** (system provided):
- Working directory: /Users/tonyday567/sisyphus
- Git repo status snapshot
- Today's date, platform, environment
- Model info (Haiku 4.5 as of this session)

**2. User instructions read** (in order):
```
~/.claude/CLAUDE.md  [if exists]
~/sisyphus/.claude/CLAUDE.md  [if exists]
~/sisyphus/CLAUDE.md  [if exists]
```

Current state: `~/.claude/CLAUDE.md` exists and applies globally:
- Optimized for ~/sisyphus/ knowledge base
- States sisyphus is the prime knowledge directory

**3. Configuration assembly**:
- Load ~/.claude/settings.json (model, plugins)
- Load ~/.claude/settings.local.json (permissions)
- Load ~/sisyphus/.claude/settings.local.json (project overrides)
- Merge permissions: project layer extends/overrides global

**4. Capabilities available**:
- Only tools in merged permissions whitelist are available
- MCP servers declared via command/configuration are instantiated
- Hooks fire at tool-execution boundaries

**5. State uncertainty**:
- Hooks can be user-defined shell commands that reject tool use
- Tools can be denied by missing permissions
- MCP servers can fail to connect without clear error propagation
- Configuration can change between sessions without notification

## Configuration map

### Global settings (~/.claude/settings.json)
```json
{
  "model": "sonnet",                          // Model choice
  "statusLine": {
    "type": "command",
    "command": "npx -y ccusage@latest statusline"
  },
  "enabledPlugins": {
    "hls@claude-hls": true                    // HLS (Haskell Language Server)
  }
}
```

### Global permissions (~/.claude/settings.local.json)
Array of allowed Bash commands, WebSearch, WebFetch with domain filters:
```json
"allow": [
  "Bash(gcc --version:*)",
  "Bash(brew install:*)",
  "WebSearch",
  "WebFetch(domain:github.com)"
]
```

### Project permissions (~/sisyphus/.claude/settings.local.json)
```json
{
  "permissions": {
    "allow": [
      "mcp__iterm-mcp__write_to_terminal",    // iTerm integration
      "mcp__iterm-mcp__read_terminal_output",
      "Bash(chmod:*)",
      "Bash(cabal install:*)",
      "Bash(card-api install:*)",
      "Bash(cache --help:*)"
    ]
  }
}
```

### Hooks (~/.claude/hooks.json)
Shell commands executed at tool-use boundaries:
- `user-prompt-submit-hook` — before processing user input
- Pre-tool hooks — before specific tool execution
- Post-tool hooks — after tool execution

## API: Checking your state

**Language:**
- System calls: Bash, MCP protocol
- Format: JSON (settings), Markdown (instructions)

**Input:**
```bash
# Query current settings
cat ~/.claude/settings.json
cat ~/.claude/settings.local.json
cat ~/sisyphus/.claude/settings.local.json

# Query active servers
claude mcp list

# Query user instructions
cat ~/.claude/CLAUDE.md
```

**Output:**
```
iterm-mcp: npx -y iterm-mcp - ✓ Connected
ghc-mcp: ghc-mcp  - ✓ Connected    [or] ✗ Failed: error message
```

**Contracts:**
- Settings are JSON; valid UTF-8
- Permissions arrays are whitelist (explicit allow)
- MCP servers in `claude mcp list` output show connection status immediately
- User instructions are applied once at entry

## Installation

No installation needed—this card documents existing behavior.

To validate state at entry:
```bash
# Save this as validate-claude-state.sh in artifacts/bin/
#!/bin/bash
echo "=== Global settings ==="
cat ~/.claude/settings.json
echo ""
echo "=== Global permissions ==="
cat ~/.claude/settings.local.json
echo ""
echo "=== Project overrides ==="
cat ~/sisyphus/.claude/settings.local.json
echo ""
echo "=== Active MCP servers ==="
claude mcp list
echo ""
echo "=== User instructions ==="
[ -f ~/.claude/CLAUDE.md ] && echo "Found: ~/.claude/CLAUDE.md" || echo "None: ~/.claude/CLAUDE.md"
```

## Tips

**Configuration is mutable:**
- Hooks can reject tool use with error message
- Permissions can be missing (tool denied silently)
- MCP servers can fail to start; failure mode unclear

**When tools fail mysteriously:**
1. Check permissions.allow in ~/.claude/settings.local.json and project override
2. Run `claude mcp list` to verify MCP server connection
3. Check hooks.json for rejecting hooks
4. Verify tool isn't running in background already (use `claude tasks`)

**MCP server failures:**
- ghc-mcp, gpt4-mcp, etc. can fail due to missing dependencies or bad configuration
- Failure is silent until you try to use a tool that requires that server
- Remove from .claude/settings.local.json and restart Claude Code

**Ephemeral state:**
- Don't assume configuration hasn't changed between sessions
- Always validate permissions and MCP server state on critical operations
- Git history of .claude/ files shows who changed what when

## Status

**Validation:** ✓ Complete — documented as of 2025-01-06
**MCP servers in sisyphus:** Previously had ghc-mcp (now removed), currently running iterm-mcp
**Global model:** Haiku 4.5 (claude-haiku-4-5-20251001)
**Frontier model:** Claude Opus 4.5 (claude-opus-4-5-20251101)

## Code: State inspection template

```bash
#!/bin/bash
# claude-state.sh — inspect Claude entry configuration

set -e

GLOBAL_SETTINGS=~/.claude/settings.json
GLOBAL_PERMS=~/.claude/settings.local.json
GLOBAL_HOOKS=~/.claude/hooks.json
PROJECT_PERMS=./claude/.claude/settings.local.json  # Relative to project
USER_INSTRUCTIONS=~/.claude/CLAUDE.md

echo "Claude Code State Inspection"
echo "============================"
echo ""

# Global configuration
echo "GLOBAL SETTINGS:"
if [ -f "$GLOBAL_SETTINGS" ]; then
  jq . "$GLOBAL_SETTINGS"
else
  echo "NOT FOUND: $GLOBAL_SETTINGS"
fi
echo ""

echo "GLOBAL PERMISSIONS:"
if [ -f "$GLOBAL_PERMS" ]; then
  jq .permissions.allow "$GLOBAL_PERMS" | head -20
else
  echo "NOT FOUND: $GLOBAL_PERMS"
fi
echo ""

echo "GLOBAL HOOKS:"
if [ -f "$GLOBAL_HOOKS" ]; then
  jq 'keys' "$GLOBAL_HOOKS"
else
  echo "NOT FOUND: $GLOBAL_HOOKS"
fi
echo ""

# Project overrides
echo "PROJECT PERMISSIONS:"
if [ -f "$PROJECT_PERMS" ]; then
  jq .permissions.allow "$PROJECT_PERMS"
else
  echo "NOT FOUND: $PROJECT_PERMS"
fi
echo ""

# User instructions
echo "USER INSTRUCTIONS:"
if [ -f "$USER_INSTRUCTIONS" ]; then
  wc -l "$USER_INSTRUCTIONS"
  head -5 "$USER_INSTRUCTIONS"
else
  echo "NONE: ~/.claude/CLAUDE.md"
fi
echo ""

# Active servers
echo "ACTIVE MCP SERVERS:"
claude mcp list || echo "claude mcp list command failed"
echo ""

# Summary
echo "PERMISSION ANALYSIS:"
GLOBAL_COUNT=$(jq '.permissions.allow | length' "$GLOBAL_PERMS" 2>/dev/null || echo 0)
PROJECT_COUNT=$(jq '.permissions.allow | length' "$PROJECT_PERMS" 2>/dev/null || echo 0)
echo "Global permissions: $GLOBAL_COUNT"
echo "Project additions: $PROJECT_COUNT"
```

## Examples: Recovering from configuration drift

**Scenario: You don't know if a tool was disabled by a hook**

```bash
# Check what happened to ghc-mcp
grep -r "ghc-mcp" ~/.claude/settings*.json  # Global layer
grep -r "ghc-mcp" .claude/settings*.json    # Project layer

# If found, verify it's still connected
claude mcp list | grep ghc-mcp

# If missing or failed, decision: reinstall or remove
# (In sisyphus/, ghc-mcp was removed in this session)
```

**Scenario: Tool silently denied**

```bash
# Tool was denied → check permissions
cat ~/.claude/settings.local.json | jq '.permissions.allow[]' | grep "YourTool"

# Not found → tool is blacklisted
# Solution: Add to permissions or use different tool
```

**Scenario: git history shows who changed what**

```bash
# See configuration evolution
git -C ~ log --oneline -- .claude/settings.local.json

# See details of last change
git -C ~ show HEAD -- .claude/settings.local.json

# See all changes in a tool removal
git -C ~/sisyphus show HEAD -- .claude/settings.local.json
```

## Tests

**Test 1: Configuration files are valid JSON**
```bash
jq empty ~/.claude/settings.json
jq empty ~/.claude/settings.local.json
jq empty ~/sisyphus/.claude/settings.local.json
echo "✓ All configuration files are valid JSON"
```

**Test 2: MCP servers respond**
```bash
SERVERS=$(claude mcp list | grep -c "✓ Connected" || echo 0)
echo "✓ $SERVERS MCP servers connected"
```

**Test 3: User instructions exist and are readable**
```bash
[ -f ~/.claude/CLAUDE.md ] && echo "✓ Global instructions found" || echo "✗ No global instructions"
```

## Relations

**Related cards:**
- **card.md** — Execution framework for literate tools
- **coding.md** — Code structure standards
- **haskell-api.md** — Haskell tool processor
- **python.md** — Python tool processor

**Related configuration:**
- ~/.claude/ — Global Claude Code settings (system-wide)
- ~/sisyphus/.claude/ — Project-specific overrides
- git history — Configuration evolution audit trail

**Related concepts:**
- **MCP servers** — Language Server Protocol tools (ghc-mcp, iterm-mcp, etc.)
- **Hooks** — Shell handlers at tool execution boundaries
- **Permissions** — Whitelist-based tool access control
- **System context** — Environment, working directory, model info provided at entry
