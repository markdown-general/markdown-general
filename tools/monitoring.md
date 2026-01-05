# content/base/tools/monitoring.md

**safety rating** ⟜ **unsafe** (runaway processes, token overconsumption)

Claude Code usage monitoring and strict rate control.

**Problem** ⟜ monthly limit consumed in 3 hours
**Need** ⟜ hard stops, real-time visibility, predictive warnings
**Solution** ⟜ layered monitoring with automatic circuit breakers

## Quick Reference

**Morning ritual** (before starting work)
```bash
ccusage blocks                # Check available budget
tmux new-session -d -s claude-monitor 'npx ccusage@latest blocks --live'
claude-safe                   # Start with protection
```

**During session**
- Glance at status line constantly (updates every few seconds)
- `/usage` to check within Claude Code
- `/compact` every 5 messages when burn rate climbs
- `/clear` for fresh start on new task
- `tmux attach -t claude-monitor` to peek at live monitor (Ctrl+B, D to detach)

**End of session**
```bash
ccusage daily                 # Review consumption
tmux kill-session -t claude-monitor
```

**Weekly review**
```bash
ccusage blocks               # See weekly trends
ccusage monthly              # See monthly patterns
```

**Threshold override** (emergency only)
```bash
CLAUDE_BUDGET_THRESHOLD=95 claude-safe              # One-time
echo 'export CLAUDE_BUDGET_THRESHOLD=90' >> ~/.bashrc  # Permanent
```

## Monitoring Layers

**Built-in visibility** ⟜ `/usage` command shows current consumption
**Status line** ⟜ persistent display at bottom of Claude Code interface
**Real-time tracking** ⟜ ccusage blocks --live for burn rate analysis  
**Hard limits** ⟜ claude-safe wrapper enforces session budgets
**Predictive warnings** ⟜ alert at 50%, 75%, 90% thresholds
**Session isolation** ⟜ one task per reset cycle, no background runs

## Circuit Breaker Pattern

**Token budget**
  ⟜ exceeding daily/session allocation
  ⟝ hard stop at configured threshold
  
**Burn rate**
  ⟜ consumption velocity predicts early depletion
  ⟝ warning at projected 80% usage before reset

**Weekly cap**
  ⟜ Pro plan 40-80h Sonnet 4 limit
  ⟝ reserve 20% for final week days

**Context bloat**
  ⟜ large context files multiply token cost per message
  ⟝ `/compact` every 5 messages, `/clear` between tasks

## Status Line

The status line is a persistent display at the bottom of Claude Code showing:

**💬 Current session cost** ⟜ what you're spending this conversation  
**💰 Today's total** ⟜ cumulative spending for the day  
**🚀 Current block** ⟜ cost and time remaining in 5-hour window  
**🔥 Burn rate** ⟜ how fast you're consuming tokens  
**🤖 Active model** ⟜ which Claude model you're using

Updates automatically every few seconds - constant visibility without interrupting flow.

## Code

```bash setup
#!/bin/bash
# One-time setup for Claude Code monitoring with hard limits

set -e

echo "🔧 Setting up Claude Code monitoring..."

# 1. Test ccusage availability
echo ""
echo "📊 Testing ccusage..."
if command -v npx &> /dev/null; then
    npx ccusage@latest blocks
    echo "✅ ccusage working"
else
    echo "❌ npx not found - install Node.js first"
    exit 1
fi

# 2. Create wrapper script
echo ""
echo "🛡️  Creating claude-safe wrapper..."

WRAPPER_PATH="${HOME}/sisyphus/artifacts/bin/claude-safe"
mkdir -p "$(dirname "$WRAPPER_PATH")"

cat > "$WRAPPER_PATH" << 'EOF'
#!/bin/bash
# Claude Code with hard budget enforcement

THRESHOLD=${CLAUDE_BUDGET_THRESHOLD:-80}
PLAN=${CLAUDE_PLAN:-pro}

# Check current usage via ccusage
CURRENT_USAGE=$(npx -y ccusage@latest blocks --json 2>/dev/null | jq -r '.[-1].usage_percent // 0' 2>/dev/null || echo "0")

if (( $(echo "$CURRENT_USAGE > $THRESHOLD" | bc -l 2>/dev/null || echo "0") )); then
    echo "❌ Budget threshold exceeded: ${CURRENT_USAGE}% > ${THRESHOLD}%"
    echo ""
    echo "Options:"
    echo "  1. Wait for 5-hour reset"
    echo "  2. Set higher threshold: export CLAUDE_BUDGET_THRESHOLD=90"
    echo "  3. Override once: CLAUDE_BUDGET_THRESHOLD=100 claude-safe"
    echo ""
    npx -y ccusage@latest blocks
    exit 1
fi

echo "✅ Budget check passed: ${CURRENT_USAGE}% of ${THRESHOLD}% threshold"
echo ""

# Launch claude
exec claude "$@"
EOF

chmod +x "$WRAPPER_PATH"
echo "✅ Wrapper created at $WRAPPER_PATH"

# 3. Setup status line config
echo ""
echo "📈 Status line configuration..."

CLAUDE_CONFIG="${HOME}/.claude/settings.json"
if [ -f "$CLAUDE_CONFIG" ]; then
    echo "⚠️  ~/.claude/settings.json already exists"
    echo "   Add this manually:"
else
    mkdir -p "$(dirname "$CLAUDE_CONFIG")"
    cat > "$CLAUDE_CONFIG" << 'EOF'
{
  "statusLine": {
    "type": "command",
    "command": "npx -y ccusage@latest statusline",
    "padding": 0
  }
}
EOF
    echo "✅ Status line configured"
fi

cat << 'EOF'

{
  "statusLine": {
    "type": "command",
    "command": "npx -y ccusage@latest statusline",
    "padding": 0
  }
}
EOF

# 4. Setup tmux monitoring session
echo ""
echo "🖥️  Tmux monitoring commands..."
cat << 'EOF'

# Start monitoring session:
tmux new-session -d -s claude-monitor 'npx ccusage@latest blocks --live'

# View it anytime:
tmux attach -t claude-monitor

# Kill when done:
tmux kill-session -t claude-monitor
EOF

# 5. Add to PATH if needed
echo ""
echo "🔧 PATH setup..."
if [[ ":$PATH:" != *":$HOME/sisyphus/artifacts/bin:"* ]]; then
    echo "⚠️  Add to your ~/.bashrc or ~/.zshrc:"
    echo "   export PATH=\"\$HOME/sisyphus/artifacts/bin:\$PATH\""
else
    echo "✅ PATH already includes sisyphus/artifacts/bin"
fi

echo ""
echo "✅ Setup complete!"
echo ""
echo "Usage:"
echo "  claude-safe              # Start Claude Code with budget check"
echo "  CLAUDE_BUDGET_THRESHOLD=90 claude-safe  # Custom threshold"
echo "  ccusage blocks          # Check current usage"
echo "  ccusage blocks --live   # Real-time monitoring"
echo "  ccusage daily           # Daily usage report"
echo ""
echo "Environment variables:"
echo "  CLAUDE_BUDGET_THRESHOLD  # Default: 80 (percent)"
echo "  CLAUDE_PLAN             # Default: pro"
```

## Run

**Extract and run setup script:**
```bash
# From monitoring.md, extract the setup block
# Save to ~/sisyphus/setup-monitoring.sh
# Or extract with card-api when that's ready

bash setup-monitoring.sh
```

**Manual installation:**
```bash
# 1. Test ccusage
npx ccusage@latest blocks

# 2. Create wrapper at ~/sisyphus/artifacts/bin/claude-safe
#    (see Code section above for full script)

# 3. Add status line to ~/.claude/settings.json
#    (see Code section above for config)

# 4. Add artifacts/bin to PATH
export PATH="$HOME/sisyphus/artifacts/bin:$PATH"
```

**After installation:**
```bash
claude-safe                  # Use instead of 'claude'
ccusage blocks              # Check usage anytime
ccusage blocks --live       # Real-time dashboard
```

## API

**Inputs:**
- `CLAUDE_BUDGET_THRESHOLD` ⟜ percentage threshold (default: 80)
- `CLAUDE_PLAN` ⟜ pro|max5|max20 (default: pro)

**Outputs:**
- Blocks Claude Code start if over threshold
- Shows current usage, reset time, options

**Side effects:**
- Creates `~/sisyphus/artifacts/bin/claude-safe`
- Configures `~/.claude/settings.json`
- Requires `npx` (Node.js) for ccusage

## Monitoring Schedule

**5-hour window** ⟜ Pro plan: 10-40 prompts per cycle
**Weekly limit** ⟜ Pro plan: 40-80 hours Sonnet 4 total
**Reset timing** ⟜ plan work sessions around reset boundaries

**Strategy**
  ⟜ need predictable availability
  ⟝ reserve last 20% of weekly budget for emergencies
  ⟞ schedule intensive work early in week

## Troubleshooting

**Wrapper blocks you but budget seems fine:**
```bash
ccusage blocks --json | jq    # Check actual usage
ccusage blocks               # Might be weekly limit, not 5-hour
```

**Status line not showing:**
```bash
cat ~/.claude/settings.json  # Verify config
npx ccusage@latest statusline # Test manually
```

**ccusage errors:**
```bash
npx ccusage@latest blocks     # Update to latest
npx --force ccusage@latest blocks  # Force fresh install
```

## Refunctionalization Notes

This card defunctionalizes Claude Code monitoring workflows.

**Setup script** ⟜ extracts to bash, runs once for installation
**Wrapper script** ⟜ embedded in setup, prevents sessions above threshold
**ccusage integration** ⟜ existing tool, no reimplementation needed  
**Status line config** ⟜ JSON embedded in setup script
**Protocol** ⟜ morning/during/end rituals establish sustainable practice

## Tests

**Threshold enforcement** ⟜ wrapper exits when usage > threshold
**Status visibility** ⟜ status line updates every few seconds
**Reset tracking** ⟜ ccusage blocks shows accurate countdown
**Weekly trends** ⟜ ccusage monthly identifies consumption patterns
**PATH integration** ⟜ claude-safe accessible from any directory

## Status

**Tests:** untested (card not yet refunctionalized)
**Last updated:** 2025-01-01
