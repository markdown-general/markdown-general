# zone/tools/monitoring.md

**safety rating** ⟜ **moderate** (visibility-based rate control)

Claude Code usage monitoring with real-time visibility.

**Problem** ⟜ monthly limit consumed in 3 hours
**Need** ⟜ real-time visibility, predictive warnings
**Solution** ⟜ status line + ccusage for passive monitoring

## Quick Reference

**Morning ritual** (before starting work)
```bash
ccusage blocks                # Check available budget
tmux new-session -d -s claude-monitor 'npx ccusage@latest blocks --live'
```

**During session**
- Glance at status line constantly (updates automatically)
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

## Monitoring Layers

**Built-in visibility** ⟜ `/usage` command shows current consumption
**Status line** ⟜ persistent display at bottom of Claude Code interface
**Real-time tracking** ⟜ ccusage blocks --live for burn rate analysis
**Predictive warnings** ⟜ alert at 50%, 75%, 90% thresholds
**Session discipline** ⟜ one task per reset cycle, no background runs

## Warning Thresholds

**Token budget**
  ⟜ approaching daily/session allocation
  ⟝ status line shows warning at high usage

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

The status line is a persistent display at the bottom of Claude Code. Current implementation shows:

**🌿 Branch** ⟜ current git branch
**📁 Path** ⟜ working directory
**◇ Context** ⟜ session context usage percentage (warns at 60%+)
**💰 Daily cost** ⟜ today's total spending (warns at $10+)

Updates automatically - constant visibility without interrupting flow.

Configured in `~/.claude/settings.json`:
```json
{
  "statusLine": {
    "type": "command",
    "command": "~/.claude/claude-status.sh",
    "padding": 0
  }
}
```

## ccusage Commands

**Check current usage:**
```bash
ccusage blocks              # Current 5-hour block status
ccusage blocks --json       # Machine-readable output
ccusage blocks --live       # Real-time dashboard
```

**Historical analysis:**
```bash
ccusage daily               # Daily breakdown
ccusage monthly             # Monthly trends
ccusage session             # Current session stats
```

**Specific date ranges:**
```bash
TODAY=$(date +%Y%m%d)
ccusage daily --since "$TODAY" --until "$TODAY" --json
```

## Monitoring Schedule

**5-hour window** ⟜ Pro plan: 10-40 prompts per cycle
**Weekly limit** ⟜ Pro plan: 40-80 hours Sonnet 4 total
**Reset timing** ⟜ plan work sessions around reset boundaries

**Strategy**
  ⟜ need predictable availability
  ⟝ reserve last 20% of weekly budget for emergencies
  ⟞ schedule intensive work early in week

## Tmux Live Monitor

**Start background monitoring:**
```bash
tmux new-session -d -s claude-monitor 'npx ccusage@latest blocks --live'
```

**View anytime:**
```bash
tmux attach -t claude-monitor  # Ctrl+B, D to detach
```

**Stop monitoring:**
```bash
tmux kill-session -t claude-monitor
```

## Troubleshooting

**Status line not showing:**
```bash
cat ~/.claude/settings.json          # Verify config
~/.claude/claude-status.sh          # Test manually
```

**ccusage errors:**
```bash
npx ccusage@latest blocks            # Update to latest
npx --force ccusage@latest blocks    # Force fresh install
```

**Unexpected high usage:**
```bash
ccusage blocks --json | jq           # Detailed breakdown
ccusage session --json | jq          # Current session analysis
```

## API

**Environment:**
- Requires `npx` (Node.js) for ccusage
- Status line script at `~/.claude/claude-status.sh`
- Configuration in `~/.claude/settings.json`

**Outputs:**
- Status line: persistent display, updates every few seconds
- ccusage: JSON or formatted terminal output
- Warnings: visual indicators (⚠) when thresholds exceeded

## Best Practices

**Context management:**
- Use `/compact` aggressively (every 5-10 messages)
- Use `/clear` between distinct tasks
- Keep conversation focused on single topic

**Session discipline:**
- Check `ccusage blocks` before starting
- Monitor status line during work
- Review `ccusage daily` at end of session

**Budget pacing:**
- Track weekly burn rate with `ccusage monthly`
- Reserve 20% budget for emergencies
- Schedule large tasks early in week

## Status

**Active monitoring:** Status line + ccusage blocks --live
**Last updated:** 2025-01-07
**Dependencies:** ccusage (npm), jq (brew)
