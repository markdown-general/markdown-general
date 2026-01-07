# zone/tools/bench-tokens.md

**Track token consumption and time cost when installing cards**

Cards should know their refunctionalization cost - tokens consumed and time elapsed during installation.

## Status Format

After `card-api flatten-md.md --install`, the card's Status section gains:

```markdown
**Installation Cost:**
  - Tokens: 23,456 (input: 15,678, output: 7,778)
  - Cache: 5,234 read, 2,544 created
  - Time: 1m 12s
  - Model: claude-sonnet-4-5-20250929
  - Installed: 2025-01-01 15:45 UTC
```

## Implementation

**Track start:**
```bash
START_TIME=$(date +%s)
START_TOKENS=$(ccusage blocks --json | jq '.[-1].totalTokens')
```

**Do installation:**
- Extract code blocks
- Compile
- Test
- Install to artifacts/bin/

**Track end and update:**
```bash
END_TIME=$(date +%s)
END_TOKENS=$(ccusage blocks --json | jq '.[-1].totalTokens')
TOKEN_DELTA=$((END_TOKENS - START_TOKENS))
TIME_DELTA=$((END_TIME - START_TIME))
MINUTES=$((TIME_DELTA / 60))
SECONDS=$((TIME_DELTA % 60))

# Get breakdown
INPUT=$(ccusage blocks --json | jq '.[-1].tokenCounts.inputTokens')
OUTPUT=$(ccusage blocks --json | jq '.[-1].tokenCounts.outputTokens')
CACHE_READ=$(ccusage blocks --json | jq '.[-1].tokenCounts.cacheReadInputTokens')
CACHE_CREATE=$(ccusage blocks --json | jq '.[-1].tokenCounts.cacheCreationInputTokens')
MODEL=$(ccusage blocks --json | jq -r '.[-1].models[0]')
TIMESTAMP=$(date -u +"%Y-%m-%d %H:%M UTC")

# Update card Status section with Installation Cost data
```

## Integration Point

**card-api.md --install handler** should:
1. Call tracking logic before extraction
2. Do installation work
3. Update card Status section after completion

## Data Source

From `ccusage blocks --json` (last block):
```json
{
  "tokenCounts": {
    "inputTokens": 15678,
    "outputTokens": 7778,
    "cacheCreationInputTokens": 2544,
    "cacheReadInputTokens": 5234
  },
  "totalTokens": 23456,
  "models": ["claude-sonnet-4-5-20250929"]
}
```

## Benefits

**Cost visibility** ⟜ see refunctionalization expense per card
**Planning** ⟜ know token budget needed before installing
**Optimization** ⟜ compare costs across card iterations
**Self-documentation** ⟜ card contains its own creation metadata

## Edge Cases

- **First install:** create Installation Cost section
- **Re-install:** replace existing cost data
- **Failed install:** don't update (preserve last success)
- **Missing ccusage:** record "unknown" for missing fields

## Dependencies

- `ccusage` (via npx)
- `jq` (brew install jq)
- `date` command

## Status

**Tests:** not yet implemented
**Last updated:** 2025-01-01
