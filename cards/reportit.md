# card: reportit ⟜ synthesis and status reporting

**type** ⟜ flow / reporting (permanent and on-demand)

**purpose** ⟜ read synthesis, detect phase transitions, generate actionable reports

**effects** ⟜ reads: flows-log + tailie-synthesis.md, writes: flows-log + reports, spawns: none, network: no

---

## Permanent Mode

Reportit is always enabled via tailie synthesis. Yin checks synthesis output to understand:
- Phase transitions (exec-heavy → balanced)
- Accumulation rate (flows-log growing?)
- Domain emphasis (too much coordination? not enough testing?)

---

## On-Demand Reporting

Operator or yin can request a report anytime:

### Full Status Report

Triggered when:
- Cleanit completes
- After buildif initial build
- Before publishit
- Operator requests: "give me status"

**Generate from:**
- Synthesis data (domain counts)
- Recent flows-log entries
- Response files (worker results if any)
- Current git state

**Output to flows-log:**
```
reportit-full-status | [repo] | build: [✓/✗] | tests: [pass/fail] | docs: [coverage%] | ready: [Y/N]
```

Also write detailed report to file:
```
~/self/yin/responses/report-[repo]-full.md
```

Example format:
```markdown
# Status Report: [repo]

**Date:** [timestamp]
**Phase:** [current development phase]

## Build Status
- Latest: [✓/✗]
- Errors: [N]
- Warnings: [N]

## Test Status
- Unit tests: [pass/fail]
- Documentation tests: [pass/fail]
- Coverage: [%]

## Code Quality
- hlint suggestions: [N]
- Haddock coverage: [%]

## Dependencies
- Total: [N]
- Outdated: [N]
- Unused: [N]

## Ready for:
- [ ] Development
- [ ] CI run
- [ ] Merge
- [ ] Release

## Known Issues
- [list if any]

## Next Steps
- [recommended actions]
```

---

## Phase Transition Reporting

When tailie synthesis detects phase shift (via bloodhound patterns):

```
reportit-phase-transition | [from] → [to] | [trigger]
```

Example:
```
reportit-phase-transition | exec-heavy → balanced | build-complete
```

Yin reads this signal and decides:
- If "exec-heavy → balanced": build is done; move to testing
- If "balanced → coord-dense": analysis/review phase
- If unexpected transition: scout mode; investigate

---

## Synthesis Monitoring

Yin reads tail of tailie-synthesis.md periodically:

```bash
tail -20 ~/self/yin/tailie-synthesis.md
```

Each line shows:
```
[timestamp] +{lines} | coord({N}) struct({N}) exec({N}) practice({N}) other({N})
```

Yin interprets:
- **exec high, others low** → building/testing phase
- **coord high** → decision/review phase
- **balanced** → mixed activity, or between phases
- **practice high** → documentation/refactoring

---

## Custom Reports

Operator can request specific reports:

Examples:
- "List all dependencies and their bounds"
- "Show last 10 build failures and fixes"
- "Document all test failures and status"

Operator specifies, yin/fixit generates on-demand.

---

## Calls to Action Cards

None directly; reportit reads existing state and synthesizes.

---

## Notes

Reportit is a synthesis layer. It doesn't change code; it interprets patterns and makes them visible. This is how operator makes informed decisions about next steps.

The report output is meant to be human-readable and actionable. When operator reads a report, they should know exactly what to do next (or that everything is good).

