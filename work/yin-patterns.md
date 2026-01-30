# yin-patterns ⟜ low-level execution choreography

Patterns for working cards, handling gates, logging, and staying in conversation.

## Gate mechanics

**What a gate looks like**
```
⊢ [condition] ⊣
  - path-1
  = path-2
```

**What it means to yin**
A gate is where you think operationally. You have evaluated or prepared something (findings, analysis, draft). The condition is a conversation point. You assess: am I clear on what we're doing? Am I certain enough to proceed? You bring findings to the operator. You're responsible for walking through—and when you walk through a gate, we all go with you.

**How to recognize you've hit a gate**
You are about to make a decision or execute a change. Look at the flow above. If there's a `⊢...⊣`, that's your signal: output what you have, think operationally about whether you're ready, present your findings.

**Gate as agency**
Gates are where your agency shows up. You assess your context, verify your understanding, decide whether to proceed. When you walk through a gate, you're choosing to. The operator sees your thinking and confirms, modifies, or redirects. Skipping a gate isn't faster—it's drift.

## Card reading before execution

**Skim the flow first**
Before executing any card, read the flow structure (→ arrows, gates ⊢...⊣). Know where the stops are. Know where you might hand off.

**Inline execution**
If you're running a card inline (reading the output immediately), you'll hit gates naturally because you're watching. The card won't let you move forward without talking.

**Spun execution**
If you're spinning a card to a field agent, include the flow in the spin instruction. Let them know gates exist. They should log findings at gates and wait for signal before continuing.

**Circuit execution**
If a card is logging to a background circuit, the listener will notify when there's output. You read the log, see if you're at a gate, decide whether to continue or signal back.

## Logging during card work

**When to log**
- At gates: Always. Document findings, analysis, what you're proposing.
- At major steps: If the step produces output (files changed, analysis done), log it.
- At end: Final summary if useful, or let the changed files speak.

**What not to log**
- Duplicate the card. The card exists.
- Transcribe the conversation. The conversation is elsewhere.
- Over-explain what's obvious from the changes themselves.

**Log location**
All outputs from a card work session go to `log/[card-name]-[what]-[timestamp].md`. One card can produce many logs during a session. They're part of the same work.

**Permanent vs. ephemeral**
Once a card is done, its logs stay in log/. They're the working record of that execution. If you spin the same card again later, you'll produce new logs. Old logs don't get deleted; they show history.

## Card hand-off patterns

**Findings at a gate**
You've read and analyzed. You have findings. You write them to a log (or state them in conversation). You present them. You ask: "Should I proceed with fix X, Y, or Z?" Operator picks. You continue.

**Changed files as the record**
When a card finishes, the files it modified are the primary record. Logs are supporting context. If drift.md changed and readme.md changed, that's what happened. The logs explain the reasoning if needed.

**One card, multiple gates**
A card can have many gates. You assess at gate 1, present findings, operator confirms direction. Continue to gate 2, assess again, present new findings. Multiple decision points, one card, one session. You're thinking operationally at each one.

## Drift during card work

**You notice the flow doesn't match reality**
Example: Card says "check if file exists" but file is in a different place than the card assumes. This is drift. This is operational thinking in action—you've caught a mismatch. Flag it, ask operator, update the card before continuing.

**Card references are stale**
Example: Card references work/old-tool.md but the tool moved. Don't guess where it went. Ask. Update card and flow before continuing.

**Gate condition is unclear**
Example: Card says `⊢ [build-succeeds] ⊣` but you don't know what build-succeeds means in this context. That's a card writing issue. Stop, ask, clarify.

## Work is the work, conversation is part of it

**Before a gate, show your thinking**
Don't just list findings dry. Narrate: "I found X, which suggests Y. I think we should do Z, which means changing these files. Does that sound right?" This is where you think operationally.

**Gates are decision points**
Gates are where operator and yin verify alignment. Your findings, your reasoning, your readiness. The gate is where you assert your thinking and we decide together.

**Reading cards is part of execution**
Understanding the flow before you act prevents false steps. A well-written card's flow is a script for a conversation, not just a checklist. You're responsible for knowing where you are.

## Card work lifecycle

1. **Spin or accept the card** — You or operator assigns it
2. **Read the flow** — Understand where gates are
3. **Execute to first gate** — Work the first steps, produce findings
4. **Present at gate** — Show findings, ask for approval
5. **Get signal** — Operator picks a path or modifies approach
6. **Continue or restart** — Execute next steps
7. **Repeat** — Hit next gate, same pattern
8. **Card done** — All paths complete or terminated
9. **Files changed, logs preserved** — Record stands, move to next card

## Patterns still emerging

These are the patterns we're seeing now. More will emerge as we work:
- Circuit + listener patterns (background execution notification)
- Field agent hand-off specifics (what to include when spinning)
- Multi-card flows (how cards reference and depend on each other)
- Yin-narrow vs. yin-wide mode patterns (context switching, focus zones)
- Forking and merging (when a card branches, how do branches reunify)
