# yin spin modes ⟜ patience, execution, rhythm

These patterns describe how cards are executed and how operators manage work. Use when designing card execution infrastructure and teaching operators about spin discipline.

## How cards are executed

Cards are spun. The spin mode describes patience, not the pattern:

**Inline** — You execute via bash right now. Read output immediately. Continue. Good for quick checks (haskell.md reading, cabal-clean verifying).

**Circuit** — Background to log/. You keep working. Listener notifies when done. You read response when ready. Good for slow operations (cabal-build, library assessment).

**Spin** — Agent executes independently, writes to log/, reports when done. You don't wait. Good for background parallel work. Trust the card.

All three modes use the same card. The mode describes your patience with the work, not the work itself.

## The Cycle: Think → Write → Spin → Breathe

**Think** — Observe the landscape. What's needed? What card does this look like?

**Write** — Find the card in ~/markdown-general/cards-purpose-based/, or write a new one. Examples:
  - haskell.md: incremental dependency substitution
  - debug-filewatcher-chain: diagnostic method
  - library-metadata, library-haddock: health checks
  - comonad-instance-false-start: warning pattern

**Spin** — Execute the card (inline, circuit, or background). Read the output. Decide next move.

**Breathe** — Leave space. Don't watch. Don't fuss. Trust the card and the listener. Next cycle comes when you're ready.

⚠️ **Card hazard** — When a card finally succeeds after many attempts, it's easy to get stuck reading the output, losing focus and tokens. Spin to background instead. Step back. Let the listener report. Trust and breathe.

## In Action

**Cheerful** — Write card, spin, breathe. No anxiety loops.

**Pattern-conscious** — Find the right card. Trust it. Read signals, not raw output.

**Poised** — Background infrastructure handles monitoring. You decide, not manage.

**Present** — Stay in conversation. Spin cards, read responses, talk with operator.
