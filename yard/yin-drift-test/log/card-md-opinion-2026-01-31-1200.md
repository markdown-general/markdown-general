# card.md 25% reduction analysis: agent1 vs agent4 vs original

**agent3 vs agent6 vs original (first assessment)**

original ⟜ 37 lines, balanced
  - includes Spin/Inline modes only
  - "Evolution" section covers heuristics and learning
  - clear but linear path

agent3-simplified ⟜ 44 lines, restructured
  - adds "What is a card" section (frames expectations)
  - adds "How cards are used" (explicit Spin/Inline/Circuit)
  - adds Circuit execution mode—present in source material
  - agent6 is line-for-line identical to original; no value signal

Finding: agent3 preserves Circuit mode; agent6 loses it. If Circuit is live practice, agent3 is more complete despite +7 lines.

---

**agent1 vs agent4 vs original (25% cuts comparison)**

original ⟜ 37 lines
  - structure: What → Shape → Writing → Spin/Inline → Evolution
  - execution modes: Spin/Inline only (no Circuit)

agent1-simplified ⟜ 44 lines, learner-focused
  - adds "What is a card" framing
  - "How to write" expanded to 6 explicit steps (Name, Identify, Write, Comment, Test, Archive)
  - execution modes: Spin/Inline/Circuit all present
  - adds "When a card fails" section
  - adds "The library" closure on persistence
  - dense but pedagogical; traces library pathways explicitly

agent4-simplified ⟜ 9 lines, extreme compression
  - collapses to definition → shape → evolution → persistence
  - omits execution modes entirely (Spin/Inline/Circuit gone)
  - omits "How to write" as explicit steps
  - omits "When a card fails"
  - reader reconstructs intent from sparse material

**Curation by grammar.md patterns:**

trace (preserve pathways)
  - original: mid-range; covers basics
  - agent1: highest; maps decision sequence, execution modes, failure path
  - agent4: lowest; reader fills gaps

prune (remove dead weight)
  - original: balanced, no obvious waste
  - agent1: "Test it. Archive it." as separate steps may be instructional buildup vs. necessity
  - agent4: aggressively removes scaffolding, functional but fragile

structure (respect relationships)
  - original: clear elaboration chain
  - agent1: adds pedagogical structure (6-step sequence) that mirrors actual practice
  - agent4: collapses relationships; leads become implicit

breathe (space for future)
  - original: small gaps between sections
  - agent1: compressed; less room for elaboration
  - agent4: wide open space but skirts "clearly vague" → "unclear"

**Observation**: agent1 and agent3 both solve the same problem—they recover execution modes (Circuit) and structure (pedagogical steps). agent4 and agent6 both compress aggressively but lose modes and scaffolding.

No decision requested. Findings logged for library learning.
