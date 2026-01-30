Significantly cut back work/card.md ⟜ reduce spec anxiety, clarify execution semantics

**instruction**
1. Read work/card.md
2. Cut aggressively, keep:
   - "What is a card" (lines 5–11)
   - Card shape (title, instruction, comment)
   - Card flow language basics (lines 28–46 only; drop lines 47–98)
   - "How cards are used" section
3. In flow language section, explicitly add clarifications:
   - Symbols (-, =, ~) are examples; their meaning grows through use, not predetermined
   - Gate evaluation (⊢ [condition] ⊣) is yin's job after breathe-step check-in; not machine-parsed
   - Indentation is insensitive; whitespace doesn't constrain logic
4. Add 2–3 minimal real examples (or make-up realistic ones)
5. Remove/simplify: "Why this language", "semantic bootstrap" explanations, agent-parseability notes, "The card promise"

**comment** ⟜ Current card.md creates usage anxiety by establishing conventions we haven't settled. Better to acknowledge symbols are semantic bootstrap than pretend we have fixed heuristics. Gate evaluation belongs to yin-in-conversation, not static spec. Prose explanation of why symbols matter (lines 91–97) is overhead; better shown than explained.
