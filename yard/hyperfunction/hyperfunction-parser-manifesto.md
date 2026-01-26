hyperfunction-parser-manifesto ⟜ unified parsing architecture via hyperfunctions

**input** ⟜ parser/hyperfunction design

**output** ⟜ architecture + implementation approach

**instruction**
articulate why state-based parsers (parsec, attoparsec, megaparsec) force conflicting choices: HTML recovery vs XML zippers, regex integration vs performance measurement, logging vs semantic parsing.
present hyperfunction solution: each stage independent, compose with operators (↬, ⊲, push, rep).
example chain: a ↬ tokenParsing ↬ perf ↬ textParsing ↬ log ↬ semanticParsing ↬ b
