stream-model-comonadic-structure.md ⟜ understand infinite streams and inductive stops

**input** ⟜ stream/comonadic concepts

**output** ⟜ explanation + code examples

**instruction**
show that hyperfunction is infinite stream of functions: rep f = f `push` rep f (infinite by definition)
inductive stop is type-driven: Applicative <*> with These e a result decides:
  This e → hard stop (error, no value, abandon)
  That a → soft stop (value extracted, success)
  These e a → partial success (keep going with value + error)
implement extract (project) and extend operations comonadically.
explain how stream threading and stopping conditions work together.
