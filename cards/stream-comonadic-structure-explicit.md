stream-comonadic-structure-explicit ⟜ demonstrate comonadic structure in stream model

**input** ⟜ stream/comonadic concepts

**output** ⟜ explanation + code examples

**instruction**
define Stream data type: data Stream a = End a | a :<| Stream a (unbounded, not a list)
implement Uncons instance: uncons (End t) = That t; uncons (t :<| rest) = These rest t
implement extract operation: project (parse (satisfy vowel)) s returns (a, remainder)
demonstrate that structure is comonadic: operations compose, laws hold, extraction preserves meaning in context
