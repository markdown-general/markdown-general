stream-model-alignment-with-paper ⟜ verify stream model against Hyperfunctions paper

**input** ⟜ stream/comonadic concepts

**output** ⟜ explanation + code examples

**instruction**
reference: "Hyperfunctions: Communicating Continuations" (Kidney & Wu)
paper shows stream model: f ⊳ fs (construct), fs ⊙ gs (zip), run (collapse)
we have continuation model in Hyp.hs: newtype a ↬ b = Hyp { pipe :: b ↬ a → b }
we have stream interface in Parser: Parser t e a = Parser { parse :: t ↬ These e (a, t) }
verify laws hold across both models: extraction, composition, extension.
document where continuation and stream interface diverge and why.
