
Example 2: Subtraction with the Stream Model
To build intuition, hyperfunctions can be visualized as streams of functions:
haskell(a ↬ b) ≈ Stream (a → b)
Key combinators:
haskell(⊳) :: (a → b) → (a ↬ b) → (a ↬ b)      -- Push function onto stream
(⊙) :: (b ↬ c) → (a ↬ b) → (a ↬ c)      -- Zip streams
run :: a ↬ a → a                          -- Collapse to value
Semantics:
haskell(f ⊳ fs) ⊙ (g ⊳ gs) = (f ∘ g) ⊳ (fs ⊙ gs)
run (f ⊳ fs) = f (run fs)
Implementation of subtraction:
haskelln − m = N (λs z → run (nat n (id⊳) (rep (const z)) ⊙ 
                       nat m (id⊳) (rep s)))
How it works:

Convert n to stream: n ids followed by infinitely many const z
Convert m to stream: m ids followed by infinitely many s
Zip them: first m ids, then n-m applications of s, then const z
Run to get n - m

Example 3: Zip with Message Passing
The zip problem: How to define zip as a fold over both input lists simultaneously?
The paper introduces Producer and Consumer types:
haskelltype Producer o a = (o → a) ↬ a        -- Produces outputs
type Consumer i a = a ↬ (i → a)        -- Consumes inputs
Implementation:
haskellzip :: [a] → [b] → [(a, b)]
zip xs ys = ι (foldr xf xb xs) (foldr yf yb ys)

-- Producer side (xs)
xf x xk = prod x xk
  where prod o p = Hyp (\q → ι q p o)
xb = Hyp (const [])

-- Consumer side (ys)  
yf y yk = cons (\x xys → (x, y) : xys) yk
  where cons f p = Hyp (\q i → f i (ι q p))
yb = Hyp (const [])
This defines zip entirely with folds, enabling fusion of both branches!
