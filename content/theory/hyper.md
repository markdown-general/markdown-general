# hyperfunctions

## Example - Subtraction with the Stream Model

To build intuition, hyperfunctions can be visualized as **streams of functions**:

```haskell
(a ↬ b) ≈ Stream (a → b)
```

**Key combinators:**

```haskell
(⊳) :: (a → b) → (a ↬ b) → (a ↬ b)      -- Push function onto stream
(⊙) :: (b ↬ c) → (a ↬ b) → (a ↬ c)      -- Zip streams
run :: a ↬ a → a                        -- Collapse to value
```

**Semantics:**

```haskell
(f ⊳ fs) ⊙ (g ⊳ gs) = (f ∘ g) ⊳ (fs ⊙ gs)
run (f ⊳ fs) = f (run fs)
```

**Implementation of subtraction:**

```haskell
n − m = N (λs z → run (nat n (id⊳) (rep (const z)) ⊙ 
                       nat m (id⊳) (rep s)))
```

How it works:
1. Convert `n` to stream: `n` ids followed by infinitely many `const z`
2. Convert `m` to stream: `m` ids followed by infinitely many `s`
3. Zip them: first `m` ids, then `n-m` applications of `s`, then `const z`
4. Run to get `n - m`

The stream is infinite, but fusion eliminates it at compile time!

---

## Example - Zip with Message Passing

**The zip problem:** How to define zip as a fold over **both** input lists simultaneously?

The paper introduces **Producer and Consumer types**:

```haskell
type Producer o a = (o → a) ↬ a        -- Produces outputs
type Consumer i a = a ↬ (i → a)        -- Consumes inputs
```

**Implementation:**

```haskell
zip :: [a] → [b] → [(a, b)]
zip xs ys = ι (foldr xf xb xs) (foldr yf yb ys)

-- Producer side (xs)
xf x xk = prod x xk
  where prod o p = Hyp (\q → ι q p o)
xb = Hyp (const [])

-- Consumer side (ys)  
yf y yk = cons (\x xys → (x, y) : xys) yk
  where cons f p = Hyp (\q i → f i (ι q p))
yb = Hyp (const [])
```

This defines zip **entirely with folds**, enabling fusion of both branches!

The pattern: **message passing between continuations executed in lockstep**. The `ι` function runs a Producer and Consumer together, with values flowing between them as messages.

---

## Theoretical Properties

### Category Structure

Hyperfunctions form a **category**:
- Objects: Same as the base category  
- Arrows: Hyperfunctions `a ↬ b`
- Composition: `#` (or `⊙`)
- Identity: `self = lift id`

**Key properties:**
```haskell
(f # g) # h = f # (g # h)    -- Associativity
f # self = f = self # f       -- Identity
```

The `lift` operation is a **functor** from the base category into the hyperfunction category:
```haskell
lift :: (a → b) → a ↬ b
lift (f ∘ g) = lift f # lift g
```

## Applications Beyond Theory

### Well-Foundedness

The paper addresses coinduction and well-foundedness through **syntactic guarded coinduction**:

- A CCS process is **guarded** if syntactically "under" an action
- Corecursive calls are permitted only if guarded
- Example: `p = a · p` is well-founded (guarded by `a`)
- Counter-example: `p = p ⊕ p` is not well-founded

All proofs proceed via induction on CCS syntax, with recursive calls either reducing argument size or being guarded under an action.

### Library Implementation

The paper demonstrates practical use in:

**LogicT for backtracking search:**

The monadic variant of `⊳` preserves effect ordering:

```haskell
(⊳ₘ) :: Monad m ⇒ (m a → b) → m (m a ↬ b) → (m a ↬ b)
ι (f ⊳ₘ h) k = f (ι k =<< h)
```

This enables efficient interleaving of LogicT computations:

```haskell
interleaveT :: Monad m ⇒ LogicT m a → LogicT m a → LogicT m a
interleaveT xs ys = LogicT (λc n →
  do xz ← runLogicT xs (λx xk → return (c x ⊳ₘ xk)) (return (Hyp (const n)))
     yz ← runLogicT ys (λy yk → return (c y ⊳ₘ yk)) (return (Hyp (const n)))
     ι xz yz)
```

**Concurrency monads:**

Building on Claessen's concurrency monad, hyperfunctions enable efficient scheduling:

```haskell
type Conc r m = Cont (m r ↬ m r)

atomₕ :: Monad m ⇒ m a → Conc r m a
atomₕ am = Cont (λk → id ⊳ₘ (k <$> am))

forkₕ :: Conc r m a → Conc r m ()
forkₕ m = Cont (λk → runCont m (const id) ◦ k ())

runₕ :: Conc r m a → m r
runₕ c = run (runCont c (const id))
```

This provides round-robin scheduling without intermediate list structures!

**First-class coroutines:**

The paper presents a new coroutine library with **first-class control** using a Channel type:

```haskell
type Channel r i o = (o → r) ↬ (i → r)

newtype Co r i o m a = Co {
  route :: (a → Channel (m r) i o) → Channel (m r) i o
}
```

**Key operations:**

```haskell
yield :: o → Co r i o m i
yield x = Co (λk → Hyp (λh i → ι h (k i) x))

send :: MonadCont m ⇒ Co r i o m r → i → m (Either r (o, Co r i o m r))
send c v = callCC $ λk → Left <$> ι (route c (λx → Hyp (λ_ → return x)))
                                    (Hyp (λr o → k (Right (o, Co (const r))))) 
                                    v
```

The `send` function enables **external communication** with coroutines - you can pass values to a coroutine and receive values back while it remains a first-class value!

### Language Semantics

Hyperfunctions provide a continuation-based semantics for concurrent languages, solving problems that have resisted other continuation-based approaches.

**The Communicator Type for CCS:**

```haskell
type Communicator n r = (Message n → r) ↬ (Message n → r)
data Message n = 𝕢 | 𝕒 (Act n)
```

A Communicator is a process with result type `r` that passes messages. A message is either:
- `𝕢` (query): asking "what is your top-level action?"
- `𝕒 a` (answer): responding with action `a`

**Full Abstraction Result:**

The paper's major theoretical contribution - hyperfunctions provide a **fully abstract model** for the Calculus of Communicating Systems (CCS).

**Main Theorem:** The Communicator model is fully abstract for CCS.

For any CCS processes `p` and `q`:
```
p ∼ q  ⟺  ⟦p⟧Communicator ≡ ⟦q⟧Communicator
```

This means:
- **Soundness:** Bisimilar processes have equal denotations
- **Completeness:** Equal denotations imply bisimilarity

The proof works by relating Communicator to the Proc model (a standard fully-abstract model based on coinductive rose trees) via homomorphisms.

**Why this matters:**

From the paper:
> "Although continuation-passing style is sometimes regarded as a standard style to use for denotational semantics, it is inadequate for describing languages that involve non-determinism or concurrent processes."

There was previously **no fully-abstract continuation-based model** for a concurrent language like CCS. Hyperfunctions solve this long-standing problem in programming language semantics.

---

## The Fundamental Pattern

Across all applications, hyperfunctions enable **communicating continuations**:

**Core capabilities:**
- Continuations that send and receive values
- Multiple continuations executing in parallel
- Structured coordination without explicit threading

**Why hyperfunctions work:**

1. **Nested Structure:** Allows multiple computations to proceed in lockstep
2. **Communication:** Values can flow between parallel continuations  
3. **Coroutining:** Control transfers seamlessly between processes
4. **Fusion:** The hyperfunction structure compiles away, leaving efficient code

**The recurring pattern:**

Whenever you need to:
- Process multiple data structures in parallel (like zip)
- Coordinate between concurrent processes (like CCS)
- Implement coroutines with message passing
- Fuse computations that communicate

### Key insights

Hyperfunctions are not just a clever trick for optimization - they are a fundamental pattern that appears whenever continuations need to communicate. The type itself emerges from the requirements of the problem domain, which explains why it has been independently rediscovered so many times.

By providing a comprehensive framework and proving full abstraction for CCS, this paper elevates hyperfunctions from an obscure optimization technique to a well-understood tool with solid theoretical foundations and clear practical applications.

### Haskell

At the core is the **hyperfunction** type, an infinitely left-nested function:

```haskell
a ↬ b = (((... → a) → b) → a) → b
```

In Haskell, this is implemented as a recursive newtype:

```haskell
newtype a ↬ b = Hyp {ι :: (b ↬ a) → b}
```

### Why This Type Exists

**Cardinality restrictions** prevent this type from having a set-theoretic interpretation. However, it does have a **domain-theoretic interpretation** as the solution to `X ≅ (X ⇒ A) ⇒ B`.

The paper notes that most programming languages don't impose strict cardinality restrictions on type definitions, so hyperfunctions can be defined as a simple (but strange) recursive type.


### quote

"This type has a lot in common with Pipe from the previous section, with
one significant difference: instead of using separate producer and
consumer continuations, it has one continuation which both produces and
consumes. This means that every input is accompanied by an output: in
terms of the interface to this type, this means that yield and await are
combined into one function that outputs a value and waits for an input
at the same time."

You can find this code attached to pipes in pipes-concurrency.

<https://hackage.haskell.org/package/pipes-concurrency-2.0.14/docs/src/Pipes.Concurrent.html#withSpawn>

Same for the box library, which shares this feature with pipes. The
feature is pretty inherent in Haskell lore going back at least to Why FP
Matters

### box

Quoting, "Wherever concurrency and continuations intersect, authors have used hyperfunctions", I thought, hey, I use STM a lot and, hey, I do a lot of CPS (ghc has trained me by now)

And I found my lateral function straight away in <https://github.com/tonyday567/box/blob/main/src/Box/Queue.hs#L159>

``` haskell-ng
withQ ::
  Queue a ->
  (Queue a -> IO (Box IO a a, IO ())) ->
  (Committer IO a -> IO l) ->
  (Emitter IO a -> IO r) ->
  IO (l, r)
withQ q spawner cio eio =
  bracket
    (spawner q)
    snd
    ( \(box, seal) ->
        concurrently
          (cio (committer box) `finally` seal)
          (eio (emitter box) `finally` seal)
    )
```

## Hyperfunctions: Reference

**Authors:** Donnacha Oisín Kidney and Nicolas Wu  
**Institution:** Imperial College London  
**Conference:** POPL 2026  
**Preprint:** https://doisinkidney.com/pdfs/hyperfunctions.pdf

