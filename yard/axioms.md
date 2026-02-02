# Hyperfunctions Axiomatically

## Category-Theoretic Framework

Let $\mathcal{C}$ be the base category of ordinary functions. We define a new category $\mathcal{H}$ of hyperfunctions with the same objects as $\mathcal{C}$ but morphisms $\mathrm{H}(a, b)$ representing hyperfunctions from $a$ to $b$.

### Required Operations

The hyperfunction category $\mathcal{H}$ is equipped with the following operations:

\[
\begin{align*}
(\circ) &: \mathrm{H}(b, c) \times \mathrm{H}(a, b) \to \mathrm{H}(a, c) \\
\mathrm{lift} &: \mathcal{C}(a, b) \to \mathrm{H}(a, b) \\
\mathrm{run} &: \mathrm{H}(a, a) \to a \\
(\ll) &: \mathcal{C}(a, b) \times \mathrm{H}(a, b) \to \mathrm{H}(a, b)
\end{align*}
\]

where:
- $\mathrm{self} := \mathrm{lift}(\mathrm{id}_a) : \mathrm{H}(a, a)$
- $\mathrm{fix}$ denotes the fixed-point operator
- $(\cdot)$ denotes ordinary function composition in $\mathcal{C}$

## Axiomatic System

**Axiom 1 (Associativity of $\circ$):**
\[ (f \circ g) \circ h = f \circ (g \circ h) \quad \forall f : \mathrm{H}(b, c), g : \mathrm{H}(a, b), h : \mathrm{H}(a, c) \]

**Axiom 2 (Identity for $\circ$):**
\[ f \circ \mathrm{self} = f = \mathrm{self} \circ f \quad \forall f : \mathrm{H}(a, a) \]

**Axiom 3 (Lift as Functor):**
\[ \mathrm{lift}(f \cdot g) = \mathrm{lift}(f) \circ \mathrm{lift}(g) \quad \forall f, g \in \mathcal{C} \]

**Axiom 4 (Run-Lift Correspondence):**
\[ \mathrm{run}(\mathrm{lift}(f)) = \mathrm{fix}(f) \quad \forall f : a \to a \]

**Axiom 5 (Left Action of $\ll$):**
\[ (f \ll p) \circ (g \ll q) = (f \cdot g) \ll (p \circ q) \quad \forall f, g \in \mathcal{C}, p, q \in \mathrm{H} \]

**Axiom 6 (Lift as Left Action):**
\[ \mathrm{lift}(f) = f \ll \mathrm{lift}(f) \quad \forall f \in \mathcal{C} \]

**Axiom 7 (Run Composition Rule):**
\[ \mathrm{run}((f \ll p) \circ q) = f(\mathrm{run}(q \circ p)) \quad \forall f \in \mathcal{C}, p, q \in \mathrm{H} \]

## Derived Operations

### Contravariant Functoriality

The mapping $\mathrm{H}(a, b) \mapsto \mathrm{H}(a', b')$ is contravariant in the first argument and covariant in the second:

\[
\mathrm{mapH} : \mathcal{C}(a', a) \times \mathcal{C}(b, b') \times \mathrm{H}(a, b) \to \mathrm{H}(a', b')
\]
\[
\mathrm{mapH}(r, s, f) = \mathrm{lift}(s) \circ f \circ \mathrm{lift}(r)
\]

### Invoke Operation

\[
\mathrm{invoke} : \mathrm{H}(a, b) \times \mathrm{H}(b, a) \to b
\]
\[
\mathrm{invoke}(f, g) = \mathrm{run}(f \circ g)
\]

### Base Injection

\[
\mathrm{base} : b \to \mathrm{H}(a, b)
\]
\[
\mathrm{base}(k) = \mathrm{lift}(\mathrm{const}_k)
\]

where $\mathrm{const}_k = \lambda x. k$ is the constant function.

### Equivalence of Definitions

\[
\mathrm{run}(f) = \mathrm{invoke}(f, \mathrm{self})
\]

## Faithfulness Theorem

**Theorem 3 (Faithfulness of Lift):**
The functor $\mathrm{lift} : \mathcal{C} \to \mathcal{H}$ is faithful, i.e.:
\[ \mathrm{lift}(f) = \mathrm{lift}(g) \implies f = g \quad \forall f, g \in \mathcal{C} \]

**Proof:**

Define the projection functor:
\[
\mathrm{project} : \mathrm{H}(a, b) \to \mathcal{C}(a, b)
\]
\[
\mathrm{project}(q)(x) = \mathrm{invoke}(q, \mathrm{base}(x))
\]

We show that $\mathrm{project} \circ \mathrm{lift} = \mathrm{id}_{\mathcal{C}}$:

\[
\begin{align*}
\mathrm{project}(\mathrm{lift}(f))(x) &= \mathrm{invoke}(\mathrm{lift}(f), \mathrm{base}(x)) \\
&= \mathrm{run}(\mathrm{lift}(f) \circ \mathrm{base}(x)) \\
&= \mathrm{run}((f \ll \mathrm{lift}(f)) \circ \mathrm{base}(x)) \\
&= f(\mathrm{run}(\mathrm{base}(x) \circ \mathrm{lift}(f))) \\
&= f(\mathrm{run}(\mathrm{lift}(\mathrm{const}_x) \circ \mathrm{lift}(f))) \\
&= f(\mathrm{run}((\mathrm{const}_x \ll \mathrm{base}(x)) \circ \mathrm{lift}(f))) \\
&= f(\mathrm{const}_x(\mathrm{run}(\mathrm{lift}(f) \circ \mathrm{base}(x)))) \\
&= f(x)
\end{align*}
\]

Thus $\mathrm{project}(\mathrm{lift}(f)) = f$, proving the faithfulness theorem.

## Model-Theoretic Properties

Without the $\ll$ operation and its associated axioms, the system admits a trivial model where $\mathrm{H}(a, b) = \mathcal{C}(a, b)$ with $\circ$ as ordinary function composition. The $\ll$ operation prevents this trivial model.

All hyperfunction models satisfy the property that distinct functions remain distinct when lifted to hyperfunctions. The category $\mathcal{H}$ thus contains a faithful copy of the base category $\mathcal{C}$.
