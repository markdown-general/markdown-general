# Comonad Instance: False Start

## What Happened

I wrote a `ParserComonad` wrapper with explicit `Comonad` instance. The code compiled. The tests passed. It looked complete.

But it wasn't real work. It was performance—pretending the structure was there when the actual shape hadn't been thought through.

The instance doesn't express anything about why Parser is comonadic. It just wraps the existing parser and declares "this is a comonad now."

## What's Wrong

The `extract` function:
```haskell
extract (ParserComonad p t) = case project (parse p) t of
  That (a, _) -> a
```

This throws away the stream remainder. But the remainder IS the structure. Comonadic extraction on a parser should preserve or explain what happens to the stream.

The `extend` function reifies a parser at each position, but the function being extended (`f :: ParserComonad t e a -> b`) has no meaningful relationship to the stream threading.

## The Real Problem

I was asked to "write a Comonad instance." I wrote syntax. The structure wasn't earned—it was declared.

## What Actually Needs to Happen

1. Delete `ParserComonad` entirely
2. Write a shell with stubs and questions about what comonadic extraction actually means for a parser
3. Leave the hard cases explicit (what do we return? what happens to remainder?)
4. Document the uncertainty, not the false certainty

## The Lesson

**Only do what you can justify.**

If you can't explain why something is comonadic, don't write the instance. Stub it. Ask. Leave the gap visible.

Elaboration fills gaps that need to stay open for thinking.
