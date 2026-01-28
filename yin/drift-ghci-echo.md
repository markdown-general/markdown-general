# drift: GHCi echo behavior with piped input

## what we found

**Design assumption:** When you send a query to GHCi, it echoes the command back, then the response.

Example (from mock-repl):
```
Prelude> :t fmap
fmap :: Functor f => (a -> b) -> f a -> f b
```

**Reality:** When input is piped to GHCi (via stdin), it does NOT echo the command. It only shows the response and the prompt.

Example (real cabal repl):
```
ghci> fmap :: Functor f => (a -> b) -> f a -> f b
```

## why it matters

The observer pattern said:
- Write query
- Read output
- **Search for query echo + expected pattern**
- Extract response

When GHCi doesn't echo piped input, the observer can't find the query echo. It times out.

## what changed

**Before:** Observer looked for BOTH query echo AND response pattern.

```bash
if grep -q "$QUERY" "$OUTPUT" && grep -q "$EXPECT" "$OUTPUT"; then
  # found it
fi
```

**After:** Observer looks for response pattern ONLY.

```bash
if grep -q "$EXPECT" "$OUTPUT"; then
  # found it
fi
```

This works because queries are sent sequentially. By the time we're polling for the first query's response, no other responses are in the file yet. So if we see `fmap ::`, we know it's the response to `:t fmap`.

## protocol still holds

The core insight remains:
- Write query to file
- Wait
- Read output
- Search for expected pattern
- Move to next query

The difference: we search for the response pattern, not the command echo. The file-based protocol is **robust** because the observer doesn't need to know how the backend processes input. It only needs to know what the response looks like.

## mock vs. real

Mock-repl was designed to echo commands (like GHCi REPL in a terminal). Real cabal repl pipes input, doesn't echo. Both work with the same observer—just the search pattern changed.

This is why testing with mock first was valuable. We verified the protocol. Then reality adjusted one detail. The protocol absorbed the adjustment.

## next

1. Observer pattern is proven with both mock and real cabal repl
2. The file-based message passing works
3. Next step: wrap this into a Haskell library (repl-startup, queryRepl)
4. Use it for agentic type wrangling
