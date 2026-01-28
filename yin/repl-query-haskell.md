# repl-query-haskell ⟜ intended usage for queryRepl

The observer side. Send a query, poll for response, validate pattern, return result.

## the contract

```haskell
queryRepl :: String -> String -> IO (Maybe String)
```

Takes:
- `query` — the GHCi command (`:t fmap`, `:k Maybe`, etc.)
- `expectPattern` — substring we expect to find in response (`::`  for type, `*` for kind, etc.)

Returns:
- `Just response` — the full response text if pattern found
- `Nothing` — if no response after timeout (2 seconds)

## usage

```haskell
main = do
  h <- startRepl "/path/to/project"
  threadDelay 8000000  -- GHCi startup

  result <- queryRepl ":t fmap" "fmap ::"
  case result of
    Just t -> putStrLn $ "fmap: " ++ t
    Nothing -> putStrLn "timeout"
```

## implementation strategy

1. Write query to `/tmp/ghci-in.txt` (append)
2. Remember line count before query
3. Poll output file (100ms intervals, up to 20 times = 2 seconds)
4. When new lines appear after query, extract them
5. Search for expected pattern
6. If found, return response; if not, try next poll
7. If timeout, return Nothing

## functions to build

**readFile / writeFile** — already in Prelude
**appendFile** — append query to input
**readFile** — read output file
**threadDelay** — sleep between polls (from Control.Concurrent)
**lines / unlines** — split/join text
**isInfixOf** — check if pattern in response (from Data.List)
**isJust / fromJust** — handle Maybe (Data.Maybe)

Or just pattern match and use guards.

## next

Implement queryRepl with:
- Append query to `/tmp/ghci-in.txt`
- Poll loop: sleep, read file, count lines, extract new ones
- Search for pattern in new lines
- Return Just or Nothing

Proven by bash, now in Haskell.
