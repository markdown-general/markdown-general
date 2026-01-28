# repl-query-response ⟜ ask a question, get a typed answer

You are an observer. You want to ask GHCi something and get a reliable answer back. You don't care about markers or synchronization protocols. You just follow this pattern.

## the pattern

1. **Write query** to `/tmp/ghci-in.txt`
2. **Wait** (50-500ms, your choice)
3. **Read entire** `/tmp/ghci-out.txt`
4. **Search** for your query echo in the output
5. **Extract** the response (next non-empty lines after the query echo)
6. **Validate** (does the response contain what you expected?)
7. **Decide** (got it? Ask next. Not yet? Loop or timeout.)

## Haskell signature

```haskell
queryRepl :: String -> String -> IO (Maybe String)
```

Takes:
- `query` — the ghci command (e.g., `:t fmap`)
- `expectPattern` — what you expect to see in the response (e.g., `fmap ::`)

Returns:
- `Just response` — full response containing the pattern
- `Nothing` — response not found after timeout

## implementation notes

Use `System.IO` file operations and `Data.Text` or `String`:

```haskell
queryRepl query expect = do
  -- Write query to input file
  appendFile "/tmp/ghci-in.txt" (query ++ "\n")
  
  -- Poll output file, up to N times
  let maxAttempts = 20  -- 20 * 100ms = 2 seconds
  tryUntilFound maxAttempts
  
  where
    tryUntilFound 0 = return Nothing
    tryUntilFound n = do
      threadDelay 100000  -- 100ms
      output <- readFile "/tmp/ghci-out.txt"
      case extractResponse query output of
        Just resp | expect `isInfixOf` resp -> return (Just resp)
        Just resp -> tryUntilFound (n - 1)  -- Found query but no expectation
        Nothing -> tryUntilFound (n - 1)    -- Query not in output yet
```

## helper: extract response

```haskell
extractResponse :: String -> String -> Maybe String
extractResponse query output =
  case dropWhile (not . (query `isInfixOf`)) (lines output) of
    [] -> Nothing  -- Query not found
    queryLine : rest ->
      let responseLines = takeWhile (not . isPrefixOf "Prelude>") rest
          cleanedLines = filter (not . null) . map strip $ responseLines
      in if null cleanedLines then Nothing else Just (unlines cleanedLines)
```

## workflow

```haskell
main = do
  -- Start repl
  ph <- startRepl "."
  
  -- Ask questions
  fmapType <- queryRepl ":t fmap" "fmap ::"
  case fmapType of
    Just t -> putStrLn $ "fmap type: " ++ t
    Nothing -> putStrLn "failed to get fmap type"
  
  -- Ask more
  maybeKind <- queryRepl ":k Maybe" "* -> *"
  case maybeKind of
    Just k -> putStrLn $ "Maybe kind: " ++ k
    Nothing -> putStrLn "failed to get Maybe kind"
```

## key insight

**No synchronization protocol.** No markers. No locks. No "is GHCi done?" tracking. You write, you read, you search for what you asked for. If it's there, you got it. If not, try again. That's the whole pattern.

The continuation is in the observer's logic, not in the infrastructure.

## next

Wire this into a type-checking loop for hyperfunction exploration. Ask about types. Get answers. Modify code. Ask again.

This is agentic type wrangling at the speed of thought.
