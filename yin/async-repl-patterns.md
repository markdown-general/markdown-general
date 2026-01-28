# async-repl-patterns ⟜ exploring Control.Concurrent.Async for repl wiring

The polling architecture wastes cycles. `async` library offers composable concurrency primitives that can eliminate manual polling and file-locking contention.

## the problem we're solving

Current architecture:
- inputThread polls input file every 50ms (continuous background work)
- queryRepl polls output file every 50ms per query (polling loop)
- Both read same files → contention, locks, timeouts

**800+ file reads in 30 seconds** for a 3-query session.

## async primitives

**async :: IO a -> IO (Async a)** — Spawn concurrent action, get handle

**wait :: Async a -> IO a** — Block until action completes, return result

**poll :: Async a -> IO (Maybe a)** — Check if action completed without blocking

**waitCatch :: Async a -> IO (Either SomeException a)** — Wait and catch exceptions

**race :: IO a -> IO b -> IO (Either a b)** — Run two actions, return first to finish

**concurrently :: IO a -> IO b -> IO (a, b)** — Run two actions, wait for both

**waitAny :: [Async a] -> IO (Async a, a)** — Wait for any of many actions

## new design

Instead of inputThread + queryRepl polling:

### startRepl returns an extra: an action that reads from input file

```haskell
startRepl :: FilePath -> IO (ProcessHandle, IO String)
```

Returns:
- ProcessHandle (the running cabal repl)
- IO String: action that reads /tmp/ghci-in.txt and blocks until new content

### queryRepl uses race to wait for either response or timeout

```haskell
queryRepl :: String -> String -> IO (Maybe String)
queryRepl query expect = do
  appendFile "/tmp/ghci-in.txt" (query ++ "\n")

  -- Race: either wait 3 seconds, or read output file when it changes
  result <- race (threadDelay 3000000) (waitForOutput expect)
  case result of
    Left () -> return Nothing  -- timeout
    Right resp -> return (Just resp)

waitForOutput :: String -> IO String
waitForOutput pattern = do
  -- Poll output file, but use async to make it non-blocking at call sites
  async (pollUntilFound pattern) >>= wait
```

## key insight: reduce polling frequency

Instead of 50ms polling everywhere, use **waitAny** to wait for the first of:
1. Output file change (via filesystem watcher, if available)
2. Timeout expires
3. Process exits

This is the FSNotify approach—watch file changes instead of polling.

Or simpler: **use async to make one slow poller instead of two competing ones**:

```haskell
-- Single input watcher thread
inputWatcher :: IO ()
inputWatcher = loop 0
  where
    loop linesSeen = do
      contents <- readFile "/tmp/ghci-in.txt"
      let newLines = drop linesSeen (lines contents)
      mapM_ (hPutStrLn stdin) newLines
      threadDelay 100000
      loop (length (lines contents))

-- queryRepl doesn't poll; it just waits for the input watcher + output file changes
queryRepl query expect = do
  appendFile "/tmp/ghci-in.txt" (query ++ "\n")
  race (threadDelay 3000000) (blockUntilPattern expect)
```

## patterns to explore via repl

```
:i async
:t race
:t waitCatch
:t poll
:t waitAny
:i Async
```

## next step

1. Query async types in repl
2. Card a concrete design (single input thread, race-based queryRepl)
3. Implement
4. Measure polling frequency (goal: <50 reads per query, not 60+)

async is perfect for this because:
- **race**: wait for response or timeout without polling
- **waitCatch**: handle process crashes gracefully
- **poll**: check completion without blocking other operations
- **Async**: composable handles, no manual thread management
