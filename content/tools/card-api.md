# card-api.md

Universal processor for sisyphus/ cards - extracts, compiles, tests, and benchmarks literate tools.

## what it does

Processes cards in content/tools/ by:
- Creating symlinks in artifacts/haskell-build/
- Parsing dependencies from cards
- Adding executable sections to haskell-build.cabal
- Building executables via cabal + markdown-unlit
- Running doctests and updating Status sections
- Installing to artifacts/bin/

## why card-api

**Single implementation** ⟜ all cards share the same operational logic
**Consistent interface** ⟜ every tool gets install, uninstall, test, benchmark, docs
**Dynamic cabal management** ⟜ adds/removes executables as cards are installed/uninstalled
**Language agnostic** ⟜ extensible to Python, C++, shell cards

## subcommands

**install** ⟜ symlink, add to cabal, build, and install card
**uninstall** ⟜ remove symlink, remove from cabal, remove binary
**test** ⟜ run doctests, update Status section
**benchmark** ⟜ measure performance, update Status section
**docs** ⟜ display card documentation

## dependencies

base, text, optparse-applicative, directory, filepath, process, perf

## code

```haskell top
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Options.Applicative
import System.Directory
  ( createDirectoryIfMissing
  , doesFileExist
  , pathIsSymbolicLink
  , createFileLink
  , makeAbsolute
  , getCurrentDirectory
  , setCurrentDirectory
  , removeFile
  )
import System.FilePath
import System.Process
import System.Exit (ExitCode(..), exitWith)
import Control.Monad (when, unless, replicateM)
import Control.Exception (catch, SomeException)
import Data.List (isInfixOf, sort)
import Perf (tickIO)
import Text.Printf (printf)
```

```haskell
-- | Command line options
data Command
  = Install
      { installCardPath :: FilePath
      , installVerbose :: Bool
      }
  | Uninstall
      { uninstallCardPath :: FilePath
      , uninstallVerbose :: Bool
      }
  | Test
      { testCardPath :: FilePath
      , testVerbose :: Bool
      }
  | Benchmark
      { benchmarkCardPath :: FilePath
      , benchmarkIterations :: Int
      , benchmarkVerbose :: Bool
      , benchmarkArgs :: [String]
      }
  | Docs
      { docsCardPath :: FilePath
      }

-- | Parse command line arguments
parseCommand :: Parser Command
parseCommand = subparser
  ( command "install" (info installParser (progDesc "Build and install card"))
  <> command "uninstall" (info uninstallParser (progDesc "Remove card and clean up"))
  <> command "test" (info testParser (progDesc "Run tests and update status"))
  <> command "benchmark" (info benchmarkParser (progDesc "Measure performance"))
  <> command "docs" (info docsParser (progDesc "Display documentation"))
  )

installParser :: Parser Command
installParser = Install
  <$> strArgument (metavar "CARD" <> help "Path to card markdown file")
  <*> switch (long "verbose" <> help "Show detailed output")

uninstallParser :: Parser Command
uninstallParser = Uninstall
  <$> strArgument (metavar "CARD" <> help "Path to card markdown file")
  <*> switch (long "verbose" <> help "Show detailed output")

testParser :: Parser Command
testParser = Test
  <$> strArgument (metavar "CARD" <> help "Path to card markdown file")
  <*> switch (long "verbose" <> help "Show detailed output")

benchmarkParser :: Parser Command
benchmarkParser = Benchmark
  <$> strArgument (metavar "CARD" <> help "Path to card markdown file")
  <*> option auto (long "iterations" <> short 'n' <> value 10 <> help "Number of benchmark iterations")
  <*> switch (long "verbose" <> short 'v' <> help "Show detailed output")
  <*> many (strArgument (metavar "ARGS..." <> help "Arguments to pass to executable"))

docsParser :: Parser Command
docsParser = Docs
  <$> strArgument (metavar "CARD" <> help "Path to card markdown file")

-- | Main entry point
main :: IO ()
main = do
  cmd <- execParser opts
  case cmd of
    Install {installCardPath = card, installVerbose = v} -> installCard card v
    Uninstall {uninstallCardPath = card, uninstallVerbose = v} -> uninstallCard card v
    Test {testCardPath = card, testVerbose = v} -> testCard card v
    Benchmark {benchmarkCardPath = card, benchmarkIterations = n, benchmarkVerbose = v, benchmarkArgs = args} -> benchmarkCard card n v args
    Docs {docsCardPath = card} -> showDocs card
  where
    opts = info (parseCommand <**> helper)
      ( fullDesc
     <> progDesc "Universal processor for sisyphus cards"
     <> header "card-api - literate tool management" )
```

```haskell
-- | Install a card: symlink, add to cabal, build, install to bin/
installCard :: FilePath -> Bool -> IO ()
installCard cardPath verboseMode = do
  when verboseMode $ showDocs cardPath
  
  let buildDir = "artifacts/haskell-build"
  let cardName = takeBaseName cardPath
  let lhsName = cardName <.> "lhs"
  let lhsPath = buildDir </> lhsName
  
  createDirectoryIfMissing True buildDir
  
  absoluteCardPath <- makeAbsolute cardPath
  let relativePath = makeRelative buildDir absoluteCardPath
  
  symlinkExists <- pathIsSymbolicLink lhsPath `catch` \(_ :: SomeException) -> return False
  unless symlinkExists $ do
    when verboseMode $ putStrLn $ "Creating symlink: " ++ lhsPath
    createFileLink relativePath lhsPath
  
  ensureCabalProject buildDir verboseMode
  
  deps <- parseDependencies cardPath
  when verboseMode $ putStrLn $ "Dependencies: " ++ deps
  
  addExecutableToCabal buildDir cardName deps verboseMode
  
  when verboseMode $ putStrLn "Building with cabal..."
  currentDir <- getCurrentDirectory
  setCurrentDirectory buildDir
  (exitCode, stdout, stderr) <- readProcessWithExitCode "cabal" ["build", "all"] ""
  
  when verboseMode $ do
    putStrLn stdout
    unless (null stderr) $ putStrLn stderr
  
  case exitCode of
    ExitSuccess -> do
      when verboseMode $ putStrLn "Installing to artifacts/bin/..."
      
      -- Use cabal install (still in buildDir)
      (installExitCode, installStdout, installStderr) <- readProcessWithExitCode "cabal" 
        ["install", cardName, "--installdir=../bin", "--overwrite-policy=always"] ""
      
      setCurrentDirectory currentDir
      
      when verboseMode $ do
        putStrLn installStdout
        unless (null installStderr) $ putStrLn installStderr
      
      case installExitCode of
        ExitSuccess -> putStrLn $ "Installed: artifacts/bin/" ++ cardName
        ExitFailure code -> do
          putStrLn $ "Install failed with exit code: " ++ show code
          putStrLn installStderr
          exitWith (ExitFailure code)
    ExitFailure code -> do
      putStrLn $ "Build failed with exit code: " ++ show code
      putStrLn stderr
      setCurrentDirectory currentDir
      exitWith (ExitFailure code)

-- | Uninstall a card: remove symlink, remove from cabal, remove binary
uninstallCard :: FilePath -> Bool -> IO ()
uninstallCard cardPath verboseMode = do
  let buildDir = "artifacts/haskell-build"
  let cardName = takeBaseName cardPath
  let lhsPath = buildDir </> cardName <.> "lhs"
  let binPath = "artifacts/bin" </> cardName
  
  when verboseMode $ putStrLn $ "Removing symlink: " ++ lhsPath
  symlinkExists <- pathIsSymbolicLink lhsPath `catch` \(_ :: SomeException) -> return False
  when symlinkExists $ removeFile lhsPath
  
  when verboseMode $ putStrLn "Removing from cabal file..."
  removeExecutableFromCabal buildDir cardName verboseMode
  
  when verboseMode $ putStrLn $ "Removing binary: " ++ binPath
  binExists <- doesFileExist binPath
  when binExists $ removeFile binPath
  
  putStrLn $ "Uninstalled: " ++ cardName

-- | Parse dependencies from card's ## dependencies section
parseDependencies :: FilePath -> IO String
parseDependencies cardPath = do
  content <- TIO.readFile cardPath
  let depLine = findDependencies (T.lines content)
  return $ maybe "base" T.unpack depLine
  where
    findDependencies [] = Nothing
    findDependencies (line:rest)
      | "## dependencies" `T.isPrefixOf` line = 
          case dropWhile T.null rest of  -- Skip empty lines
            (nextLine:_) | not (T.isPrefixOf "##" nextLine) -> Just (T.strip nextLine)
            _ -> Nothing
      | otherwise = findDependencies rest

-- | Parse benchmark scenarios from card's ## benchmarks section
-- Returns list of (name, args)
parseBenchmarks :: FilePath -> IO [(String, [String])]
parseBenchmarks cardPath = do
  content <- TIO.readFile cardPath
  return $ findBenchmarks (T.lines content)
  where
    findBenchmarks [] = []
    findBenchmarks (line:rest)
      | "## benchmarks" `T.isPrefixOf` line = parseBenchmarkLines rest
      | otherwise = findBenchmarks rest
    
    parseBenchmarkLines [] = []
    parseBenchmarkLines (line:rest)
      | "##" `T.isPrefixOf` line = []  -- Hit next section
      | "**" `T.isPrefixOf` line = 
          case parseBenchmarkLine line of
            Just benchmark -> benchmark : parseBenchmarkLines rest
            Nothing -> parseBenchmarkLines rest
      | otherwise = parseBenchmarkLines rest
    
    parseBenchmarkLine line =
      let stripped = T.strip line
          -- Pattern: **name** ⟜ args or **name**: args
          withoutStars = T.dropWhile (== '*') stripped
          nameAndRest = T.breakOn "**" withoutStars
      in case nameAndRest of
        (name, rest) | not (T.null rest) ->
          let afterName = T.drop 2 rest  -- Drop the closing **
              argsText = T.strip $ T.dropWhile (\c -> c == '⟜' || c == ':' || c == ' ') afterName
              args = map T.unpack $ T.words argsText
          in if T.null name then Nothing else Just (T.unpack (T.strip name), args)
        _ -> Nothing

-- | Add executable section to cabal file
addExecutableToCabal :: FilePath -> String -> String -> Bool -> IO ()
addExecutableToCabal buildDir name deps verboseMode = do
  let cabalFile = buildDir </> "haskell-build.cabal"
  content <- readFile cabalFile
  
  -- Ensure base is included
  let allDeps = if "base" `isInfixOf` deps 
                then deps 
                else "base, " ++ deps
  
  let execSection = unlines
        [ ""
        , "executable " ++ name
        , "  import: deps"
        , "  main-is: " ++ name ++ ".lhs"
        , "  build-depends: " ++ allDeps
        ]
  
  if ("executable " ++ name) `isInfixOf` content
    then when verboseMode $ putStrLn $ "Executable " ++ name ++ " already in cabal file"
    else do
      when verboseMode $ putStrLn $ "Adding executable " ++ name ++ " to cabal file"
      writeFile cabalFile (content ++ execSection)

-- | Remove executable section from cabal file
removeExecutableFromCabal :: FilePath -> String -> Bool -> IO ()
removeExecutableFromCabal buildDir name verboseMode = do
  let cabalFile = buildDir </> "haskell-build.cabal"
  content <- readFile cabalFile
  let contentLines = lines content
  let filtered = removeExecutableSection name contentLines
  writeFile cabalFile (unlines filtered)
  when verboseMode $ putStrLn $ "Removed executable " ++ name ++ " from cabal file"

-- | Remove executable section from cabal file lines
removeExecutableSection :: String -> [String] -> [String]
removeExecutableSection name = go False
  where
    execMarker = "executable " ++ name
    go _ [] = []
    go True (line:rest)
      | null (dropWhile (== ' ') line) = go False rest
      | otherwise = case dropWhile (== ' ') line of
          (c:_) | c /= ' ' -> go False (line:rest)
          _ -> go True rest
    go False (line:rest)
      | execMarker `isInfixOf` line = go True rest
      | otherwise = line : go False rest

-- | Ensure cabal project exists in build directory
ensureCabalProject :: FilePath -> Bool -> IO ()
ensureCabalProject buildDir verboseMode = do
  let cabalFile = buildDir </> "haskell-build.cabal"
  exists <- doesFileExist cabalFile
  unless exists $ do
    when verboseMode $ putStrLn "Warning: haskell-build.cabal not found. Run haskell-build-bootstrap.md first"
    putStrLn "Error: No cabal project found. Run haskell-build-bootstrap.md"
    exitWith (ExitFailure 1)

-- | Run an executable and time it with tickIO
-- tickIO returns nanoseconds, we convert to seconds
timeExecution :: FilePath -> [String] -> IO (Either String Double)
timeExecution executable args = do
  exists <- doesFileExist executable
  if not exists
    then return $ Left $ "Executable not found: " ++ executable
    else do
      (t, (exitCode, _, stderr)) <- tickIO $ 
        readProcessWithExitCode executable args ""
      
      case exitCode of
        ExitSuccess -> return $ Right (fromIntegral t * 1e-9)
        ExitFailure code -> return $ Left $ 
          "Execution failed (code " ++ show code ++ "): " ++ stderr

-- | Run benchmark N times and collect timings
runBenchmarkN :: FilePath -> [String] -> Int -> IO (Either String [Double])
runBenchmarkN executable args n = do
  results <- replicateM n (timeExecution executable args)
  let (errs, times) = partitionEithers results
  if null errs
    then return $ Right times
    else return $ Left $ head errs
  where
    partitionEithers = foldr (either left right) ([], [])
    left a (ls, rs) = (a:ls, rs)
    right b (ls, rs) = (ls, b:rs)

-- | Calculate statistics from timing data (all in seconds)
data Stats = Stats
  { statMedian :: Double
  , statMean :: Double
  , statMin :: Double
  , statMax :: Double
  }

calcStats :: [Double] -> Stats
calcStats xs = Stats
  { statMedian = sorted !! (length sorted `div` 2)
  , statMean = sum xs / fromIntegral (length xs)
  , statMin = minimum xs
  , statMax = maximum xs
  }
  where
    sorted = sort xs

-- | Format timing as human-readable string (input in seconds)
formatTiming :: Double -> String
formatTiming seconds
  | seconds < 0.001 = printf "%.1fµs" (seconds * 1e6)
  | seconds < 1.0   = printf "%.2fms" (seconds * 1e3)
  | otherwise       = printf "%.2fs" seconds

-- | Format stats for Status section
formatStats :: Stats -> Int -> T.Text
formatStats stats n = T.pack $
  formatTiming (statMedian stats) ++ 
  " (median over " ++ show n ++ " runs)"

-- | Run doctests and update status
testCard :: FilePath -> Bool -> IO ()
testCard cardPath verboseMode = do
  when verboseMode $ putStrLn "Running doctests..."
  
  let buildDir = "artifacts/haskell-build"
  let cardName = takeBaseName cardPath
  
  currentDir <- getCurrentDirectory
  setCurrentDirectory buildDir
  
  (exitCode, stdout, stderr) <- readProcessWithExitCode "cabal" 
    ["repl", "--with-ghc=doctest", cardName] ""
  
  setCurrentDirectory currentDir
  
  when verboseMode $ do
    putStrLn stdout
    putStrLn stderr
  
  let status = case exitCode of
        ExitSuccess -> "✓ passed"
        ExitFailure _ -> "✗ failed"
  
  updateStatus cardPath "Tests" status
  
  case exitCode of
    ExitSuccess -> putStrLn "Tests passed"
    ExitFailure code -> do
      putStrLn "Tests failed"
      putStrLn stderr
      exitWith (ExitFailure code)

-- | Run benchmarks and update status
benchmarkCard :: FilePath -> Int -> Bool -> [String] -> IO ()
benchmarkCard cardPath iterations verboseMode args = do
  when verboseMode $ putStrLn "Running benchmarks..."
  
  let cardName = takeBaseName cardPath
  let executable = "artifacts/bin" </> cardName
  
  if null args
    then do
      -- No args provided - run benchmark suite from card
      benchmarks <- parseBenchmarks cardPath
      if null benchmarks
        then do
          putStrLn "No benchmark suite found in card and no args provided"
          putStrLn "Either:"
          putStrLn "  1. Add a ## benchmarks section to the card, or"
          putStrLn "  2. Pass args: card-api benchmark CARD -- <args>"
          exitWith (ExitFailure 1)
        else do
          when verboseMode $ putStrLn $ "Found " ++ show (length benchmarks) ++ " benchmark scenarios"
          results <- mapM (runNamedBenchmark executable iterations verboseMode) benchmarks
          let formatted = formatBenchmarkSuite results iterations
          updateStatus cardPath "Benchmark" formatted
          putStrLn "Benchmark suite complete"
    else do
      -- Args provided - run single ad-hoc benchmark
      when verboseMode $ do
        putStrLn $ "Executable: " ++ executable
        putStrLn $ "Args: " ++ unwords args
      
      result <- runBenchmarkN executable args iterations
      
      case result of
        Left err -> do
          putStrLn $ "Benchmark failed: " ++ err
          updateStatus cardPath "Benchmark" (T.pack $ "✗ " ++ err)
        Right times -> do
          let stats = calcStats times
          let formatted = formatStats stats iterations
          
          when verboseMode $ do
            putStrLn $ "Min:    " ++ formatTiming (statMin stats)
            putStrLn $ "Median: " ++ formatTiming (statMedian stats)
            putStrLn $ "Mean:   " ++ formatTiming (statMean stats)
            putStrLn $ "Max:    " ++ formatTiming (statMax stats)
          
          putStrLn $ "Benchmark: " ++ T.unpack formatted
          updateStatus cardPath "Benchmark" formatted

-- | Run a named benchmark scenario
runNamedBenchmark :: FilePath -> Int -> Bool -> (String, [String]) -> IO (String, Either String Stats)
runNamedBenchmark executable iterations verboseMode (name, args) = do
  when verboseMode $ putStrLn $ "\nRunning: " ++ name
  when verboseMode $ putStrLn $ "  Args: " ++ unwords args
  
  result <- runBenchmarkN executable args iterations
  
  case result of
    Left err -> do
      when verboseMode $ putStrLn $ "  Failed: " ++ err
      return (name, Left err)
    Right times -> do
      let stats = calcStats times
      when verboseMode $ do
        putStrLn $ "  Min:    " ++ formatTiming (statMin stats)
        putStrLn $ "  Median: " ++ formatTiming (statMedian stats)
        putStrLn $ "  Mean:   " ++ formatTiming (statMean stats)
        putStrLn $ "  Max:    " ++ formatTiming (statMax stats)
      return (name, Right stats)

-- | Format benchmark suite results for Status section
formatBenchmarkSuite :: [(String, Either String Stats)] -> Int -> T.Text
formatBenchmarkSuite results iterations =
  let lines = map formatResult results
  in T.intercalate "\n" lines
  where
    formatResult (name, Left err) = T.pack $ "- " ++ name ++ ": ✗ " ++ err
    formatResult (name, Right stats) = 
      T.pack $ "- " ++ name ++ ": " ++ formatTiming (statMedian stats) ++ 
               " (median over " ++ show iterations ++ " runs)"

-- | Display card documentation
showDocs :: FilePath -> IO ()
showDocs cardPath = do
  content <- readFile cardPath
  putStrLn content

-- | Update Status section in card
updateStatus :: FilePath -> T.Text -> T.Text -> IO ()
updateStatus cardPath field val = do
  content <- TIO.readFile cardPath
  let updated = replaceStatusField content field val
  TIO.writeFile cardPath updated

-- | Replace a field in the Status section
-- Handles both single-line and multi-line values
replaceStatusField :: T.Text -> T.Text -> T.Text -> T.Text
replaceStatusField content field val =
  let fieldPattern = "**" <> field <> ":**"
      newValue = if "\n" `T.isPrefixOf` val
                 then fieldPattern <> val  -- Multi-line: starts with newline
                 else fieldPattern <> " " <> val  -- Single-line: add space
      contentLines = T.lines content
      updated = replaceFieldLines fieldPattern newValue contentLines
  in T.unlines updated
  where
    replaceFieldLines :: T.Text -> T.Text -> [T.Text] -> [T.Text]
    replaceFieldLines pattern new [] = []
    replaceFieldLines pattern new (line:rest)
      | pattern `T.isPrefixOf` line =
          -- Found the field, replace it and skip old multi-line content
          let newLines = T.lines new
              restAfter = dropWhile isFieldContinuation rest
          in newLines ++ replaceFieldLines pattern new restAfter
      | otherwise = line : replaceFieldLines pattern new rest
    
    -- A line is part of the field if it starts with "- " (list item)
    isFieldContinuation :: T.Text -> Bool
    isFieldContinuation line =
      let trimmed = T.stripStart line
      in "- " `T.isPrefixOf` trimmed && not ("**" `T.isPrefixOf` trimmed)
```

```haskell
{- cabal: 
build-depends: base, text, optparse-applicative, directory, filepath, process, perf
default-language: Haskell2010
build-tool-depends: markdown-unlit:markdown-unlit
ghc-options: -pgmL markdown-unlit -Wall
-}
```

## run

Prerequisites:
```bash
# Run haskell-build-bootstrap.md once
cd ~/sisyphus
# Extract and run from the markdown file (see haskell-build-bootstrap.md)
```

Then use card-api:

```bash
# Install a card
card-api install ~/sisyphus/content/tools/flatten-md.md

# Test a card
card-api test ~/sisyphus/content/tools/flatten-md.md

# Benchmark a card (runs suite from card's ## benchmarks section)
card-api benchmark ~/sisyphus/content/tools/flatten-md.md --iterations 100 --verbose

# Or ad-hoc benchmark with custom args (use -- separator)
card-api benchmark ~/sisyphus/content/tools/flatten-md.md --iterations 10 --verbose -- flatten input.txt output.md

# Uninstall a card
card-api uninstall ~/sisyphus/content/tools/flatten-md.md
```

## examples

### install a card

```bash
card-api install ~/sisyphus/content/tools/flatten-md.md --verbose
```

Result: 
- Symlink created in artifacts/haskell-build/
- Executable section added to haskell-build.cabal
- Binary built and installed to artifacts/bin/

### uninstall a card

```bash
card-api uninstall ~/sisyphus/content/tools/flatten-md.md --verbose
```

Result:
- Symlink removed
- Executable section removed from cabal file
- Binary removed from artifacts/bin/

### test a card

```bash
card-api test ~/sisyphus/content/tools/flatten-md.md
```

Result: Status section updated with test results

### benchmark a card

```bash
# Run benchmark suite (reads ## benchmarks section from card)
card-api benchmark ~/sisyphus/content/tools/flatten-md.md --iterations 100 --verbose

# Or pass args for ad-hoc benchmark (use -- to separate flags from args)
card-api benchmark ~/sisyphus/content/tools/flatten-md.md --iterations 100 --verbose -- flatten test-input.txt output.md
```

Example `## benchmarks` section in card:
```markdown
## benchmarks

**flatten-base** ⟜ flatten --input-dir content/base/ --output /tmp/flat.md
**unflatten-base** ⟜ unflatten --input /tmp/flat.md --output-dir /tmp/out
```

Result (benchmark suite):
```
Running benchmarks...
Found 2 benchmark scenarios

Running: flatten-base
  Args: flatten --input-dir content/base/ --output /tmp/flat.md
  Min:    15.88ms
  Median: 17.88ms
  Mean:   17.91ms
  Max:    21.58ms

Running: unflatten-base
  Args: unflatten --input /tmp/flat.md --output-dir /tmp/out
  Min:    10.12ms
  Median: 11.45ms
  Mean:   11.52ms
  Max:    13.21ms

Benchmark suite complete
```

Status section updated to:
```markdown
**Benchmark:**
- flatten-base: 17.88ms (median over 100 runs)
- unflatten-base: 11.45ms (median over 100 runs)
```

## status

**Tests:** ✓ passed - doctests working
**Benchmark:** not yet run
**Last updated:** 2025-12-31

## upstream

**Dependency tracking simplification** ⟜ retire custom dependency parsing in favor of cabal metadata
- Remove parseDependencies, addExecutableToCabal, removeExecutableFromCabal functions
- Let cabal handle dependency resolution automatically
- Simplify card-api.md codebase

**Multi-language support** ⟜ currently Haskell-only
- Python cards via sed extraction (existing pattern)
- C++ cards via gcc compilation
- Detect language from fence info string
