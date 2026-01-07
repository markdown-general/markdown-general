# card-api.md
Universal processor for markdown-general/ cards - extracts, compiles, tests, and installs literate tools with multiple executables.
## what it does
Processes cards in zone/tools/ by:
- Creating symlinks for main executable in artifacts/haskell-build/
- Extracting tagged fence blocks to separate executables
- Building all executables via cabal + markdown-unlit
- Running tests (doctests + all tagged executables) and updating Status sections
- Installing all executables to artifacts/bin/
## why card-api
**Single card, multiple programs** ⟜ main executable + tagged executables from any tagged blocks
**Consistent interface** ⟜ every tool gets install, uninstall, test, docs
**Flexible testing** ⟜ doctests + any executables, all treated uniformly
**Inline metadata** ⟜ cards are self-contained with cabal dependencies
## subcommands
**install** ⟜ extract, build, and install all executables from card
**uninstall** ⟜ remove symlinks and all binaries
**test** ⟜ run tests, update Status (all tests or --only specific)
**docs** ⟜ display card documentation
## dependencies
base, text, optparse-applicative, directory, filepath, process, perf
## code
```haskell main
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Encoding (encodeUtf8)
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
  , listDirectory
  )
import System.FilePath
import System.Process
import System.Exit (ExitCode(..), exitWith)
import Control.Monad (when, unless, forM_)
import Control.Exception (catch, SomeException)
import Data.List (isInfixOf, isPrefixOf, nub)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import Perf (tickIO)
import Text.Printf (printf)
```
```haskell main
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
      , testOnly :: Maybe String
      }
  | Benchmark
      { benchCardPath :: FilePath
      , benchName :: String
      , benchArgs :: [String]  -- Pass-through args for perf options (-r, --stat, etc.)
      , benchVerbose :: Bool
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
  <> command "benchmark" (info benchmarkParser (progDesc "Run benchmark with perf options"))
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
  <*> optional (strOption (long "only" <> help "Run only specific test"))
benchmarkParser :: Parser Command
benchmarkParser = Benchmark
  <$> strArgument (metavar "CARD" <> help "Path to card markdown file")
  <*> strArgument (metavar "BENCHMARK" <> help "Benchmark name to run")
  <*> many (strArgument (metavar "ARGS" <> help "Perf options (-r, --stat, etc.)"))
  <*> switch (long "verbose" <> help "Show detailed output")
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
    Test {testCardPath = card, testVerbose = v, testOnly = only} -> testCard card v only
    Benchmark {benchCardPath = card, benchName = name, benchArgs = args, benchVerbose = v} -> 
      benchmarkCard card name args v
    Docs {docsCardPath = card} -> showDocs card
  where
    opts = info (parseCommand <**> helper)
      ( fullDesc
     <> progDesc "Universal processor for sisyphus cards"
     <> header "card-api - literate tool management" )
```
```haskell main
-- | Parse fence block tags from card to find tagged executables
parseTaggedExecutables :: FilePath -> IO [String]
parseTaggedExecutables cardPath = do
  content <- TIO.readFile cardPath
  return $ nub $ extractTags (T.lines content)
  where
    extractTags :: [T.Text] -> [String]
    extractTags [] = []
    extractTags (line:rest)
      | "```haskell " `T.isPrefixOf` line =
          let tag = T.unpack $ T.strip $ T.drop 11 line
          in if tag /= ""
             then tag : extractTags rest
             else extractTags rest
      | otherwise = extractTags rest
-- | Install a card: extract all executables, build, install
installCard :: FilePath -> Bool -> IO ()
installCard cardPath verboseMode = do
  when verboseMode $ showDocs cardPath
  let buildDir = "artifacts/haskell-build"
  let cardName = takeBaseName cardPath
  createDirectoryIfMissing True buildDir
  createDirectoryIfMissing True "artifacts/bin"
  absoluteCardPath <- makeAbsolute cardPath
  let relativePath = makeRelative buildDir absoluteCardPath
  -- Find ALL tagged executables (including main)
  taggedExecs <- parseTaggedExecutables cardPath
  when verboseMode $ putStrLn $ "Found tagged executables: " ++ show taggedExecs
  -- Extract all executables using markdown-unlit
  currentDir <- getCurrentDirectory
  setCurrentDirectory buildDir
  forM_ taggedExecs $ \tag -> do
    let fileName = if tag == "main"
                   then cardName <.> "hs"
                   else cardName ++ "-" ++ tag <.> "hs"
    when verboseMode $ putStrLn $ "Extracting " ++ tag ++ " to " ++ fileName
    (exitCode, stdout, stderr) <- readProcessWithExitCode "markdown-unlit"
      [tag, "-h", relativePath, relativePath, fileName] ""
    case exitCode of
      ExitSuccess -> return ()  -- File already written by markdown-unlit
      ExitFailure _ -> do
        putStrLn $ "Failed to extract " ++ tag ++ ": " ++ stderr
        setCurrentDirectory currentDir
        exitWith (ExitFailure 1)
  -- Regenerate cabal file from directory contents
  when verboseMode $ putStrLn "Regenerating cabal file..."
  (cabalGenCode, cabalStdout, cabalStderr) <- readProcessWithExitCode "discover-executables" [] ""
  when verboseMode $ putStrLn cabalStdout
  case cabalGenCode of
    ExitFailure code -> do
      setCurrentDirectory currentDir
      putStrLn "Failed to regenerate cabal file"
      putStrLn cabalStderr
      exitWith (ExitFailure code)
    ExitSuccess -> return ()
  -- Build only this card's executables
  let tagToExec tag = if tag == "main" then cardName else cardName ++ "-" ++ tag
  let cardExecs = map tagToExec taggedExecs
  when verboseMode $ putStrLn $ "Building executables: " ++ unwords cardExecs
  (exitCode, stdout, stderr) <- readProcessWithExitCode "cabal"
    (["build"] ++ cardExecs) ""
  when verboseMode $ do
    putStrLn stdout
    unless (null stderr) $ putStrLn stderr
  case exitCode of
    ExitSuccess -> do
      when verboseMode $ putStrLn "Installing to artifacts/bin/..."
      -- Install only this card's executables
      (installExitCode, installStdout, installStderr) <- readProcessWithExitCode "cabal"
        (["install"] ++ cardExecs ++ ["--installdir=../bin", "--overwrite-policy=always"]) ""
      setCurrentDirectory currentDir
      when verboseMode $ do
        putStrLn installStdout
        unless (null installStderr) $ putStrLn installStderr
      case installExitCode of
        ExitSuccess -> do
          forM_ cardExecs $ \execName ->
            putStrLn $ "Installed: artifacts/bin/" ++ execName
        ExitFailure code -> do
          putStrLn $ "Install failed with exit code: " ++ show code
          putStrLn installStderr
          exitWith (ExitFailure code)
    ExitFailure code -> do
      putStrLn $ "Build failed with exit code: " ++ show code
      putStrLn stderr
      setCurrentDirectory currentDir
      exitWith (ExitFailure code)
-- | Uninstall a card: remove extracted files and binaries
uninstallCard :: FilePath -> Bool -> IO ()
uninstallCard cardPath verboseMode = do
  let buildDir = "artifacts/haskell-build"
  let cardName = takeBaseName cardPath
  
  -- Find all tagged executables
  taggedExecs <- parseTaggedExecutables cardPath
  
  -- Remove all extracted files and binaries
  forM_ taggedExecs $ \tag -> do
    let fileName = if tag == "main"
                   then buildDir </> cardName <.> "hs"
                   else buildDir </> cardName ++ "-" ++ tag <.> "hs"
    let binName = if tag == "main"
                  then "artifacts/bin" </> cardName
                  else "artifacts/bin" </> cardName ++ "-" ++ tag
    
    when verboseMode $ putStrLn $ "Removing: " ++ fileName
    fileExists <- doesFileExist fileName
    when fileExists $ removeFile fileName
    
    when verboseMode $ putStrLn $ "Removing: " ++ binName
    binExists <- doesFileExist binName
    when binExists $ removeFile binName
  
  -- Regenerate cabal file from remaining files
  when verboseMode $ putStrLn "Regenerating cabal file..."
  currentDir <- getCurrentDirectory
  setCurrentDirectory buildDir
  (cabalGenCode, cabalStdout, cabalStderr) <- readProcessWithExitCode "discover-executables" [] ""
  setCurrentDirectory currentDir
  
  when verboseMode $ putStrLn cabalStdout
  
  case cabalGenCode of
    ExitFailure code -> do
      putStrLn "Warning: Failed to regenerate cabal file"
      putStrLn cabalStderr
    ExitSuccess -> return ()
  
  -- Reset Status section
  when verboseMode $ putStrLn "Resetting Status section..."
  content <- TIO.readFile cardPath
  let resetContent = resetStatusSection content
  TIO.writeFile cardPath resetContent
  
  putStrLn $ "Uninstalled: " ++ cardName
-- | Reset Status section to "not yet run"
resetStatusSection :: T.Text -> T.Text
resetStatusSection content =
  let contentLines = T.lines content
      inStatus = scanl checkStatus False contentLines
      resetLines = zipWith resetIfTestResult inStatus contentLines
      filtered = filter (not . T.null) resetLines
  in T.unlines filtered
  where
    checkStatus wasInStatus line
      | "## status" `T.isPrefixOf` T.toLower line = True
      | "##" `T.isPrefixOf` line = False
      | otherwise = wasInStatus
    
    resetIfTestResult inStatusSection line
      | inStatusSection && "**Tests:**" `T.isPrefixOf` line = "**Tests:** not yet run"
      | inStatusSection && "- " `T.isPrefixOf` T.stripStart line = ""  -- Remove test results
      | otherwise = line
```
```haskell main
-- | Test result
data TestResult
  = TestPassed String     -- Single line success
  | TestFailed String     -- Single line error
  | TestTimed Double      -- Timing in seconds
  | TestMultiLine [String] -- Multi-line output (e.g. perf reports)
-- | Format test result for Status section
formatTestResult :: TestResult -> T.Text
formatTestResult (TestPassed msg) = T.pack $ "✓ " ++ msg
formatTestResult (TestFailed msg) = T.pack $ "✗ " ++ msg
formatTestResult (TestTimed t) = T.pack $ formatTiming t
formatTestResult (TestMultiLine outputLines) = 
  -- Format as: "✓\n    line1\n    line2..."
  T.pack $ "✓\n" ++ unlines (map ("    " ++) outputLines)
-- | Format timing in appropriate units
formatTiming :: Double -> String
formatTiming seconds
  | seconds < 0.001 = printf "%.2fμs" (seconds * 1e6)
  | seconds < 1.0   = printf "%.2fms" (seconds * 1e3)
  | otherwise       = printf "%.2fs" seconds
-- | Run doctests for a card
runDoctests :: FilePath -> Bool -> IO TestResult
runDoctests cardPath verboseMode = do
  let cardName = takeBaseName cardPath
  let buildDir = "artifacts/haskell-build"
  
  when verboseMode $ putStrLn $ "Running doctests for " ++ cardName ++ "..."
  
  currentDir <- getCurrentDirectory
  setCurrentDirectory buildDir
  
  (exitCode, stdout, stderr) <- readProcessWithExitCode "cabal" 
    ["repl", "--with-ghc=doctest", cardName] ""
  
  setCurrentDirectory currentDir
  
  when verboseMode $ do
    putStrLn stdout
    unless (null stderr) $ putStrLn stderr
  
  return $ case exitCode of
    ExitSuccess -> 
      let exampleCount = countExamples stderr  -- doctest outputs to stderr
      in if exampleCount > 0
         then TestPassed $ show exampleCount ++ " doctests passing"
         else TestPassed "no doctests found"
    ExitFailure _ ->
      TestFailed $ takeWhile (/= '\n') stderr
-- | Count examples from doctest output
-- Parses "Examples: 9  Tried: 9  Errors: 0  Failures: 0"
countExamples :: String -> Int
countExamples output =
  case filter ("Examples:" `isInfixOf`) (lines output) of
    (line:_) -> 
      let ws = words line
      in case dropWhile (/= "Examples:") ws of
           (_:numStr:_) -> read numStr
           _ -> 0
    [] -> 0
-- | Run a test executable
runTestExecutable :: FilePath -> String -> Bool -> IO TestResult
runTestExecutable cardPath testName verboseMode = do
  let cardName = takeBaseName cardPath
  let executable = "artifacts/bin" </> cardName ++ "-" ++ testName
  
  execExists <- doesFileExist executable
  if not execExists
    then return $ TestFailed "executable not found (run install first)"
    else do
      when verboseMode $ putStrLn $ "Running " ++ testName ++ "..."
      
      -- Run with timing (tickIO returns Nanos = Integer nanoseconds)
      (nanos, (exitCode, stdout, stderr)) <- tickIO $ readProcessWithExitCode executable [] ""
      let seconds = fromIntegral nanos / 1e9  -- Convert nanos to seconds
      
      when verboseMode $ do
        putStrLn stdout
        unless (null stderr) $ putStrLn stderr
      
      return $ case exitCode of
        ExitSuccess -> 
          let outputLines = filter (not . null) (lines stdout)
          in case outputLines of
               [] -> TestTimed seconds  -- No output, just show timing
               [singleLine] -> TestPassed singleLine  -- Single line output
               multiLines -> TestMultiLine multiLines  -- Multi-line output (e.g. perf report)
        ExitFailure _ ->
          TestFailed $ takeWhile (/= '\n') stderr
-- | Run and update a single test
runAndUpdateTest :: FilePath -> String -> Bool -> IO ()
runAndUpdateTest cardPath testName verboseMode = do
  result <- if testName == "doctests"
            then runDoctests cardPath verboseMode
            else runTestExecutable cardPath testName verboseMode
  
  let formatted = formatTestResult result
  updateStatusLine cardPath testName formatted
  
  putStrLn $ testName ++ ": " ++ T.unpack formatted
-- | Test a card
testCard :: FilePath -> Bool -> Maybe String -> IO ()
testCard cardPath verboseMode onlyTest = do
  case onlyTest of
    Just testName -> do
      -- Run specific test only
      runAndUpdateTest cardPath testName verboseMode
    
    Nothing -> do
      -- Run all tests
      taggedExecs <- parseTaggedExecutables cardPath
      let testTags = filter (\t -> t /= "main" && t /= "noop") taggedExecs
      let allTests = "doctests" : testTags
      
      when verboseMode $ putStrLn $ "Running " ++ show (length allTests) ++ " tests..."
      
      forM_ allTests $ \testName ->
        runAndUpdateTest cardPath testName verboseMode
      
      putStrLn "All tests complete"
-- | Run a benchmark with perf options
benchmarkCard :: FilePath -> String -> [String] -> Bool -> IO ()
benchmarkCard cardPath benchName args verboseMode = do
  let cardName = takeBaseName cardPath
  let executable = "artifacts/bin" </> cardName ++ "-" ++ benchName
  
  execExists <- doesFileExist executable
  if not execExists
    then do
      putStrLn $ "Benchmark not found: " ++ benchName
      putStrLn $ "Run: card-api install " ++ cardPath
      exitWith (ExitFailure 1)
    else do
      when verboseMode $ do
        putStrLn $ "Running benchmark: " ++ benchName
        putStrLn $ "Executable: " ++ executable
        putStrLn $ "Args: " ++ unwords args
      
      -- Run benchmark with args
      (exitCode, stdout, stderr) <- readProcessWithExitCode executable args ""
      
      -- Always show output (benchmarks are meant to be viewed)
      putStrLn stdout
      unless (null stderr) $ putStrLn stderr
      
      case exitCode of
        ExitSuccess -> do
          -- Update Status with multi-line output
          let outputLines = filter (not . null) (lines stdout)
          let result = case outputLines of
                [] -> T.pack "✓ completed"
                [single] -> T.pack $ "✓ " ++ single
                multi -> T.pack $ "✓\n" ++ unlines (map ("    " ++) multi)
          
          content <- TIO.readFile cardPath
          let updated = replaceOrAddStatusLine content benchName result
          TIO.writeFile cardPath updated
          
          when verboseMode $ putStrLn $ "Updated Status for: " ++ benchName
        
        ExitFailure code -> do
          putStrLn $ "Benchmark failed with exit code: " ++ show code
          exitWith (ExitFailure code)
-- | Display card documentation
showDocs :: FilePath -> IO ()
showDocs cardPath = do
  content <- readFile cardPath
  putStrLn content
-- | Update a single line in Status section
updateStatusLine :: FilePath -> String -> T.Text -> IO ()
updateStatusLine cardPath testName result = do
  content <- TIO.readFile cardPath
  let updated = replaceOrAddStatusLine content testName result
  TIO.writeFile cardPath updated
-- | Replace or add a status line in the Tests section
replaceOrAddStatusLine :: T.Text -> String -> T.Text -> T.Text
replaceOrAddStatusLine content testName result =
  let linePattern = "- " <> T.pack testName <> ":"
      -- If result contains newlines, split into multiple lines
      resultLines = T.lines result
      newLines = case resultLines of
        [single] -> [linePattern <> " " <> single]  -- Single line
        (first:rest) -> (linePattern <> " " <> first) : rest  -- Multi-line
        [] -> [linePattern <> " ✓"]  -- Shouldn't happen
      contentLines = T.lines content
      updated = replaceInTests linePattern newLines contentLines
  in T.unlines updated
  where
    replaceInTests :: T.Text -> [T.Text] -> [T.Text] -> [T.Text]
    replaceInTests pattern newLines [] = []
    replaceInTests pattern newLines (line:rest)
      -- Found Tests section header
      | "**Tests:**" `T.isPrefixOf` line =
          let (testsLines, afterTests) = span isTestLine rest
              updatedTests = replaceOrAppendLines pattern newLines testsLines
          in line : updatedTests ++ replaceInTests pattern newLines afterTests
      | otherwise = line : replaceInTests pattern newLines rest
    
    replaceOrAppendLines :: T.Text -> [T.Text] -> [T.Text] -> [T.Text]
    replaceOrAppendLines pattern newLines [] = newLines  -- Add if not found
    replaceOrAppendLines pattern newLines (line:rest)
      | pattern `T.isPrefixOf` T.stripStart line = 
          -- Found the test line - remove it and any indented continuation lines
          let restWithoutContinuation = dropWhile isContinuationLine rest
          in newLines ++ restWithoutContinuation
      | otherwise = line : replaceOrAppendLines pattern newLines rest
    
    -- Check if a line is a continuation (indented more than test lines)
    isContinuationLine :: T.Text -> Bool
    isContinuationLine line =
      let stripped = T.stripStart line
          spaces = T.length line - T.length stripped
      in spaces > 2 && not (T.null stripped)  -- Indented >2 spaces and non-empty
    
    isTestLine :: T.Text -> Bool
    isTestLine line =
      let trimmed = T.stripStart line
      in ("- " `T.isPrefixOf` trimmed && not ("**" `T.isPrefixOf` trimmed)) ||
         T.null trimmed
```
```haskell noop
-- Do-nothing executable for measuring syscall overhead
module Main where
main :: IO ()
main = return ()
```
```haskell bench-syscall
-- Benchmark system call overhead by calling noop executable
module Main where
import Perf (tickIO)
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..), exitFailure)
main :: IO ()
main = do
  (nanos, (exitCode, _, stderr)) <- tickIO $
    readProcessWithExitCode "card-api-noop" [] ""
  
  case exitCode of
    ExitSuccess -> do
      let ms = fromIntegral nanos / 1e6 :: Double
      putStrLn $ show ms ++ "ms"
    ExitFailure _ -> do
      putStrLn $ "✗ failed: " ++ stderr
      exitFailure
```
## run
Prerequisites:
```bash
# Ensure cabal and markdown-unlit are available
cabal install markdown-unlit
```
Bootstrap card-api:
```bash
mkdir -p ~/markdown-general/artifacts/haskell-build
cd ~/markdown-general/artifacts/haskell-build
# Create symlink
ln -s ../../zone/tools/card-api.md card-api.lhs
# Create minimal cabal project
cat > cabal.project << 'EOF'
packages: .
EOF
cat > haskell-build.cabal << 'EOF'
cabal-version: 2.4
name: haskell-build
version: 0
build-type: Simple
executable card-api
  main-is: card-api.lhs
  build-depends: base, text, optparse-applicative, directory, filepath, process, perf
  build-tool-depends: markdown-unlit:markdown-unlit
  ghc-options: -pgmL markdown-unlit -Wall
  default-language: Haskell2010
EOF
# Build and install
cabal install --installdir=../bin --overwrite-policy=always
```
Use card-api:
```bash
# Install a card (installs main + all tagged executables)
card-api install ~/markdown-general/zone/tools/flatten-md.md
# Run all tests
card-api test ~/markdown-general/zone/tools/flatten-md.md
# Run specific test only
card-api test ~/markdown-general/zone/tools/flatten-md.md --only roundtrip
# Run benchmark (no args)
card-api benchmark ~/markdown-general/zone/tools/flatten-md.md bench-perf
# Run benchmark with perf options (note: use -- to separate args)
card-api benchmark ~/markdown-general/zone/tools/flatten-md.md bench-perf -- -r
card-api benchmark ~/markdown-general/zone/tools/flatten-md.md bench-perf -- --stat mean
# Uninstall card
card-api uninstall ~/markdown-general/zone/tools/flatten-md.md
```
## card structure
Cards support multiple executables via fence block tags.
**All code blocks must be tagged** - the tag determines the executable name.
Example card structure (see flatten-md.md for complete example):
- `haskell main` blocks → main executable
- Other `haskell <tag>` blocks → test/benchmark executables
- Tag name becomes part of executable name: `cardname-tag`
**Tag mapping:**
- `main` tag → `cardname` executable → `cardname.hs` file
- Other tags → `cardname-tag` executable → `cardname-tag.hs` file
## status section
Updated with all test results:
```markdown
**Installation Cost:**
  - Tokens: unknown
  - Cache: 0 read, 0 created
  - Time: 7s
  - Model: claude-haiku-4-5-20251001
  - Installed: 2026-01-01 06:02 UTC
**Installation Cost:**
  - Tokens: unknown
  - Cache: 0 read, 0 created
  - Time: 7s
  - Model: claude-haiku-4-5-20251001
  - Installed: 2026-01-01 06:02 UTC
**Installation Cost:**
  - Tokens: unknown
  - Cache: 0 read, 0 created
  - Time: 7s
  - Model: claude-haiku-4-5-20251001
  - Installed: 2026-01-01 06:02 UTC

**Tests:** not yet run
```
## examples
See flatten-md.md for a complete working example with:
- Main executable (`main` tag)
- Test executable (`tick` tag)
- Benchmark executables (`bench-flatten`, `bench-unflatten` tags)
- Doctests in the main code
```bash
# Install all executables
card-api install my-tool.md
# Installs: my-tool, my-tool-basic, my-tool-edge-cases, my-tool-perf
# Run all tests
card-api test my-tool.md
# Updates Status with all results
# Run just one test
card-api test my-tool.md --only perf
# Updates just that line in Status
```
## status

**Tests:** not yet run

**Development Cost (Claude Code):**
  - Tokens: 26,685,432 (input: 5,212, output: 792)
  - Cache: 25,995,338 read, 684,090 created
  - Cost: $3.46
  - Time: ~2 hours
  - Model: claude-haiku-4-5-20251001
  - Session: 2026-01-01 06:05 UTC
  - Work: removed Aeson token tracking (misplaced), cleaned dependencies, refactored discover-executables

**Last updated:** 2026-01-01
