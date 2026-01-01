# flatten-md.md

Concatenates markdown files with HTML comment delimiters for reversible flattening.

## what it does

**Flatten** ⟜ directory of .md files → single concatenated file with delimiters
**Unflatten** ⟜ concatenated file → original directory structure

Delimiters use HTML comments so they're invisible when rendered:
```html
<!-- FILE: content/base/card.md -->
...content...
<!-- END FILE -->
```

## code

```haskell main
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Options.Applicative
import System.Directory (listDirectory, doesDirectoryExist, createDirectoryIfMissing)
import System.FilePath ((</>), takeExtension, takeFileName)
import Control.Monad (filterM)
```

```haskell main
-- | Command line options
data Command
  = Flatten
      { inputDir :: FilePath
      , outputFile :: FilePath
      }
  | Unflatten
      { inputFile :: FilePath
      , outputDir :: FilePath
      }

-- | Parse command line arguments
parseCommand :: Parser Command
parseCommand = subparser
  ( command "flatten" (info flattenParser (progDesc "Flatten directory to single file"))
 <> command "unflatten" (info unflattenParser (progDesc "Unflatten file to directory"))
  )

flattenParser :: Parser Command
flattenParser = Flatten
  <$> strOption (long "input-dir" <> metavar "DIR" <> help "Input directory")
  <*> strOption (long "output" <> metavar "FILE" <> help "Output file")

unflattenParser :: Parser Command
unflattenParser = Unflatten
  <$> strOption (long "input" <> metavar "FILE" <> help "Input file")
  <*> strOption (long "output-dir" <> metavar "DIR" <> help "Output directory")

-- | Main entry point
main :: IO ()
main = do
  cmd <- execParser opts
  case cmd of
    Flatten inDir outFile -> flattenDir inDir outFile
    Unflatten inFile outDir -> unflattenFile inFile outDir
  where
    opts = info (parseCommand <**> helper)
      ( fullDesc
     <> progDesc "Flatten/unflatten markdown files"
     <> header "flatten-md - reversible markdown concatenation" )
```

```haskell main
-- | Flatten a directory to a single file
flattenDir :: FilePath -> FilePath -> IO ()
flattenDir dir outFile = do
  files <- findMarkdownFiles dir
  putStrLn $ "Flattening " ++ show (length files) ++ " files..."
  
  contents <- mapM readWithDelimiter files
  TIO.writeFile outFile (T.concat contents)
  
  lineCount <- countLines outFile
  putStrLn $ "Created: " ++ outFile ++ " (" ++ show lineCount ++ " lines)"

-- | Find all .md files recursively
findMarkdownFiles :: FilePath -> IO [FilePath]
findMarkdownFiles dir = do
  entries <- listDirectory dir
  let paths = map (dir </>) entries
  
  files <- filterM (\p -> return (takeExtension p == ".md")) paths
  dirs <- filterM doesDirectoryExist paths
  
  subFiles <- concat <$> mapM findMarkdownFiles dirs
  return (files ++ subFiles)

-- | Read file and wrap with delimiters
readWithDelimiter :: FilePath -> IO T.Text
readWithDelimiter path = do
  content <- TIO.readFile path
  return $ addDelimiter path content

-- | Add HTML comment delimiters
--
-- >>> addDelimiter "test.md" "content\n"
-- "<!-- FILE: test.md -->\ncontent\n<!-- END FILE -->\n"
addDelimiter :: FilePath -> T.Text -> T.Text
addDelimiter path content =
  "<!-- FILE: " <> T.pack path <> " -->\n" <> content <> "<!-- END FILE -->\n"

-- | Count lines in a file
countLines :: FilePath -> IO Int
countLines path = do
  content <- TIO.readFile path
  return $ length (T.lines content)
```

```haskell main
-- | Unflatten a concatenated file back to directory
unflattenFile :: FilePath -> FilePath -> IO ()
unflattenFile inFile outDir = do
  content <- TIO.readFile inFile
  let files = extractFiles content
  
  putStrLn $ "Extracting " ++ show (length files) ++ " files..."
  mapM_ (writeFile' outDir) files
  putStrLn $ "Extracted to: " ++ outDir

-- | Write a file to output directory
writeFile' :: FilePath -> (FilePath, T.Text) -> IO ()
writeFile' baseDir (path, content) = do
  let fullPath = baseDir </> path
  let dir = take (length fullPath - length (takeFileName fullPath)) fullPath
  createDirectoryIfMissing True dir
  TIO.writeFile fullPath (stripDelimiters content)

-- | Extract (filepath, content) pairs from concatenated file
--
-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import qualified Data.Text as T
--
-- >>> let input = T.unlines ["<!-- FILE: a.md -->", "content a", "<!-- END FILE -->", "<!-- FILE: b.md -->", "content b", "<!-- END FILE -->"]
-- >>> extractFiles input
-- [("a.md","content a\n"),("b.md","content b\n")]
extractFiles :: T.Text -> [(FilePath, T.Text)]
extractFiles content =
  let sections = splitSections (T.lines content)
  in map extractFileContent sections

-- | Split content into file sections
splitSections :: [T.Text] -> [[T.Text]]
splitSections = go []
  where
    go acc [] = [reverse acc | not (null acc)]
    go acc (line:rest)
      | isFileDelimiter line = 
          let section = reverse acc
              (fileSection, remaining) = span (not . isEndDelimiter) (line:rest)
          in (if null section then [] else [section]) ++ 
             [fileSection ++ take 1 remaining] ++ 
             go [] (drop 1 remaining)
      | otherwise = go (line:acc) rest

-- | Extract filepath and content from a section
extractFileContent :: [T.Text] -> (FilePath, T.Text)
extractFileContent section =
  case section of
    (firstLine:rest) ->
      let path = extractPath firstLine
          content = T.unlines (init rest)  -- Remove END FILE line
      in (path, content)
    [] -> ("", "")

-- | Check if line is a file delimiter
--
-- >>> isFileDelimiter "<!-- FILE: test.md -->"
-- True
-- >>> isFileDelimiter "regular content"
-- False
isFileDelimiter :: T.Text -> Bool
isFileDelimiter line = "<!-- FILE:" `T.isPrefixOf` T.strip line

-- | Check if line is end delimiter
isEndDelimiter :: T.Text -> Bool
isEndDelimiter line = "<!-- END FILE -->" `T.isPrefixOf` T.strip line

-- | Extract filepath from delimiter line
--
-- >>> extractPath "<!-- FILE: content/base/card.md -->"
-- "content/base/card.md"
extractPath :: T.Text -> FilePath
extractPath line =
  let stripped = T.strip line
      afterPrefix = T.drop (T.length "<!-- FILE: ") stripped
      beforeSuffix = T.take (T.length afterPrefix - T.length " -->") afterPrefix
  in T.unpack beforeSuffix

-- | Strip delimiters from content
--
-- >>> stripDelimiters "<!-- FILE: test.md -->\ncontent\n<!-- END FILE -->\n"
-- "content\n"
stripDelimiters :: T.Text -> T.Text
stripDelimiters content =
  let contentLines = T.lines content
      withoutFirst = case contentLines of
        (first:rest) | isFileDelimiter first -> rest
        _ -> contentLines
      withoutLast = case reverse withoutFirst of
        (last':rest) | isEndDelimiter last' -> reverse rest
        _ -> withoutFirst
  in T.unlines withoutLast
```

## tests

```haskell tick
-- Simple test that always succeeds
module Main where
import System.Exit

main :: IO ()
main = do
  putStrLn "tick: passed"
  exitSuccess
```

```haskell bench-flatten
-- Benchmark flattening content/base/
module Main where
import Perf (tickIO)
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..), exitFailure)
import System.Directory (removeFile, doesFileExist)
import Control.Monad (when)

main :: IO ()
main = do
  -- Clean up any existing output
  let outFile = "/tmp/flatten-bench.md"
  exists <- doesFileExist outFile
  when exists $ removeFile outFile
  
  -- Time the flatten operation
  (nanos, (exitCode, _, stderr)) <- tickIO $
    readProcessWithExitCode "flatten-md" 
      ["flatten", "--input-dir", "content/base", "--output", outFile] ""
  
  case exitCode of
    ExitSuccess -> do
      let ms = fromIntegral nanos / 1e6 :: Double
      putStrLn $ show ms ++ "ms"
    ExitFailure _ -> do
      putStrLn $ "✗ failed: " ++ stderr
      exitFailure
```

```haskell bench-unflatten
-- Benchmark unflattening
module Main where
import Perf (tickIO)
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..), exitFailure)
import System.Directory (removeDirectoryRecursive, doesDirectoryExist)
import Control.Monad (when)

main :: IO ()
main = do
  -- Clean up any existing output
  let outDir = "/tmp/unflatten-bench"
  exists <- doesDirectoryExist outDir
  when exists $ removeDirectoryRecursive outDir
  
  -- Time the unflatten operation
  (nanos, (exitCode, _, stderr)) <- tickIO $
    readProcessWithExitCode "flatten-md"
      ["unflatten", "--input", "/tmp/flatten-bench.md", "--output-dir", outDir] ""
  
  case exitCode of
    ExitSuccess -> do
      let ms = fromIntegral nanos / 1e6 :: Double
      putStrLn $ show ms ++ "ms"
    ExitFailure _ -> do
      putStrLn $ "✗ failed: " ++ stderr
      exitFailure
```

```haskell bench-perf
-- Performance breakdown of flatten operation
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude
import Data.String
import Perf
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (listDirectory, doesDirectoryExist, getHomeDirectory, createDirectoryIfMissing)
import System.FilePath (FilePath, (</>), takeExtension)
import Control.Monad (filterM)
import System.Clock (Clock(Monotonic))
import qualified Data.Map.Strict as Map
import Options.Applicative

-- | Find all .md files recursively
findMarkdownFiles :: FilePath -> IO [FilePath]
findMarkdownFiles dir = do
  entries <- listDirectory dir
  let paths = map (dir </>) entries
  
  files <- filterM (\p -> return (takeExtension p == ".md")) paths
  dirs <- filterM doesDirectoryExist paths
  
  subFiles <- concat <$> mapM findMarkdownFiles dirs
  return (files ++ subFiles)

-- | Read file and wrap with delimiters
readWithDelimiter :: FilePath -> IO T.Text
readWithDelimiter path = do
  content <- TIO.readFile path
  return $ addDelimiter path content

-- | Add HTML comment delimiters
addDelimiter :: FilePath -> T.Text -> T.Text
addDelimiter path content =
  "<!-- FILE: " <> T.pack path <> " -->\n" <> content <> "<!-- END FILE -->\n"

-- | Performance-instrumented version of flattenDir
flattenDirPerf :: (Semigroup t) => FilePath -> FilePath -> PerfT IO t ()
flattenDirPerf dir outFile = do
  -- Measure file finding
  files <- fam "find-files" (findMarkdownFiles dir)
  
  -- Measure reading all files
  contents <- fam "read-files" (mapM readWithDelimiter files)
  
  -- Measure pure concatenation
  combined <- ffap "concat" T.concat contents
  
  -- Measure writing
  fam "write-file" (TIO.writeFile outFile combined)

main :: IO ()
main = do
  -- Parse report options from command line
  reportOpts <- execParser (info (parseReportOptions defaultReportOptions <**> helper)
    (fullDesc <> progDesc "Performance breakdown of flatten operation"))
  
  -- Set golden file path while preserving check/record flags from command line
  let goldenPath = "artifacts/haskell-golden/flatten-md-bench-perf.golden"
  createDirectoryIfMissing True "artifacts/haskell-golden"
  let opts = reportOpts { reportGolden = (reportGolden reportOpts) { golden = goldenPath } }
  
  -- Run benchmark
  home <- getHomeDirectory
  m <- execPerfT (measureDs MeasureTime Monotonic 1) 
         (flattenDirPerf (home ++ "/sisyphus/content/base") "/tmp/perf-flat.md")
  
  report opts (statify StatMedian m)
```

## status

**Tests:**
- doctests: not yet run
- tick: not yet run
- bench-flatten: not yet run
- bench-unflatten: not yet run
- bench-perf: ✓
    label1          label2          old result      new result      change          
    concat          time            3.00e3          3.00e3                          
    find-files      time            1.92e5          1.50e4          improvement     
    read-files      time            1.00e3          1.00e3                          
    write-file      time            0.00e0          0.00e0                          
    label1          label2          old result      new result      change          
    concat          time            3.00e3          3.00e3                          
    find-files      time            1.92e5          1.92e5                          
    read-files      time            1.00e3          1.00e3                          
    write-file      time            0.00e0          0.00e0                          

**Last updated:** 2025-01-01
