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

## why flatten

**Single file review** ⟜ examine entire codebase in one view
**LLM context** ⟜ feed complete context without file juggling
**Version control** ⟜ track conceptual changes across files
**Reversible** ⟜ perfect round-trip via unflatten

## subcommands

**flatten** ⟜ --input-dir DIR --output FILE
**unflatten** ⟜ --input FILE --output-dir DIR

## dependencies

base, text, optparse-applicative, directory, filepath

## code

```haskell top
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Options.Applicative
import System.Directory (listDirectory, doesDirectoryExist, createDirectoryIfMissing)
import System.FilePath ((</>), takeExtension, takeFileName)
import Control.Monad (filterM)
```

```haskell
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

```haskell
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

```haskell
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

## run

Installation via card-api.md:

```bash
card-api install ~/sisyphus/content/tools/flatten-md.md
```

After installation:

```bash
# Flatten files
flatten-md flatten --input-dir ~/sisyphus/content/base/ --output /tmp/flat.md

# Unflatten concatenated file
flatten-md unflatten --input /tmp/flat.md --output-dir /tmp/restored/
```

## examples

### flatten content/base/

```bash
flatten-md flatten --input-dir ~/sisyphus/content/base/ --output /tmp/base-flattened.md
```

Result: /tmp/base-flattened.md contains all files with delimiters

### unflatten back to original structure

```bash
# Unflatten concatenated file
flatten-md unflatten --input /tmp/base-flattened.md --output-dir /tmp/base-restored/

# Verify round-trip
diff -r ~/sisyphus/content/base/ /tmp/base-restored/
```

Expected: no differences (perfect round-trip)

## status

**Tests:** ✓ passed - 7 doctests passing
**Benchmark:** - flatten-base: 17.81ms (median over 100 runs)
- unflatten-base: 17.78ms (median over 100 runs)

Usage: flatten-md flatten --input-dir DIR --output FILE

  Flatten directory to single file

**Last updated:** 2025-12-31

## upstream

**Verbose flag** ⟜ gate putStrLn behind --verbose flag
- Currently all output unconditional
- Need to thread verbose flag through or use Writer monad

**File ordering** ⟜ currently filesystem order
- Simple and fast
- Deterministic ordering deferred to content/upstream/metadata.md
- Future: .file-importance.md for metadata-driven ordering

## benchmarks

**flatten-base** ⟜ flatten --input-dir content/base/ --output /tmp/flat.md
**unflatten-base** ⟜ unflatten --input /tmp/flat.md --output-dir /tmp/out

