# flatten-md.md

- flatten markdown files from a directory into a single markdown file.
- unflatten a single markdown file to create a directory of markdown files.

## what it does

flatten - takes a list of markdown files, concatenates them with HTML comment delimiters, outputs to specified location.

unflatten - reverses the process: reads the concatenated file, extracts individual files back to original paths.

## why html comments

**Invisible delimiters** ⟜ render cleanly in markdown viewers
**Unambiguous parsing** ⟜ `<!-- FILE: path -->` markers are unique
**No escaping needed** ⟜ markdown content can contain anything
**Preserves structure** ⟜ full relative paths in delimiters
**Simple boundaries** ⟜ next FILE marker or EOF ends content

## subcommands

**flatten** ⟜ concatenate files with delimiters
**unflatten** ⟜ extract files from concatenated output

## code
```haskell
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Options.Applicative
import System.Directory
import System.FilePath
import Control.Monad (forM_, unless)
import Data.List (isPrefixOf, isSuffixOf)

-- | Command line options
data Command
  = Flatten
      { fileList :: FilePath
      , output :: FilePath
      , baseDir :: FilePath
      }
  | Unflatten
      { concatFile :: FilePath
      , outputDir :: FilePath
      }

-- | Parse command line arguments
parseCommand :: Parser Command
parseCommand = subparser
  ( command "flatten" (info flattenParser (progDesc "Concatenate files with delimiters"))
 <> command "unflatten" (info unflattenParser (progDesc "Extract files from concatenated output"))
  )

flattenParser :: Parser Command
flattenParser = Flatten
  <$> strArgument (metavar "FILE_LIST" <> help "File containing list of markdown files")
  <*> strArgument (metavar "OUTPUT" <> help "Output path for concatenated file")
  <*> strOption (long "base-dir" <> value "." <> metavar "DIR" 
                <> help "Base directory for relative paths")

unflattenParser :: Parser Command
unflattenParser = Unflatten
  <$> strArgument (metavar "CONCAT_FILE" <> help "Concatenated file to extract")
  <*> strOption (long "output-dir" <> value "." <> metavar "DIR"
                <> help "Output directory for extracted files")

-- | Main entry point
main :: IO ()
main = do
  cmd <- execParser opts
  case cmd of
    Flatten flist out base -> flattenFiles flist out base
    Unflatten concat outDir -> unflattenFile concat outDir
  where
    opts = info (parseCommand <**> helper)
      ( fullDesc
     <> progDesc "Flatten and unflatten markdown files with reversibility"
     <> header "flatten-md - markdown file concatenation tool" )

-- | Flatten files into single concatenated file
flattenFiles :: FilePath -> FilePath -> FilePath -> IO ()
flattenFiles fileListPath outputPath baseDirPath = do
  -- Read file list
  fileListContent <- TIO.readFile fileListPath
  let files = filter (not . T.null) 
            . filter (not . T.isPrefixOf "#")
            . map T.strip 
            . T.lines 
            $ fileListContent
  
  putStrLn $ "Flattening " ++ show (length files) ++ " files..."
  
  -- Create output directory if needed
  createDirectoryIfMissing True (takeDirectory outputPath)
  
  -- Process each file
  contents <- mapM (processFile baseDirPath) files
  
  -- Write concatenated output
  TIO.writeFile outputPath (T.concat contents)
  putStrLn $ "Created: " ++ outputPath

-- | Process a single file for flattening
processFile :: FilePath -> T.Text -> IO T.Text
processFile baseDir relPath = do
  let fullPath = baseDir </> T.unpack relPath
  exists <- doesFileExist fullPath
  
  if not exists
    then do
      putStrLn $ "Warning: " ++ fullPath ++ " not found, skipping"
      return ""
    else do
      content <- TIO.readFile fullPath
      let delimiter = "<!-- FILE: " <> relPath <> " -->\n"
      let cleaned = T.unlines . filter (not . isFileDelimiter) . T.lines $ content
      return $ delimiter <> cleaned

-- | Check if line is a FILE delimiter
isFileDelimiter :: T.Text -> Bool
isFileDelimiter line = 
  T.isPrefixOf "<!-- FILE: " line && T.isSuffixOf " -->" (T.strip line)

-- | Unflatten concatenated file back to individual files
unflattenFile :: FilePath -> FilePath -> IO ()
unflattenFile concatPath outputDirPath = do
  content <- TIO.readFile concatPath
  
  putStrLn $ "Unflattening " ++ concatPath ++ "..."
  
  let linesWithDelims = T.lines content
  let files = extractFiles linesWithDelims
  
  -- Write each file
  forM_ files $ \(path, fileContent) -> do
    let fullPath = outputDirPath </> path
    createDirectoryIfMissing True (takeDirectory fullPath)
    TIO.writeFile fullPath fileContent
    putStrLn $ "  " ++ path
  
  putStrLn $ "Extracted " ++ show (length files) ++ " files to: " ++ outputDirPath ++ "/"

-- | Extract files from lines with delimiters
extractFiles :: [T.Text] -> [(FilePath, T.Text)]
extractFiles = go Nothing []
  where
    go Nothing [] [] = []
    go (Just (path, acc)) [] [] = [(path, T.unlines (reverse acc))]
    go current acc [] = maybe [] (\(p, a) -> [(p, T.unlines (reverse a))]) current
    go current acc (line:rest)
      | isFileDelimiter line =
          let newPath = extractPath line
              result = maybe [] (\(p, a) -> [(p, T.unlines (reverse a))]) current
          in result ++ go (Just (newPath, [])) [] rest
      | otherwise =
          case current of
            Nothing -> go Nothing [] rest
            Just (p, a) -> go (Just (p, line:a)) [] rest

-- | Extract path from delimiter line
extractPath :: T.Text -> FilePath
extractPath line =
  let stripped = T.strip line
      withoutPrefix = T.drop 11 stripped  -- Remove "<!-- FILE: "
      withoutSuffix = T.take (T.length withoutPrefix - 4) withoutPrefix  -- Remove " -->"
  in T.unpack (T.strip withoutSuffix)
```

## run

Installation via haskell-api.md:
```bash
haskell-api flatten-md.md --install
```

After installation:
```bash
# Flatten files
flatten-md flatten files.txt output.md --base-dir content/

# Unflatten concatenated file
flatten-md unflatten merged.md --output-dir restored/
```

## api

### flatten command

**Inputs:**
- `file_list` - path to file containing ordered list of markdown files (one per line)
- `output` - path for concatenated output file
- `--base-dir` - base directory for relative paths (default: current directory)

**Outputs:**
- Single markdown file with HTML comment delimiters
- Each file's content preserved with `<!-- FILE: relative/path.md -->` marker

**File list format:**
```
work/cache.md
work/coding.md
# Comments starting with # are ignored
work/deck.md
```

### unflatten command

**Inputs:**
- `concat_file` - path to concatenated markdown file
- `--output-dir` - directory for extracted files (default: current directory)

**Outputs:**
- Individual markdown files restored to original paths
- Directory structure created as needed

## examples

### flatten work/
```bash
flatten-md flatten base-files.txt /tmp/base-flattened.md --base-dir .
```

Result: /tmp/base-flattened.md contains all files with delimiters

### unflatten back to original structure
```bash
# Unflatten concatenated file
flatten-md unflatten /tmp/base-flattened.md --output-dir restored/

# Verify round-trip
diff -r work/ restored/work/
```

## tests

TODO: Add comprehensive test suite

### round-trip identity

Expected: Files should be identical after flatten → unflatten cycle

### delimiter edge cases

Expected: Tool should handle existing delimiters, empty files, files without trailing newlines

## status

**Tests:** TODO - not yet implemented
**Benchmark:** TODO - not yet implemented  
**Last updated:** 2025-12-31

## upstream problems

TODO: Define after initial implementation works
