# discover-executables.md

Generates haskell-build.cabal by discovering all .hs files in the build directory.

## what it does

Scans `artifacts/haskell-build/` and creates executable stanzas for all `.hs` files.

Makes the cabal file a derived artifact - regenerated on every install/uninstall.

## why

**No manual cabal management** ⟜ cabal file auto-generated from directory contents
**No sync issues** ⟜ what's in the directory is what's in the cabal file
**No overwriting** ⟜ each install/uninstall regenerates from scratch

## code

```haskell main
{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Directory (listDirectory, doesFileExist)
import System.FilePath (takeExtension, dropExtension, (</>))
import Data.List (sort)
import Control.Monad (filterM)
```

```haskell main
-- | Main entry point
main :: IO ()
main = do
  -- Work in current directory
  let buildDir = "."
  
  -- Find all .lhs and .hs files
  allFiles <- listDirectory buildDir
  let sourceFiles = filter isSourceFile allFiles
  
  -- Generate cabal content
  let cabalContent = generateCabal (sort sourceFiles)
  
  -- Write cabal file
  writeFile "haskell-build.cabal" cabalContent
  
  putStrLn $ "Generated haskell-build.cabal with " ++ 
             show (length sourceFiles) ++ " executables"

-- | Check if file is a Haskell source file
isSourceFile :: FilePath -> Bool
isSourceFile path = takeExtension path == ".hs"

-- | Generate complete cabal file content
generateCabal :: [FilePath] -> String
generateCabal files = unlines $
  [ "cabal-version: 2.4"
  , "name: haskell-build"
  , "version: 0"
  , "build-type: Simple"
  , ""
  ] ++ concatMap generateExecutable files

-- | Generate executable stanza for a file
generateExecutable :: FilePath -> [String]
generateExecutable file =
  let execName = dropExtension file
      deps = "base, text, optparse-applicative, directory, filepath, process, perf, clock, containers"
  in [ "executable " ++ execName
     , "  main-is: " ++ file
     , "  build-depends: " ++ deps
     , "  ghc-options: -Wall"
     , "  default-language: Haskell2010"
     , ""
     ]
```

## run

```bash
# Run from the haskell-build directory
cd artifacts/haskell-build
discover-executables

# Result: haskell-build.cabal created/updated in current directory
```

## usage in card-api

After adding/removing symlinks and extracted files, card-api calls:

```bash
discover-executables
```

This regenerates the cabal file to match what's currently in the directory.

## status

**Tests:** not yet run
**Last updated:** 2025-01-01
