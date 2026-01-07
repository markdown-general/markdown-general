#line 20 "/Users/tonyday567/sisyphus/content/tools/discover-executables.md"
{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Directory (listDirectory, doesFileExist)
import System.FilePath (takeExtension, dropExtension, (</>))
import Data.List (sort)
import Control.Monad (filterM)
#line 31 "/Users/tonyday567/sisyphus/content/tools/discover-executables.md"
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
      baseDeps = "base, text, optparse-applicative, directory, filepath, process, perf, clock, containers"
      deps = if execName == "card-api"
             then baseDeps ++ ", aeson, bytestring, vector"
             else baseDeps
  in [ "executable " ++ execName
     , "  main-is: " ++ file
     , "  build-depends: " ++ deps
     , "  ghc-options: -Wall"
     , "  default-language: Haskell2010"
     , ""
     ]
