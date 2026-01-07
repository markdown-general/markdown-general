## haskell

cabal.project addition write-ghc-environment-files: always

When you or I are changing Haskell files:

- make sure `ghcid --command="cabal repl" --outputfile=ghcid.txt` is running
- check the tail of ghcid.txt and ask to fix any errors, or mash for auto-fix.

