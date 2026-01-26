## haskell

cabal.project addition write-ghc-environment-files: always

When you or I are changing Haskell files:

## ghc environment files

Warning: Installing libraries with `cabal install --lib` creates a global GHC environment file that can confuse other tools.

When you see:
```
Warning: The libraries were installed by creating a global GHC environment
file at:
/Users/tonyday567/.ghc/aarch64-darwin-9.10.1/environments/default
```

The presence of such an environment file changes GHC's behaviour: it changes the default package set in ghc and ghci from its normal value (all boot libraries). GHC environment files are little-used and often not tested for.

Management of these files is still difficult; see https://github.com/haskell/cabal/issues/6481

To limit the effects, use the `--package-env` flag to create the file in a specific directory:

```bash
cabal install --lib <packages...> --package-env .
```

This creates the file in the current directory instead of globally.

