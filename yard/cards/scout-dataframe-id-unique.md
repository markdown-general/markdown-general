scout-dataframe-id-unique ⟜ verify if id column contains unique identifiers in s5e11

**input** ⟜ ~/repos/dataframe-dev with s5e11 cached

**output** ⟜ raw cabal repl output piped to log/scout-dataframe-id-unique.md

**instruction**

cd ~/repos/dataframe-dev && cabal repl

```haskell
:set -XOverloadedStrings
import qualified DataFrame as D
import qualified DataFrame.Functions as F
import Data.Text (pack)
import Data.List (nub, sort)

-- Load s5e11
df <- D.readCsv "other/s5e11/train.csv"

-- Get dimensions
let (nrows, ncols) = D.dimensions df
putStrLn $ "DataFrame dimensions: " ++ show nrows ++ " rows, " ++ show ncols ++ " cols"

-- Extract id column
let ids = D.columnAsList @Int (F.col (pack "id")) df
putStrLn $ "Total id values: " ++ show (length ids)

-- Check uniqueness
let unique_ids = nub ids
putStrLn $ "Unique id values: " ++ show (length unique_ids)

-- Check if all unique
if length ids == length unique_ids
  then putStrLn "✓ All id values are unique (primary key confirmed)"
  else do
    putStrLn "✗ Duplicate ids found"
    let duplicates = filter (\x -> length (filter (==x) ids) > 1) unique_ids
    putStrLn $ "Duplicate count: " ++ show (length duplicates)

-- Check range
putStrLn $ "ID range: min=" ++ show (minimum ids) ++ ", max=" ++ show (maximum ids)
```

Pipe entire output to log/ file as-is, no filtering.
