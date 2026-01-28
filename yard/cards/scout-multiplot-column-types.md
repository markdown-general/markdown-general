scout-multiplot-column-types ⟜ verify which s5e11 columns extract successfully for multiPlot

**input** ⟜ ~/repos/dataframe-dev with s5e11 cached

**output** ⟜ raw cabal repl output piped to log/scout-multiplot-column-types.md

**instruction**

cd ~/repos/dataframe-dev && cabal repl

```haskell
:set -XOverloadedStrings
import qualified DataFrame as D
import qualified DataFrame.Functions as F
import Data.Text (pack)

df <- D.readCsv "other/s5e11/train.csv"

putStrLn "=== Testing numeric columns ==="
let cs = D.columnAsList @Int (F.col (pack "credit_score")) df
putStrLn $ "credit_score (Int): " ++ show (length cs) ++ " values"

let lp = D.columnAsList @Double (F.col (pack "loan_paid_back")) df
putStrLn $ "loan_paid_back (Double): " ++ show (length lp) ++ " values, first 5: " ++ show (take 5 lp)

putStrLn "\n=== Testing categorical columns ==="
let genders = D.columnAsList @String (F.col (pack "gender")) df
putStrLn $ "gender (String): " ++ show (length genders) ++ " values, first 5: " ++ show (take 5 genders)

let employ = D.columnAsList @String (F.col (pack "employment_status")) df
putStrLn $ "employment_status (String): " ++ show (length employ) ++ " values"
```

Test which columns work with @Int, @Double, @String types for multiPlot.
