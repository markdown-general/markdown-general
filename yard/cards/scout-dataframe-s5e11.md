scout-dataframe-s5e11 ⟜ explore DataFrame API on s5e11 data, understand column types and extraction

**input** ⟜ ~/repos/dataframe-dev with s5e11 cached

**output** ⟜ findings written to log/scout-dataframe-s5e11.md

**instruction**

cd ~/repos/dataframe-dev && cabal repl

```haskell
:set -XOverloadedStrings
import qualified DataFrame as D
import Data.Text (pack)

-- Load s5e11
df <- D.readCsv "other/s5e11/train.csv"

-- Inspect columns
let summary = D.describeColumns df
D.writeCsv "/tmp/summary.csv" summary
print summary

-- Test numeric extraction
let nums = D.columnAsList @Double (pack "annual_income") df
putStrLn $ "annual_income (first 5): " ++ show (take 5 nums)

-- Test categorical extraction
let cats = D.columnAsList @String (pack "gender") df
putStrLn $ "gender (first 5): " ++ show (take 5 cats)
putStrLn $ "unique genders: " ++ show (length (nub cats))

-- Test column names
let names = D.columnNames df
putStrLn $ "All columns: " ++ show names
```

Document findings:
- describeColumns output (types: Double, Int, Text)
- Numeric columns (can extract with @Double/@Int)
- Categorical columns (can extract with @String or @Text)
- Pattern for detecting type from summary
- Ready for multiPlot categorical detection

**comment** ⟜ This is exploratory. Run in repl, capture output, write to log/scout-dataframe-s5e11.md with your findings. Document:
1. How describeColumns reports types
2. Which extraction works for numeric vs categorical
3. Pattern for Map/count of categorical values
4. Ready to implement multiPlot categorical charting
