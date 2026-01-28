circuit-load-s5e11-describe ⟜ load s5e11 CSV and run describeColumns to inspect all columns

**input** ⟜ ~/repos/dataframe-dev with s5e11 cached at other/s5e11/test.csv

**output** ⟜ findings written to ~/markdown-general/log/circuit-load-s5e11-describe.md

**instruction**

cd ~/repos/dataframe-dev && cabal repl << 'EOF'
:set -XOverloadedStrings
import qualified DataFrame as D

-- Load test.csv (593,994 rows × 12 columns)
df <- D.readCsv "other/s5e11/test.csv"

-- Describe all columns
let summary = D.describeColumns df
print summary

-- Show column names
let names = D.columnNames df
putStrLn $ "\nColumn names: " ++ show names

-- Quick verification
putStrLn $ "\nTotal rows: " ++ show (D.nrows df)
putStrLn $ "Total cols: " ++ show (D.ncols df)
EOF

**comment** ⟜ Circuit execution: runs in background, writes output to log/. No waiting. Operator reads when ready.
- Load the test.csv file from other/s5e11/
- Call describeColumns to see all column types (Double, Int, Text, etc)
- Show column names
- Show dimensions (rows × cols)
- This reveals what types are available for boxplotForColumn and stackedbarForColumn
