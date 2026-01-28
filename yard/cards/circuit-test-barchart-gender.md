circuit-test-barchart-gender ⟜ test barChart on gender column from s5e11

**input** ⟜ ~/repos/dataframe-dev with s5e11 cached

**output** ⟜ SVG chart saved to /tmp/gender-chart.svg, findings to ~/markdown-general/log/circuit-test-barchart-gender.md

**instruction**

cd ~/repos/dataframe-dev && cabal repl << 'EOF'
:set -XOverloadedStrings
import qualified DataFrame as D
import qualified DataFrame.Functions as F
import Data.Text (Text, pack)
import DataFrame.Dev (count, countedBar)
import Chart
import Prettychart
import qualified Data.ByteString as BS

-- Load dataframe
df <- D.readCsv "other/s5e11/test.csv"

-- Extract gender column as [Text] (not String)
let genderVals = D.columnAsList @Text (F.col (pack "gender")) df
putStrLn $ "Extracted " ++ show (length genderVals) ++ " gender values"

-- Count occurrences
let genderCounts = count genderVals
putStrLn $ "Gender counts (top 5): " ++ show (take 5 genderCounts)

-- Create chart
let chart = countedBar "Gender Distribution" genderCounts
putStrLn "Chart created"

-- Encode to SVG
let svg = encodeChartOptions chart
putStrLn $ "SVG generated: " ++ show (BS.length svg) ++ " bytes"

-- Save
BS.writeFile "/tmp/gender-chart.svg" svg
putStrLn "✓ Saved to /tmp/gender-chart.svg"
EOF

**comment** ⟜ Circuit test: does barChart work end-to-end on a categorical column?
