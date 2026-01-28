circuit-fake-data-chart ⟜ generate fake 4-element data (10 each) and chart to filewatch

**input** ⟜ ~/repos/dataframe-dev

**output** ⟜ fake-chart.svg saved to /tmp/watch/

**instruction**

cd ~/repos/dataframe-dev && cabal repl << 'EOF'
:set -XOverloadedStrings
import Data.Text (Text)
import DataFrame.Dev (count, countedBar)
import Chart
import Prettychart
import qualified Data.ByteString as BS

-- Fake data: 4 elements × 10 each
let fakeVals = concat (replicate 10 ["apple", "banana", "cherry", "date" :: Text])
putStrLn $ "Created " ++ show (length fakeVals) ++ " values"

-- Count and chart
let counts = count fakeVals
putStrLn $ "Counts: " ++ show counts

let chart = countedBar "Fake Distribution" counts
putStrLn "Chart created"

-- Encode and save
let svg = encodeChartOptions chart
BS.writeFile "/tmp/watch/fake-chart.svg" svg
putStrLn $ "✓ Saved to /tmp/watch/fake-chart.svg (" ++ show (BS.length svg) ++ " bytes)"
EOF

**comment** ⟜ Fake data test: apple, banana, cherry, date each appear 10 times
