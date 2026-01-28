scout-dataframe-credit-score-dist ⟜ examine credit_score distribution to confirm if truly numeric or categorical

**input** ⟜ ~/repos/dataframe-dev with s5e11 cached

**output** ⟜ raw cabal repl output piped to log/scout-dataframe-credit-score-dist.md

**instruction**

cd ~/repos/dataframe-dev && cabal repl

```haskell
:set -XOverloadedStrings
import qualified DataFrame as D
import qualified DataFrame.Functions as F
import Data.Text (pack)
import Data.Map.Strict qualified as Map
import Data.List (sortOn, foldl')
import Data.Ord (Down(..))
import Control.Monad (forM_)

df <- D.readCsv "other/s5e11/train.csv"

-- Extract first 100 credit scores
let scores = take 100 (D.columnAsList @Int (F.col (pack "credit_score")) df)
putStrLn $ "Sample size: " ++ show (length scores)

-- Count occurrences
let counts = foldl' (\m v -> Map.insertWith (+) v 1 m) Map.empty scores
putStrLn $ "Unique values in sample: " ++ show (Map.size counts)

-- Sort by count descending
let sorted = sortOn (Down . snd) (Map.toList counts)

putStrLn "\nCredit score distribution (first 100 rows, sorted by frequency):"
forM_ sorted $ \(score, count) -> putStrLn $ "  " ++ show score ++ ": " ++ show count ++ " occurrences"
```

Capture raw output for analysis of distribution patterns.
