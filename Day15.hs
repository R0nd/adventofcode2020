module Day15 where

import Data.IntMap (IntMap, fromList, insert, lookup)
import Data.List (elemIndex, foldl')
import Data.Text (pack, splitOn, unpack)
import Data.Text.IO (readFile)

next (n : ns) = maybe 0 (+ 1) $ elemIndex n ns

makeTurn ns = next ns : ns

nextM ns last idx = maybe 0 (idx -) $! Data.IntMap.lookup last ns

makeTurnM (ns, last) idx = (\x -> (insert last idx ns, x)) $! nextM ns last idx

toIndexMap list = fromList $ zip list [0 ..]

runfold seed n = snd $! foldl' makeTurnM (toIndexMap $ init seed, last seed) [length seed - 1 .. n - 2]

main = do
  contents <- Data.Text.IO.readFile "15.txt"
  let seed = map (read . unpack) $ splitOn (pack ",") contents
  print $ map (runfold seed) [2020, 30000000]