module Day3 where

import qualified Data.Text as Text
import qualified Data.Text.IO as Text

teecat f src = src : f src

step (dx, dy) a (x, y)
  | y < (length a - 1) = teecat (step (dx, dy) a) (mod (x + dx) (length $ head a), y + dy)
  | otherwise = []

main = do
  contents <- fmap Text.lines (Text.readFile "3.txt")
  let field = map Text.unpack contents

  let slopes = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]

  let treeCount slope = length $ filter ('#' ==) $ map (\(x, y) -> field !! y !! x) $ step slope field (0, 0)
  let treeCounts = map treeCount slopes
  print treeCounts
  print $ product treeCounts