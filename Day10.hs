module Day10 where

import Data.List (group, sort)
import Data.Text (lines, unpack)
import Data.Text.IO (readFile)

count a = length . filter (== a)

differences chain = zipWith (-) (tail chain) chain

removals n = (n - 2) * (n - 1) `div` 2 + n

main = do
  contents <- fmap Data.Text.lines (Data.Text.IO.readFile "10.txt")
  let adapters = sort $ map (read . unpack) contents
  let device = 3 + last adapters

  let chain = 0 : adapters ++ [device]
  print . product $ map (`count` differences chain) [1, 3]
  print . product . map (removals . length) . filter ((== 1) . head) . group $ differences chain