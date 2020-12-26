module Day1 where

import Data.List
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

combinations k ns = filter ((k ==) . length) $ subsequences ns

except i ns = take i ns ++ drop (i + 1) ns

pick (picks, rest) = zipWith (\i v -> (v : picks, except i rest)) [0 ..] rest

picks n ns = map fst $ iterate (foldMap pick) [([], ns)] !! n

main = do
  contents <- fmap Text.lines (Text.readFile "1.txt")
  let entries = map (read . Text.unpack) contents :: [Int]

  let entryPairs = picks 2 entries
  let solutionPair = find ((2020 ==) . sum) entryPairs
  print solutionPair
  case solutionPair of
    Just x -> print (product x)

  let entryTriples = picks 3 entries
  let solutionTriple = find ((2020 ==) . sum) entryTriples
  print solutionTriple
  case solutionTriple of
    Just x -> print (product x)