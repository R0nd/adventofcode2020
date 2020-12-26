module Day9 where

import Control.Arrow ((&&&))
import Data.List (find, inits)
import Data.Text (lines, unpack)
import Data.Text.IO (readFile)

except i ns = take i ns ++ drop (i + 1) ns

pick (picks, rest) = zipWith (\i v -> (v : picks, except i rest)) [0 ..] rest

picks n ns = map fst $ iterate (foldMap pick) [([], ns)] !! n

valid (prev, next) = elem next . map sum $ picks 2 prev

main = do
  contents <- fmap Data.Text.lines (Data.Text.IO.readFile "9.txt")
  let values = map (read . unpack) contents
  let preambleLength = 25
  let invalid = maybe (error "Invalid number not found") last . find (not . valid . (init &&& last)) $ map (take (preambleLength + 1) . (`drop` values)) [0 .. length values - preambleLength - 1]
  print invalid
  let weakness = maybe (error "Weakness not found") (uncurry (+) . (minimum &&& maximum)) . find ((== invalid) . sum) $ concatMap (takeWhile ((<= invalid) . sum) . inits . (`drop` values)) [0 .. length values - 1]
  print weakness