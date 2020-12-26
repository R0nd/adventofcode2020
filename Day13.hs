module Day13 where

import Data.Bifunctor (bimap, second)
import Data.List (find, minimumBy)
import Data.Maybe (fromMaybe)
import Data.Text (lines, pack, splitOn, unpack)
import Data.Text.IO (readFile)

readConstraints = map (second read) . filter ((/= "x") . snd) . zip [0 ..] . map unpack . splitOn (pack ",")

checkConstraint time (offset, busId) = mod (time + offset) busId == 0

tuplify (a : b) = (a, head b)

departure startTime busId = ceiling ((fromIntegral startTime :: Double) / fromIntegral busId) * busId - startTime

compareSnd (_, a) (_, b) = compare a b

nextBus [] _ offset = offset
nextBus (constraint : rest) interval offset = nextBus rest (interval * snd constraint) . fromMaybe (error "No bus") . find (`checkConstraint` constraint) $ iterate (interval +) offset

main = do
  contents <- fmap Data.Text.lines (Data.Text.IO.readFile "13.txt")
  let (startTime, constraints) = bimap ((\s -> read s :: Int) . unpack) readConstraints $ tuplify contents

  let busIds = map snd constraints
  let departures = zipWith (curry (second (departure startTime))) busIds busIds
  print . uncurry (*) $ minimumBy compareSnd departures

  print $ nextBus constraints 1 0