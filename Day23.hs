module Day23 where

import Control.Arrow ((&&&))
import Data.IntMap (fromList, insert, (!))
import Data.List (elemIndex, iterate')
import Data.Maybe (fromJust)
import Data.Text (lines, unpack)
import Data.Text.IO (readFile)

decMod v = mod (v - 1)

move len (cups, cur) = do
  let picked = take 3 . tail $ iterate (cups !) cur
  let dst = head . dropWhile (`elem` 0 : picked) . tail $ iterate (`decMod` (len + 1)) cur
  let nextCups = insert (last picked) (cups ! dst) . insert dst (head picked) $ insert cur (cups ! last picked) cups
  (nextCups, nextCups ! cur)

readDigit = fromJust . flip elemIndex ['0' .. '9']

main = do
  contents <- fmap Data.Text.lines (Data.Text.IO.readFile "23.txt")
  let cups = map readDigit . unpack $ head contents

  let cupsP1 = zip cups (tail $ cycle cups)
  let resultP1 = fst . (!! 100) $ iterate' (move (length cups)) (fromList cupsP1, head cups)
  print . concatMap show . take (length cups - 1) . tail $ iterate (resultP1 !) 1

  let len = 1000000
  let extraCupStart = length cupsP1 + 1
  let cupsP2 = (:) (len, head cups) . take (len - 1) $ init cupsP1 ++ [(last cups, extraCupStart)] ++ map (id &&& (+) 1) [extraCupStart ..]
  let resultP2 = fst . (!! (len * 10)) $ iterate' (move len) (fromList cupsP2, head cups)
  print . product . take 2 . tail $ iterate (resultP2 !) 1