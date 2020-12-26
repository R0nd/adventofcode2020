module Day25 where

import Data.List (elemIndex, iterate')
import Data.Maybe (fromJust)
import Data.Text (lines, unpack)
import Data.Text.IO (readFile)

transform subject = iterate' (flip rem 20201227 . (*) subject) 1

main = do
  contents <- fmap Data.Text.lines (Data.Text.IO.readFile "25.txt")
  let pubKeys = map (read . unpack) contents
  let loopSize = fromJust . elemIndex (head pubKeys) $ transform 7
  print . (!! loopSize) . transform $ last pubKeys