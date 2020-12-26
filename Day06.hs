module Day6 where

import Data.List (intersect)
import Data.Text (pack, splitOn, unpack)
import Data.Text.IO (readFile)

iif True a _ = a
iif False _ b = b

uniq = foldr (\v acc -> iif (v `elem` acc) acc (v : acc)) []

readGroup = map unpack . splitOn (pack "\n")

readAnyGroup = uniq . concat . readGroup

readAllGroup = foldr1 intersect . readGroup

readGroups p = map p . splitOn (pack "\n\n")

main = do
  contents <- Data.Text.IO.readFile "6.txt"
  print $ map (sum . map length . ($ contents) . readGroups) [readAnyGroup, readAllGroup]