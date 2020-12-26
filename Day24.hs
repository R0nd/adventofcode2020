module Day24 where

import Data.Bifunctor (bimap, first, second)
import Data.List (group, iterate', nub, sort)
import Data.Text (lines, unpack)
import Data.Text.IO (readFile)

readDirection coord [] = coord
readDirection (row, col) ('n' : 'w' : rest) = readDirection (row + 1, col - 1) rest
readDirection (row, col) ('n' : 'e' : rest) = readDirection (row + 1, col) rest
readDirection (row, col) ('e' : rest) = readDirection (row, col + 1) rest
readDirection (row, col) ('s' : 'e' : rest) = readDirection (row - 1, col + 1) rest
readDirection (row, col) ('s' : 'w' : rest) = readDirection (row - 1, col) rest
readDirection (row, col) ('w' : rest) = readDirection (row, col - 1) rest

adjacent a b = any ((== b) . ($ a)) edges

inc = (+) 1

dec = flip (-) 1

edges = [bimap inc dec, first inc, second inc, bimap dec inc, first dec, second dec]

grow grid = nub . filter (`notElem` grid) $ concatMap (\tile -> map ($tile) edges) grid

score tile = length . filter (adjacent tile)

nextDay grid = filter ((`elem` [1, 2]) . flip score grid) grid ++ filter ((== 2) . flip score grid) (grow grid)

main = do
  contents <- fmap Data.Text.lines (Data.Text.IO.readFile "24.txt")
  let coords = map (readDirection (0, 0) . unpack) contents
  print . length . filter odd . map length . group $ sort coords

  let tiles = concat . filter (odd . length) . group $ sort coords
  print . length . (!! 100) $ iterate' nextDay tiles