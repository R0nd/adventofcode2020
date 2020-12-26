module Day17 where

import Data.List (elemIndices, maximum, minimum, transpose)
import Data.Text (lines, unpack)
import Data.Text.IO (readFile)

bounds = map (\dim -> [minimum dim - 1 .. maximum dim + 1]) . transpose

adjacent a b = (\diff -> all (<= 1) diff && any (> 0) diff) . map abs $ zipWith (-) a b

score p = length . filter (adjacent p)

nextState p field
  | any (\f -> and $ zipWith (==) f p) field = (`elem` [2, 3]) $ score p field
  | otherwise = (== 3) $ score p field

nextField field = filter (`nextState` field) . sequence $ bounds field

readField dimensions = concat . zipWith (\y -> map (\x -> take dimensions $ [x, y] ++ repeat 0)) [0 ..] . map (elemIndices '#' . unpack)

main = do
  contents <- fmap Data.Text.lines (Data.Text.IO.readFile "17.txt")
  let field = readField 3 contents
  print . length . (!! 6) $ iterate nextField field

  let hyperfield = readField 4 contents
  print . length . (!! 6) $ iterate nextField hyperfield