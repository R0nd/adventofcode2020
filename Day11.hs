module Day11 where

import Control.Arrow ((&&&))
import Data.Array (Array, bounds, elems, listArray)
import Data.Array.IArray ((!))
import Data.Bifunctor (bimap)
import Data.Text (lines, unpack)
import Data.Text.IO (readFile)

count x = length . filter (== x)

rayOccupied _ [] = False
rayOccupied grid (point : rest)
  | tile == 'L' = False
  | tile == '#' = True
  | tile == '.' = rayOccupied grid rest
  where
    tile = getTile grid point

between (a, b) x = a <= x && x <= b

bounds2d = bounds &&& bounds . (! 0)

zipWithTuple f a = bimap (f (fst a)) (f (snd a))

hasIndex grid = uncurry (&&) . zipWithTuple between (bounds2d grid)

operations = [id, (+ 1), (+ (-1))]

directions = tail $ concatMap (\x -> map (bimap x) operations) operations

getTile :: Array Int (Array Int Char) -> (Int, Int) -> Char
getTile grid (row, col) = grid ! row ! col

rays :: Array Int (Array Int Char) -> Int -> Int -> Int
rays grid row col = length . filter (rayOccupied grid) $ map (\dir -> takeWhile (hasIndex grid) . tail $ iterate dir (row, col)) directions

adjacent grid row col = count '#' . map (getTile grid) . filter (hasIndex grid) $ map (\dir -> dir (row, col)) directions

change _ _ ('.', _) _ = '.'
change f _ ('L', (row, col)) grid
  | (== 0) $ f grid row col = '#'
  | otherwise = 'L'
change f tolerance ('#', (row, col)) grid
  | (>= tolerance) $ f grid row col = 'L'
  | otherwise = '#'

step f tolerance grid = listArray (0, length grid - 1) $ zipWith (\rowIdx row -> listArray (0, length (grid ! 0) - 1) $ zipWith (\tile colIdx -> change f tolerance (tile, (rowIdx, colIdx)) grid) (elems row) [0 ..]) [0 ..] $ elems grid

prevNext = scanl (\(_, prev) next -> (prev, next)) ([], [])

list = map elems . elems

run f tolerance = sum . map (count '#') . snd . last . takeWhile (uncurry (/=)) . tail . prevNext . map list . iterate (step f tolerance)

main = do
  contents <- fmap Data.Text.lines (Data.Text.IO.readFile "11.txt")
  let grid = listArray (0, length contents - 1) $ map (listArray (0, length (unpack $ head contents) - 1) . unpack) contents
  print $ run adjacent 4 grid
  print $ run rays 5 grid