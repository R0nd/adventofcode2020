module Day20 where

import Data.Bifunctor (second)
import Data.List (find, findIndex, transpose)
import Data.Maybe (fromMaybe, isNothing)
import Data.Text (lines, unpack)
import Data.Text.IO (readFile)

splitOn _ [] = []
splitOn p ns = uncurry (:) . second (splitOn p . drop 1) $ break p ns

binToInt [] = 0
binToInt xs
  | last xs == '#' = 1 + 2 * binToInt (init xs)
  | otherwise = 2 * binToInt (init xs)

digits = filter (`elem` ['0' .. '9'])

fpow n = foldr (.) id . replicate n

compose a = concatMap (\bb -> map (. bb) a)

edges mtx = map (\f -> binToInt $ f mtx) . compose [id, reverse] $ compose [head, last] [id, transpose]

readTile :: [String] -> (Int, [[Char]])
readTile (header : body) = (read $ digits header, body)

fit a = any (flip elem $ edges a) . edges

fith l r = map last l == map head r

fitv t b = last t == head b

adjacent tiles tile = filter (fit tile) . filter (/= tile) $ tiles

rol = map reverse . transpose

mutations = compose [id, reverse] $ map (`fpow` rol) [0 .. 3]

replace pos newVal list = take pos list ++ newVal : drop (pos + 1) list

tryGet row col grid
  | row >= 0 && col >= 0 && row < length grid && col < length grid = grid !! row !! col
  | otherwise = Nothing

fitPredicate row col grid tile = do
  let leftTile = tryGet row (col - 1) grid
  let topTile = tryGet (row - 1) col grid
  maybe True (`fith` tile) leftTile && maybe True (`fitv` tile) topTile

hasMutation p tile = any (p . ($ tile)) mutations

nextTile 0 0 grid tiles = do
  let nextTile = fromMaybe (error "") $ find ((== 2) . length . adjacent tiles) tiles
  let restTiles = filter (/= nextTile) tiles
  let tileMutations = concatMap (`map` restTiles) mutations
  let nextTileGrid = find (\ct -> any (fitv ct) tileMutations && any (fith ct) tileMutations) $ map ($ nextTile) mutations
  let nextGrid = replace 0 (replace 0 nextTileGrid (head grid)) grid
  (nextGrid, restTiles)
nextTile row col grid tiles = do
  let nextTile = fromMaybe (error "") $ find (hasMutation (fitPredicate row col grid)) tiles
  let restTiles = filter (nextTile /=) tiles
  let nextTileGrid = find (fitPredicate row col grid) $ map ($ nextTile) mutations
  let nextGrid = replace row (replace col nextTileGrid (grid !! row)) grid
  (nextGrid, restTiles)

nextGrid (grid, tiles) = do
  let row = fromMaybe (error "") $ findIndex (any isNothing) grid
  let col = fromMaybe (error "") . findIndex isNothing $ grid !! row
  nextTile row col grid tiles

trimTile = map (init . tail) . init . tail

flatten :: [[a]] -> [a]
flatten = foldl1 (++)

mask a b = and $ zipWith (\aa bb -> aa /= '#' || aa == bb) (flatten a) (flatten b)

submatrix row col w h = map (take w . drop col) . take h . drop row

submatrices w h grid = concatMap (\row -> map (\col -> submatrix row col w h grid) [0 .. length (head grid) - w]) [0 .. length grid - h]

count n = sum . map (length . filter (== n))

main = do
  contents <- fmap Data.Text.lines (Data.Text.IO.readFile "20.txt")

  let tiles = map readTile . splitOn null $ map unpack contents
  let cornerTiles = filter ((== 2) . length . adjacent (map snd tiles) . snd) tiles
  print . product $ map fst cornerTiles

  let gridSize = floor . sqrt . fromIntegral $ length tiles
  let grid = replicate gridSize $ replicate gridSize Nothing
  let result = (!! (sum $ map length grid)) $ iterate nextGrid (grid, map snd tiles)
  let image = foldl1 (++) . map (foldl1 (zipWith (++)) . map trimTile) . fromMaybe (error "") . mapM sequence $ fst result

  let monsterMask = ["                  # ", "#    ##    ##    ###", " #  #  #  #  #  #   "]
  let monsterCount = maximum $ map (length . filter (mask monsterMask) . submatrices (length $ head monsterMask) (length monsterMask) . ($ image)) mutations
  print $ count '#' image - monsterCount * count '#' monsterMask