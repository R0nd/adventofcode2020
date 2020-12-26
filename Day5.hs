module Day5 where

import Data.Bifunctor (bimap)
import Data.List ((\\))
import Data.Text (lines, unpack)
import Data.Text.IO (readFile)

fbToBin 'F' = 0
fbToBin 'B' = 1
fbToBin _ = error "Invalid char"

lrToBin 'L' = 0
lrToBin 'R' = 1
lrToBin _ = error "Invalid char"

binToint :: [Int] -> Int
binToint = foldl (\acc x -> acc * 2 + x) 0

readSeat = bimap (binToint . map fbToBin) (binToint . map lrToBin) . splitAt 7

seatId :: (Int, Int) -> Int
seatId (x, y) = x * 8 + y

range :: [Int] -> [Int]
range x = [minimum x .. maximum x]

main = do
  contents <- fmap Data.Text.lines (Data.Text.IO.readFile "5.txt")
  let seatIds = map (seatId . readSeat . unpack) contents
  print $ maximum seatIds
  print $ range seatIds \\ seatIds