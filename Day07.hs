module Day7 where

import Control.Arrow (Arrow ((&&&)))
import Data.Bifunctor (bimap, second)
import Data.Map (filter, fromList, lookup)
import Data.Maybe (maybe)
import Data.Text (lines, pack, splitOn, unpack)
import Data.Text.IO (readFile)

readBag = unpack . head . splitOn (pack " bags")

readBags "no other bags." = []
readBags x = map (bimap read (readBag . pack) . (head &&& unwords . init . tail) . words . unpack) . splitOn (pack ", ") $ pack x

readEntry = bimap readBag (readBags . unpack) . (head &&& last) . splitOn (pack " contain ")

holds color = elem color . map snd

forEntry def f entries = maybe def f . (`Data.Map.lookup` entries)

holdsNested color entries = uncurry (||) . (holds color &&& any (forEntry False (holdsNested color entries) entries . snd))

bagCount entries = sum . map (uncurry (*) . second ((+ 1) . forEntry 0 (bagCount entries) entries))

main = do
  contents <- fmap Data.Text.lines (Data.Text.IO.readFile "7.txt")
  let entries = fromList $ map readEntry contents
  let color = "shiny gold"
  print $ length $ Data.Map.filter (holdsNested color entries) entries
  print $ maybe 0 (bagCount entries) (Data.Map.lookup color entries)
