module Day21 where

import Control.Arrow ((&&&))
import Data.Bifunctor (bimap)
import Data.List (intercalate, intersect, nub, sortOn, (\\))
import Data.Text (lines, pack, splitOn, stripPrefix, stripSuffix, unpack)
import Data.Text.IO (readFile)

readEntry = bimap (words . unpack) (maybe (error "") (map unpack . splitOn (pack ", ")) . maybe (error "") (stripSuffix (pack ")")) . stripPrefix (pack "contains ")) . (head &&& last) . splitOn (pack " (")

findIngredient entries allergen = foldl1 intersect . map fst $ filter (elem allergen . snd) entries

reduce ingredients = filterMap ((> 1) . length) (\\ concat (filter ((== 1) . length) ingredients)) ingredients

final = all ((== 1) . length)

iif True a _ = a
iif False _ b = b

filterMap p m = map (\x -> iif (p x) m id x)

main = do
  contents <- fmap Data.Text.lines (Data.Text.IO.readFile "21.txt")
  let entries = map readEntry contents

  let allergens = nub $ concatMap snd entries
  let pairs = zip allergens . map head . head . dropWhile (not . final) . iterate reduce $ map (findIngredient entries) allergens
  let ingredients = map snd pairs
  print . length . filter (`notElem` ingredients) $ concatMap fst entries
  print . intercalate "," . map snd $ sortOn fst pairs