module Day2 where

import qualified Data.Text as Text
import qualified Data.Text.IO as Text

data Policy = Policy
  { constraint :: [Int],
    letter :: Char
  }

data Entry = Entry
  { policy :: Policy,
    password :: String
  }

readConstraint = (\x -> [head x .. last x]) . map (read . Text.unpack) . Text.splitOn (Text.pack "-")

readPolicy = (\[x, y] -> Policy (readConstraint x) (head $ Text.unpack y)) . Text.splitOn (Text.pack " ")

listToTuple [x, y] = (x, y)
listToTuple l = error $ "Unexpected list: " ++ show l

readEntry = (\(x, y) -> Entry (readPolicy x) (Text.unpack y)) . listToTuple . Text.splitOn (Text.pack ": ")

isSledValid e = (\x -> x `elem` constraint (policy e)) $ length $ filter ((letter $ policy e) ==) $ password e

isTobogganValid e = (== 1) . length . filter ((letter $ policy e) ==) . map (\i -> password e !! (i - 1)) . (\c -> [head c, last c]) . constraint $ policy e

main = do
  contents <- fmap Text.lines (Text.readFile "2.txt")
  let entries = map readEntry contents

  print $ length $ filter isSledValid entries
  print $ length $ filter isTobogganValid entries