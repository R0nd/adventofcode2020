module Day19 where

import Control.Arrow ((&&&))
import Data.Bifunctor (bimap)
import Data.Either (Either, fromLeft, isRight, lefts, rights)
import Data.IntMap (IntMap, fromList, insert, (!))
import Data.Text (Text, lines, null, pack, splitOn, unpack)
import Data.Text.IO (readFile)

type Chain = [Either Int Char]

orRule :: [[Int]] -> [Chain]
orRule = map seqRule

seqRule :: [Int] -> Chain
seqRule = map Left

consumeRule :: Char -> [Chain]
consumeRule c = [[Right c]]

readRule :: String -> [Chain]
readRule ['"', c, '"'] = consumeRule c
readRule s
  | '|' `elem` s = orRule . map (map read . words . unpack) . splitOn (pack " | ") $ pack s
  | otherwise = (: []) . seqRule . map read $ words s

readEntry :: Text -> (Int, [Chain])
readEntry s = bimap read readRule . (head &&& last) . map unpack $ splitOn (pack ": ") s

split :: Chain -> (Chain, Int, Chain)
split chain = (takeWhile isRight chain, fromLeft (error "") . head $ dropWhile isRight chain, tail $ dropWhile isRight chain)

reduceChain :: IntMap [Chain] -> Chain -> [Chain]
reduceChain ruleMap chain = (\(x, y, z) -> map (\yy -> x ++ yy ++ z) $ ruleMap ! y) $ split chain

matchChain :: Chain -> String -> Bool
matchChain chain message = and . zipWith (==) message . rights $ takeWhile isRight chain

matchRule :: IntMap [Chain] -> Chain -> String -> Bool
matchRule ruleMap chain message
  | Prelude.null $ lefts chain = matchChain chain message && uncurry (==) (bimap length length (chain, message))
  | otherwise = length chain <= length message && matchChain chain message && any (\nextChain -> matchRule ruleMap nextChain message) (reduceChain ruleMap chain)

main = do
  contents <- fmap Data.Text.lines (Data.Text.IO.readFile "19.txt")
  let ruleMap = fromList . map readEntry $ takeWhile (not . Data.Text.null) contents
  let messages = map unpack . tail $ dropWhile (not . Data.Text.null) contents

  print . length $ filter (matchRule ruleMap [Left 0]) messages

  let fixedRuleMap = insert 11 (orRule [[42, 31], [42, 11, 31]]) $ insert 8 (orRule [[42], [42, 8]]) ruleMap
  print . length $ filter (matchRule fixedRuleMap [Left 0]) messages