module Day16 where

import Control.Arrow ((&&&))
import Data.Bifunctor (second)
import Data.List (findIndices, isPrefixOf, notElem, transpose)
import Data.Text (lines, null, pack, splitOn, unpack)
import Data.Text.IO (readFile)

between [a, b] v = v >= a && v <= b

readRange = map (read . unpack) . splitOn (pack "-") . pack

readRule s = (unpack . head &&& (\r v -> any (flip between v . readRange . unpack) r) . splitOn (pack " or ") . head . tail) $ splitOn (pack ": ") s

validValue rules v = any (`snd` v) rules

readTicket = map (read . unpack) . splitOn (pack ",")

validTicket rules = all $ validValue rules

orderedValidTicket = zipWith snd

paragraphs [] = []
paragraphs a = uncurry (:) . second (paragraphs . dropWhile Data.Text.null) $ break Data.Text.null a

removeExcess xs [n] = [n]
removeExcess xs ns = filter (`notElem` xs) ns

eliminate ns
  | all ((== 1) . length) ns = ns
  | otherwise = eliminate $ map (\n -> flip removeExcess n . concat $ filter ((== 1) . length) ns) ns

main = do
  contents <- fmap Data.Text.lines (Data.Text.IO.readFile "16.txt")
  let [rulesParagraph, myTicketParagraph, nearbyTicketsParagraph] = paragraphs contents
  let rules = map readRule rulesParagraph
  print . sum . filter (not . validValue rules) . concatMap readTicket $ tail nearbyTicketsParagraph

  let validTickets = filter (validTicket rules) . map readTicket $ tail nearbyTicketsParagraph
  let myTicket = readTicket $ last myTicketParagraph
  print . product . map (myTicket !!) . findIndices ("departure" `isPrefixOf`) . concat . eliminate . map (\vt -> map fst $ filter (\r -> validTicket [r] vt) rules) $ transpose validTickets