module Day22 where

import Data.Bifunctor (bimap, first, second)
import Data.List (sort)
import Data.Text (all, lines, null, unpack)
import Data.Text.IO (readFile)

number x = not (Data.Text.null x) && Data.Text.all (`elem` ['0' .. '9']) x

iif True a _ = a
iif False _ b = b

nextTurn (a, b) = bimap tail tail $ iif (head a > head b) first second (++ (reverse $ sort [head a, head b])) (a, b)

done = uncurry (||) . bimap Prelude.null Prelude.null

recursiveTurn ((a, b) : states)
  | (a, b) `elem` states = [(a, [])]
  | otherwise = do
    let (cardA : restA) = a
    let (cardB : restB) = b
    let winner = iif (length restA >= cardA && length restB >= cardB) (not . Prelude.null . fst . head $ recursiveGame (take cardA restA, take cardB restB)) (cardA > cardB)
    iif winner [(restA ++ [cardA, cardB], restB)] [(restA, restB ++ [cardB, cardA])] ++ (a, b) : states

recursiveGame = dropWhile (not . done) . map head . iterate recursiveTurn . (: [])

score = sum . zipWith (*) [1 ..] . reverse

main = do
  contents <- fmap Data.Text.lines (Data.Text.IO.readFile "22.txt")
  let state = bimap (map (read . unpack)) (map (read . unpack) . dropWhile (not . number)) . span number $ dropWhile (not . number) contents
  let winning = uncurry (++) . head . dropWhile (not . done) $ iterate nextTurn state
  print $ score winning

  let recursiveWinning = uncurry (++) . head $ recursiveGame state
  print $ score recursiveWinning