module Day14 where

import Control.Arrow ((&&&))
import Data.Bits (clearBit, complement, setBit, (.&.), (.|.))
import Data.List (elemIndices)
import Data.Map (empty, insert)
import Data.Text (lines, pack, splitOn, unpack)
import Data.Text.IO (readFile)
import Data.Word (Word64)

binToInt :: Char -> String -> Word64
binToInt _ [] = 0
binToInt highChar xs
  | last xs == highChar = 1 + 2 * binToInt highChar (init xs)
  | otherwise = 2 * binToInt highChar (init xs)

readMask = binToInt '1' &&& binToInt '0'

nor a b = (.&.) a $ complement b

isDigit = flip elem ['0' .. '9']

readMemDst = read . filter isDigit

applyMask (orMask, norMask) = flip nor norMask . (.|.) orMask

process (_, mem) ["mask", value] = (readMask value, mem)
process (mask, mem) [memDst, value] = (mask, insert (readMemDst memDst) (applyMask mask $ read value) mem)

float [] memDst = [memDst]
float (idx : rest) memDst = concatMap (float rest . ($ 35 - idx) . ($ memDst)) [setBit, clearBit]

applyMemMask maskString memDst = float (elemIndices 'X' maskString) $ (.|.) (binToInt '1' maskString) memDst

processMem (_, mem) ["mask", value] = (value, mem)
processMem (mask, mem) [memDst, value] = (mask, foldl (\nextmem dst -> insert dst (read value) nextmem) mem . applyMemMask mask $ readMemDst memDst)

run f seed = sum . snd . foldl f seed . map (map unpack . splitOn (pack " = "))

main = do
  contents <- fmap Data.Text.lines (Data.Text.IO.readFile "14.txt")

  print $ run process ((0, 0), empty) contents
  print $ run processMem ("", empty) contents