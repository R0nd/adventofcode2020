module Day8 where

import Control.Arrow ((&&&))
import Data.Bifunctor (bimap, first, second)
import Data.List (find, findIndices)
import Data.Text (lines, unpack)
import Data.Text.IO (readFile)

readSign '+' = (+)
readSign '-' = (-)
readSign _ = error "Invalid sign"

readOperation "acc" = \sign val -> bimap (`sign` val) (+ 1)
readOperation "jmp" = \sign val -> second (`sign` val)
readOperation "nop" = \_ _ -> second (+ 1)
readOperation _ = error "Invalid operation"

readInstruction = (\(op : ((sign : val) : _)) -> readOperation op (readSign sign) (read val)) . words

switch a b src
  | src == a = b
  | src == b = a
  | otherwise = src

swapOperation a b = unwords . uncurry (:) . first (switch a b) . (head &&& tail) . words

execute instructions history (acc, ins)
  | ins `elem` history = (acc, True)
  | ins == length instructions = (acc, False)
  | otherwise = execute instructions (ins : history) $ (instructions !! ins) (acc, ins)

main = do
  contents <- fmap Data.Text.lines (Data.Text.IO.readFile "8.txt")
  let instructionContents = map unpack contents
  let instructions = map readInstruction instructionContents
  print . fst $ execute instructions [] (0, 0)

  let altInstructionContents = map (swapOperation "jmp" "nop") instructionContents
  let variantIndices = findIndices (uncurry (/=)) $ zip instructionContents altInstructionContents
  let altInstructions = map readInstruction altInstructionContents
  let instructionVariants = map (\i -> take (i - 1) instructions ++ (altInstructions !! i) : drop i instructions) variantIndices
  print $ maybe 0 fst . find (not . snd) $ map (\iv -> execute iv [] (0, 0)) instructionVariants