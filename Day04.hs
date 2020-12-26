module Day4 where

import Data.Bifunctor (first)
import Data.Text (pack, splitOn, unpack)
import Data.Text.IO (readFile)

normalizeWhitespace '\n' = ' '
normalizeWhitespace c = c

readDocument = map ((\x -> (head x, last x)) . map unpack . splitOn (pack ":")) . splitOn (pack " ") . pack . map normalizeWhitespace

readDocuments = map (readDocument . unpack) . splitOn (pack "\n\n")

hasRequiredFields doc = do
  let requiredFields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]
  all (`elem` map fst doc) requiredFields

between (a, b) v = v >= a && v <= b

isValidHeight (value, "in") = between (59, 76) value
isValidHeight (value, "cm") = between (150, 193) value
isValidHeight _ = False

splitHeight height = do
  let valueLength = last $ filter (\n -> all (between ('0', '9')) (take n height)) [0 .. length height]
  first read $ splitAt valueLength height

isValidColor value = (head value == '#') && all (\v -> any (`between` v) [('0', '9'), ('a', 'f')]) (tail value)

isFieldValid ("byr", value) = between (1920, 2002) $ read value
isFieldValid ("iyr", value) = between (2010, 2020) $ read value
isFieldValid ("eyr", value) = between (2020, 2030) $ read value
isFieldValid ("hgt", value) = isValidHeight $ splitHeight value
isFieldValid ("hcl", value) = isValidColor value
isFieldValid ("ecl", value) = value `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
isFieldValid ("pid", value) = length value == 9 && all (between ('0', '9')) value
isFieldValid ("cid", _) = True
isFieldValid _ = False

allFieldsValid = all isFieldValid

main = do
  contents <- Data.Text.IO.readFile "4.txt"
  let documentsWithFields = filter hasRequiredFields $ readDocuments contents
  print $ length documentsWithFields
  print $ length $ filter allFieldsValid documentsWithFields