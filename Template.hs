module Template where

import Data.Text (lines)
import Data.Text.IO (readFile)

main = do
  contents <- fmap Data.Text.lines (Data.Text.IO.readFile ".txt")
  print contents