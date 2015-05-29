module Main(main) where

import Analysis

fileName = "tiny"

main = do
  cSourceToBitcode $ fileName ++ ".c"
  printModule $ File $ fileName ++ ".bc"

