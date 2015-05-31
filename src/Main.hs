module Main(main) where

import LLVM.General
import LLVM.General.Analysis
import LLVM.General.AST
import LLVM.General.Context

import Analysis
import Utils

fileName = "/Users/dillon/CWorkspace/git/git/archive.o"

main = do
  mod <- parseBitcode fileName
  case mod of
    Left err -> putStrLn $ "Parsing error: " ++ err
    Right m -> analyzeModuleWithTimeLimit 10 m >>= (\errs -> putStrLn $ show errs)
  

