module Main(main) where

import LLVM.General
import LLVM.General.Analysis
import LLVM.General.AST
import LLVM.General.Context

import Analysis
import Utils

fileName = "/Users/dillon/Haskell/Bugs/NumStress/test/cases/oneError/oe7"

main = do
  cSourceToBitcode fileName
  printModule $ fileName ++ ".bc"

