module AnalysisTests(allAnalysisTests) where

import Control.Monad
import Data.List as L

import Analysis
import TestUtils

projPath = "/Users/dillon/Haskell/Bugs/NumStress/"
testCasePath = projPath ++ "test/cases/"

allAnalysisTests = do
  r1 <- testFunctionM numErrorsInModule noErrorsCases
  putStrLn r1
  r2 <- testFunctionM numErrorsInModule oneErrorCases
  putStrLn r2
  r3 <- testFunctionM numErrorsInModule multiErrorCases
  putStrLn r3


numErrorsInModule str = liftM L.length $ moduleErrorsWithTimeLimit 3 str

noErrorPath = testCasePath ++ "noErrors/"

noErrorsCases =
  L.map (\x -> (noErrorPath ++ x, 0))
  ["ne1",
   "ne2",
   "ne3",
   "ne4",
   "ne5"]

oneErrorPath = testCasePath ++ "oneError/"

oneErrorCases =
  L.map (\x -> (oneErrorPath ++ x, 1))
  ["oe1",
   "oe2",
   "oe3",
   "oe4",
   "oe5",
   "oe6",
   "oe7",
   "oe8",
   "oe9",
   "oe10"]

multiErrorPath = testCasePath ++ "manyErrors/"

multiErrorCases =
  L.map (\(x, y) -> (multiErrorPath ++ x, y))
  [("me1", 2)]

runCase path name = moduleErrors $ path ++ name
