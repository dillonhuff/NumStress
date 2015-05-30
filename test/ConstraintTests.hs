module ConstraintTests(allConstraintTests) where

import Data.List as L

import Constraint
import Term
import TestUtils

allConstraintTests = do
  r1 <- testFunctionM isSAT unsatCases
  putStrLn r1

unsatCases =
  L.map (\x -> (x, False))
  [false,
   eq (intConstant 32 4) (intConstant 32 0)]
