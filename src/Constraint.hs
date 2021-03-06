module Constraint(Constraint,
                  true, false, eq, neq, dis, con, Constraint.not,
                  sgt, slt,
                  isSAT) where

import Data.List as L
import Z3.Monad

import Term

data Constraint
  = Dis [Constraint]
  | Con [Constraint]
  | Not Constraint
  | Predicate String Int [Term]
  | T
  | F
    deriving (Eq, Ord)

instance Show Constraint where
  show T = "true"
  show F = "false"
  show (Predicate s _ ts) = "(" ++ s ++ " " ++ (L.concat $ L.intersperse " " $ L.map show ts) ++ ")"
  show (Dis args) = "(or " ++ (L.concat $ L.intersperse " " $ L.map show args) ++ ")"
  show (Con args) = "(and " ++ (L.concat $ L.intersperse " " $ L.map show args) ++ ")"
  show (Not arg) = "(not " ++ show arg ++ ")"

dis = Dis
con = Con
false = F
true = T
not = Not
eq a b = Predicate "==" 2 [a, b]
neq a b = Constraint.not $ Predicate "==" 2 [a, b]
sgt a b = Predicate "s->" 2 [a, b]
slt a b = Predicate "s-<" 2 [a, b]

constraintToZ3Formula T = mkTrue
constraintToZ3Formula F = mkFalse
constraintToZ3Formula (Predicate "==" _ [l, r]) = binaryPredToZ3Formula mkEq l r
constraintToZ3Formula (Predicate "s->" _ [l, r]) = binaryPredToZ3Formula mkBvsgt l r
constraintToZ3Formula (Predicate "s-<" _ [l, r]) = binaryPredToZ3Formula mkBvslt l r
constraintToZ3Formula (Not e) = do
  eF <- constraintToZ3Formula e
  mkNot eF
constraintToZ3Formula (Dis args) = do
  argsAsZ3Formulas <- mapM constraintToZ3Formula args
  mkOr argsAsZ3Formulas
constraintToZ3Formula (Con args) = do
  argsAsZ3Formulas <- mapM constraintToZ3Formula args
  mkAnd argsAsZ3Formulas

binaryPredToZ3Formula p l r = do
  lForm <- termToZ3Formula l
  rForm <- termToZ3Formula r
  p lForm rForm  

isSAT c = evalZ3 $ isSATZ3 c

isSATZ3 :: Constraint -> Z3 Bool
isSATZ3 c = do
  z3CF <- constraintToZ3Formula c
  assert z3CF
  res <- check
  case res of
    Unsat -> return False
    Sat -> return True
    other -> error $ "isSatisfiable: invalid result " ++ show other

