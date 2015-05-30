module Constraint(Constraint,
                  true, false, eq, dis, con,
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
eq a b = Predicate "==" 2 [a, b]

constraintToZ3Formula T = mkTrue
constraintToZ3Formula F = mkFalse
constraintToZ3Formula (Predicate "==" _ [l, r]) = do
  lForm <- termToZ3Formula l
  rForm <- termToZ3Formula r
  mkEq lForm rForm
constraintToZ3Formula (Not e) = do
  eF <- constraintToZ3Formula e
  mkNot eF
constraintToZ3Formula (Dis args) = do
  argsAsZ3Formulas <- mapM constraintToZ3Formula args
  mkOr argsAsZ3Formulas
constraintToZ3Formula (Con args) = do
  argsAsZ3Formulas <- mapM constraintToZ3Formula args
  mkAnd argsAsZ3Formulas

isSAT c = evalZ3 $ isSATZ3 c

isSATZ3 :: Constraint -> Z3 Bool
isSATZ3 c = do
  z3CF <- constraintToZ3Formula c
  res <- error $ show z3CF --check
  case res of
    Unsat -> return False
    Sat -> return False
    other -> error $ "isSatisfiable: invalid result " ++ show other

