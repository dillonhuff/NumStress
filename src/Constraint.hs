module Constraint(Constraint,
                  true, false, eq, dis, con) where

import Data.List as L

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
