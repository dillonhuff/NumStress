module Constraint(Constraint,
                  true, false, eq, dis, con) where

import Term

data Constraint
  = Dis [Constraint]
  | Con [Constraint]
  | Not Constraint
  | Predicate String Int [Term]
  | T
  | F
    deriving (Eq, Ord, Show)

dis = Dis
con = Con
false = F
true = T
eq a b = Predicate "==" 2 [a, b]
