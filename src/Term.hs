module Term(Term, symbol) where

import TypeSystem

data Term
  = Func String Int [Term]
  | IntConstant Int Integer
  | Symbol TypeT Int
    deriving (Eq, Ord, Show)

symbol = Symbol

