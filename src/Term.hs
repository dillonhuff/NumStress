module Term(Term, symbol) where

import Data.List as L

import TypeSystem

data Term
  = Func String Int [Term]
  | IntConstant Int Integer
  | Symbol TypeT Int
    deriving (Eq, Ord)

instance Show Term where
  show (Symbol t i) = "($" ++ show i ++ ", " ++ show t ++ ")"
  show (IntConstant w i) = show i
  show (Func n _ args) = "(" ++ n ++ " " ++ (L.concat $ L.intersperse " " $ L.map show args) ++ ")"
  

symbol = Symbol

