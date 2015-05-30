module Term(Term,
            symbol, intConstant, symbolType,
            signDivide) where

import Data.List as L
import Data.Word

import TypeSystem

data Term
  = Func String Int [Term]
  | IntConstant Word32 Integer
  | Symbol TypeT Int
    deriving (Eq, Ord)

instance Show Term where
  show (Symbol t i) = "($" ++ show i ++ ", " ++ show t ++ ")"
  show (IntConstant w i) = show i
  show (Func n _ args) = "(" ++ n ++ " " ++ (L.concat $ L.intersperse " " $ L.map show args) ++ ")"
  

symbol = Symbol
intConstant width val = IntConstant width val

signDivide a b = Func "s-div" 2 [a, b]

symbolType (Symbol t _) = t
