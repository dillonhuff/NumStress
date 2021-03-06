module Term(Term,
            symbol, intConstant, termType,
            signDivide, termToZ3Formula, isum, idiff, iprod) where

import Data.List as L
import Data.Word
import Z3.Monad

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
isum a b = Func "+" 2 [a, b]
idiff a b = Func "-" 2 [a, b]
iprod a b = Func "*" 2 [a, b]

termType (Symbol t _) = t
termType (IntConstant w _) = integer w

termToZ3Formula (Func "+" _ [l, r]) = binaryFuncToZ3Formula mkBvadd l r
termToZ3Formula (Func "-" _ [l, r]) = binaryFuncToZ3Formula mkBvsub l r
termToZ3Formula (Func "*" _ [l, r]) = binaryFuncToZ3Formula mkBvmul l r
termToZ3Formula (Func "s-div" _ [l, r]) = binaryFuncToZ3Formula mkBvsdiv l r
termToZ3Formula (IntConstant w value) =  do
  bvW <- mkBvSort $ fromIntegral w
  mkInt64 (fromIntegral value) bvW
termToZ3Formula (Symbol t i) =
  case isInteger t of
    True -> do
      vSym <- mkStringSymbol $ show i
      mkBvVar vSym $ fromIntegral $ intWidth t
    False -> error $ "termToZ3Formula does not support type "  ++ show t

binaryFuncToZ3Formula f l r = do
  lf <- termToZ3Formula l
  rf <- termToZ3Formula r
  f lf rf
