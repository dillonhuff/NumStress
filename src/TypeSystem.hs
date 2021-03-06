module TypeSystem(TypeT,
                  integer, address,
                  typePointedTo,
                  isAddress, isInteger, intWidth) where

import Data.Word

data TypeT
  = Address TypeT
  | Integer Word32
    deriving (Eq, Ord)

instance Show TypeT where
  show (Address t) = "&" ++ show t
  show (Integer w) = "i" ++ show w

integer = Integer
address = Address

isAddress (Address _) = True
isAddress _ = False

isInteger (Integer _) = True
isInteger _ = False

typePointedTo (Address t) = t

intWidth (Integer w) = w
