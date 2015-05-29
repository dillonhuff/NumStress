module TypeSystem(TypeT,
                  integer, address,
                  typePointedTo,
                  isAddress) where

import Data.Word

data TypeT
  = Address TypeT
  | Integer Word32
    deriving (Eq, Ord, Show)

integer = Integer
address = Address

isAddress (Address _) = True
isAddress _ = False

typePointedTo (Address t) = t
