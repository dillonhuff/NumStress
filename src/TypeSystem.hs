module TypeSystem(TypeT,
                  isAddress) where

data TypeT
  = Address TypeT
  | Integer Int
    deriving (Eq, Ord, Show)

isAddress (Address _) = True
isAddress _ = False

