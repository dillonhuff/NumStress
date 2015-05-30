module Error(NSError, divZeroError) where

data NSError
  = DivZero
    deriving (Eq, Ord, Show)

divZeroError = DivZero
