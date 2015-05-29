module Error(NSError) where

data NSError
  = DivZero
    deriving (Eq, Ord, Show)
