module InstructionStream(InstructionStream,
                         instructionStream,
                         labelIndex,
                         getInstr) where

import Data.List as L
import Data.Map as M

import InstructionSet
import Utils

data InstructionStream
  = InstructionStream (Map Int Instr)
    deriving (Eq, Ord)

instance Show InstructionStream where
  show (InstructionStream m) = prettyMap m

instructionStream = InstructionStream

getInstr ind (InstructionStream m) =
  case M.lookup ind m of
    Just i -> i
    Nothing -> error $ "Error in getInstr: cannot find " ++ show ind ++ " in " ++ show m

labelIndex l is@(InstructionStream m) =
  case L.lookup l $ L.map (\(x, y) -> (y, x)) $ M.toList m of
    Just ind -> ind
    Nothing -> error $ "labelIndex for label " ++ show l ++ " failed on " ++ show is
