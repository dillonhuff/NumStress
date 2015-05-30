module InstructionStream(InstructionStream,
                         instructionStream,
                         getInstr) where

import Data.Map as M

import InstructionSet

data InstructionStream
  = InstructionStream (Map Int Instr)
    deriving (Eq, Ord, Show)

instructionStream = InstructionStream

getInstr ind (InstructionStream m) =
  case M.lookup ind m of
    Just i -> i
    Nothing -> error $ "Error in getInstr: cannot find " ++ show ind ++ " in " ++ show m
