module ExecutionState(ExecutionState,
                      memstates,
                      executionState,
                      nextInstruction,
                      selectTopMemState,
                      addMemStates,
                      liveStates,
                      errorsDetected) where

import Data.List as L

import Error
import InstructionSet
import InstructionStream
import MemoryState

data ExecutionState
  = ExecutionState {
    memstates :: [MemoryState],
    iStream :: InstructionStream
    } deriving (Eq, Ord, Show)

executionState = ExecutionState

selectTopMemState (ExecutionState (m:ms) is) = (m, ExecutionState ms is)

addMemStates newMS (ExecutionState ms is) = ExecutionState (newMS ++ ms) is

liveStates e = L.filter isLive $ memstates e
errorsDetected e = L.map extractError $ L.filter isError $ memstates e

instructionAt ind e = getInstr ind $ iStream e

nextInstruction :: ExecutionState -> MemoryState -> Instr
nextInstruction es ms =
  instructionAt (ip ms) es
