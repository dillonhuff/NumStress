module Analysis(moduleErrors) where

import Control.Monad.State.Lazy
import Data.List as L
import Data.Map as M
import LLVM.General
import LLVM.General.Analysis
import LLVM.General.AST
import LLVM.General.Context

import Constraint
import Error
import InstructionSet
import LLVMUtils
import MemoryState
import Term
import TypeSystem
import Utils

valueFromName ((:=) _ v) = v
valueFromName (Do v) = v

extractGlobal (GlobalDefinition d) = d

isFunctionDef (GlobalDefinition (Function _ _ _ _ _ _ _ _ _ _ _ _)) = True
isFunctionDef _ = False

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

moduleErrors :: String -> IO [NSError]
moduleErrors str = do
  parseRes <- parseModule str
  case parseRes of
    Left err -> error $ show err
    Right mod -> return $ analyzeModule mod

analyzeModule mod = L.concatMap (\d -> analyzeFunction $ extractGlobal d) $ L.filter isFunctionDef $ moduleDefinitions mod

analyzeFunction :: Global -> [NSError]
analyzeFunction (Function _ _ _ _ _ _ (ps,True) _ _ _ _ _) = []
analyzeFunction (Function _ _ _ _ _ _ (ps,False) _ _ _ _ bbs) =
  evalState (symExeFunc ps bbs) 0

symExeFunc :: [Parameter] -> [BasicBlock] -> State Int [NSError]
symExeFunc ps bbs = do
  startingExec <- initializeExecutionState ps bbs
  symExe startingExec

initializeExecutionState :: [Parameter] -> [BasicBlock] -> State Int ExecutionState
initializeExecutionState ps bbs = do
  initMemState <- initialMemoryState ps
  return $ executionState [initMemState] $ basicBlocksToIStream bbs

initialMemoryState :: [Parameter] -> State Int MemoryState
initialMemoryState ps =
  let initSt = initMemState M.empty M.empty in
  foldM addParameter initSt ps

addParameter :: MemoryState -> Parameter -> State Int MemoryState
addParameter ms (Parameter t n _) =
  let typeT = llvmTypeToTypeT t in
  addNamedSymbol typeT n ms

basicBlocksToIStream :: [BasicBlock] -> InstructionStream
basicBlocksToIStream bbs =
  let instrList = L.concatMap basicBlockToInstrList bbs
      numberedInstrs = L.zip [1..(length instrList)] instrList in
  instructionStream $ M.fromList numberedInstrs

data InstructionStream
  = InstructionStream (Map Int Instr)
    deriving (Eq, Ord, Show)

instructionStream = InstructionStream

getInstr ind (InstructionStream m) =
  case M.lookup ind m of
    Just i -> i
    Nothing -> error $ "Error in getInstr: cannot find " ++ show ind ++ " in " ++ show m

symExe :: ExecutionState -> State Int [NSError]
symExe execState =
  case liveStates execState of
    [] -> return $ errorsDetected execState
    states -> pickStateAndExecute execState >>= symExe

pickStateAndExecute :: ExecutionState -> State Int ExecutionState
pickStateAndExecute execState =
  let (memStateToExec, execStateWithoutMemStateToExec) = selectTopMemState execState in
  do
    resultStates <- runMemState execStateWithoutMemStateToExec memStateToExec
    return $ addMemStates resultStates execStateWithoutMemStateToExec

runMemState :: ExecutionState -> MemoryState -> State Int [MemoryState]
runMemState es ms =
  let i = nextInstruction es ms in
  executeInstruction i es ms

executeInstruction :: Instr -> ExecutionState -> MemoryState -> State Int [MemoryState]
executeInstruction i es ms =
  case opcode i of
    RET -> return [setCompleted ms]
    RETVAL -> return [setCompleted ms]
    LABEL -> return [incrementIP ms]
    ALLOCA -> execAlloca i ms
    STORE -> return $ execStore i ms
    LOAD -> execLoad i ms
    SDIV -> return $ execSDiv i ms
    other -> error $ "executeInstruction does not support " ++ show other

execAlloca i ms =
  let x = receivingOp i in
  do
    newMS <- addOpSymbol (InstructionSet.allocatedType i) x ms
    return [incrementIP $ newMS]

execStore i ms =
  let a = storeValue i
      b = storeLoc i in
  [incrementIP $ setConstraint (\c -> con [c, eq (deref b ms) (valueSym a ms)]) ms]

execLoad :: Instr -> MemoryState -> State Int [MemoryState]
execLoad i ms =
  let a = receivingOp i
      b = loadLoc i in
  do
    newMS <- addOpSymbol (opType a) a ms
    return [incrementIP $ setConstraint (\c -> con [c, eq (valueSym a newMS) (deref b newMS)]) newMS]

execSDiv i ms =
  error $ show ms

nextInstruction :: ExecutionState -> MemoryState -> Instr
nextInstruction es ms =
  instructionAt (ip ms) es
