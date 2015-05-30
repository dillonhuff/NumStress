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
import ExecutionState
import InstructionSet
import InstructionStream
import LLVMUtils
import MemoryState
import Term
import TypeSystem
import Utils

extractGlobal (GlobalDefinition d) = d

isFunctionDef (GlobalDefinition (Function _ _ _ _ _ _ _ _ _ _ _ _)) = True
isFunctionDef _ = False

moduleErrors :: String -> IO [NSError]
moduleErrors str = do
  parseRes <- parseModule str
  case parseRes of
    Left err -> error $ show err
    Right mod -> analyzeModule mod

analyzeModule mod = concatMapM (\d -> analyzeFunction $ extractGlobal d) $ L.filter isFunctionDef $ moduleDefinitions mod

concatMapM :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f as = liftM L.concat $ sequence $ L.map f as

analyzeFunction :: Global -> IO [NSError]
analyzeFunction (Function _ _ _ _ _ _ (ps,True) _ _ _ _ _) = return []
analyzeFunction (Function _ _ _ _ _ _ (ps,False) _ _ _ _ bbs) = symExeFunc ps bbs

initializeExecutionState :: [Parameter] -> [BasicBlock] -> ExecutionState
initializeExecutionState ps bbs =
  let initMemState = initialMemoryState ps in
  executionState [initMemState] $ basicBlocksToIStream bbs

initialMemoryState :: [Parameter] -> MemoryState
initialMemoryState ps =
  let initSt = initMemState M.empty M.empty in
  L.foldl addParameter initSt ps

addParameter :: MemoryState -> Parameter -> MemoryState
addParameter ms (Parameter t n _) =
  let typeT = llvmTypeToTypeT t in
  addNamedSymbol typeT n ms

symExeFunc :: [Parameter] -> [BasicBlock] -> IO [NSError]
symExeFunc ps bbs =
  let startingExec = initializeExecutionState ps bbs in
  symExe startingExec

symExe execState =
  case liveStates execState of
    [] -> return $ L.map extractError $ L.filter isError $ memstates execState
    states -> pickStateAndExecute execState >>= symExe

pickStateAndExecute :: ExecutionState -> IO ExecutionState
pickStateAndExecute execState =
  let (memStateToExec, execStateWithoutMemStateToExec) = selectTopMemState execState in
  do
    resultStates <- runMemState execStateWithoutMemStateToExec memStateToExec
    return $ addMemStates resultStates execStateWithoutMemStateToExec

runMemState :: ExecutionState -> MemoryState -> IO [MemoryState]
runMemState es ms =
  let i = nextInstruction es ms in
  executeInstruction i es ms

executeInstruction :: Instr -> ExecutionState -> MemoryState -> IO [MemoryState]
executeInstruction i es ms =
  case opcode i of
    RET -> return [setCompleted ms]
    RETVAL -> return [setCompleted ms]
    LABEL -> return [incrementIP ms]
    ALLOCA -> return $ execAlloca i ms
    STORE -> return $ execStore i ms
    LOAD -> return $ execLoad i ms
    SDIV -> execSDiv i ms
    ADD -> return $ execAdd i ms
    SUB -> return $ execSub i ms
    other -> error $ "executeInstruction does not support " ++ show other

execAlloca i ms =
  let x = receivingOp i
      newMS = addOpSymbol (InstructionSet.allocatedType i) x ms in
  [incrementIP $ newMS]

execStore i ms =
  let a = storeValue i
      b = storeLoc i in
  [incrementIP $ setConstraint (\c -> con [c, eq (deref b ms) (valueSym a ms)]) ms]

execLoad :: Instr -> MemoryState -> [MemoryState]
execLoad i ms =
  let a = receivingOp i
      b = loadLoc i
      newMS = addOpSymbol (opType a) a ms in
  [incrementIP $ setConstraint (\c -> con [c, eq (valueSym a newMS) (deref b newMS)]) newMS]

execAdd :: Instr -> MemoryState -> [MemoryState]
execAdd i ms =
  let a = lhs i
      b = rhs i
      res = receivingOp i
      ms1 = addOpSymbol (opType res) res ms
      newMS = incrementIP $ setConstraint (\c -> con [c, eq (valueSym res ms1) (isum (valueSym a ms1) (valueSym b ms1))]) ms1 in
  [newMS]

execSub :: Instr -> MemoryState -> [MemoryState]
execSub i ms =
  let a = lhs i
      b = rhs i
      res = receivingOp i
      ms1 = addOpSymbol (opType res) res ms
      newMS = incrementIP $ setConstraint (\c -> con [c, eq (valueSym res ms1) (iminus (valueSym a ms1) (valueSym b ms1))]) ms1 in
  [newMS]

execSDiv :: Instr -> MemoryState -> IO [MemoryState]
execSDiv i ms =
  let a = lhs i
      b = rhs i
      res = receivingOp i
      bVal = valueSym b ms
      bType = symbolType bVal
      bWidth = intWidth bType
      ms1 = addOpSymbol (opType res) res ms
      errMS = setConstraint (\c -> con [c, eq (valueSym b ms) (intConstant bWidth 0)]) ms1
      safeMS = incrementIP $ setConstraint (\c -> con [c, eq (valueSym res ms1) (signDivide (valueSym a ms1) (valueSym b ms1))]) ms1 in
  do
    stateIsSat <- isSatisfiable errMS
    case stateIsSat of
      True -> return [safeMS, setError divZeroError errMS]
      False -> return [safeMS]
