module Analysis(moduleErrors) where

import Control.Monad.State.Lazy
import Data.List as L
import Data.Map as M
import LLVM.General
import LLVM.General.Analysis
import LLVM.General.AST
import LLVM.General.Context

import Error
import InstructionSet
import LLVMUtils
import TypeSystem
import Utils

valueFromName ((:=) _ v) = v
valueFromName (Do v) = v

extractGlobal (GlobalDefinition d) = d

isFunctionDef (GlobalDefinition (Function _ _ _ _ _ _ _ _ _ _ _ _)) = True
isFunctionDef _ = False

addNamedSymbol :: TypeT -> Name -> MemoryState -> State Int MemoryState
addNamedSymbol t n ms = do
  (s, ms) <- newSymbol t ms
  return $ addNameSymbol (ref (nameToString n) t) s ms

addOpSymbol :: TypeT -> Op -> MemoryState -> State Int MemoryState
addOpSymbol t op ms = do
  (s, ms) <- newSymbol t ms
  return $ addNameSymbol op s ms

freshSymbol t = do
  i <- get
  put $ i + 1
  return $ symbol t i

newSymbol t ms =
  case isAddress t of
    True -> error $ "newSymbol does not support " ++ show t ++ " yet"
    False -> do
      val <- freshSymbol t
      addr <- freshSymbol $ TypeSystem.address t
      return $ (addr, addValue addr val ms)

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

data MemoryState
  = MemoryState {
    nameMap :: Map Op Term,
    addrMap :: Map Term Term,
    memConstraint :: Constraint,
    ip :: Int,
    status :: MemStatus
    } deriving (Eq, Show)

initMemState nm am = MemoryState nm am T 1 Live

isLive (MemoryState _ _ _ _ Live) = True
isLive _ = False

isError (MemoryState _ _ _ _ (Error _)) = True
isError _ = False

extractError (MemoryState _ _ _ _ (Error e)) = e

incrementIP (MemoryState nm am c ip s) =
  MemoryState nm am c (ip+1) s

setCompleted (MemoryState nm am c ip _) =
  MemoryState nm am c ip Completed

addNameSymbol n s (MemoryState nm am c ip st) =
  MemoryState (M.insert n s nm) am c ip st

addValue addr val (MemoryState nm am c ip st) =
  MemoryState nm (M.insert addr val am) c ip st

instance Ord MemoryState where
  (<=) m1 m2 = memConstraint m1 <= memConstraint m2

data MemStatus
  = Live
  | Completed
  | Error NSError
    deriving (Eq, Ord, Show)

data Constraint
  = Dis [Constraint]
  | Con [Constraint]
  | Not Constraint
  | Predicate String Int [Term]
  | T
  | F
    deriving (Eq, Ord, Show)

dis = Dis
con = Con
false = F
true = T

data Term
  = Func String Int [Term]
  | IntConstant Int Integer
  | Symbol TypeT Int
    deriving (Eq, Ord, Show)

symbol = Symbol

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
    STORE -> execStore i ms
    other -> error $ "executeInstruction does not support " ++ show other

execAlloca i ms =
  let x = receivingOp i in
  do
    newMS <- addOpSymbol (InstructionSet.allocatedType i) x ms
    return [incrementIP $ newMS]

execStore i ms = error "execStore not implemented"

nextInstruction :: ExecutionState -> MemoryState -> Instr
nextInstruction es ms =
  instructionAt (ip ms) es
