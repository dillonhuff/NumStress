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
    memStateSAT <- isSatisfiable memStateToExec
    case memStateSAT of
      True -> do
        resultStates <- runMemState execStateWithoutMemStateToExec memStateToExec
        return $ addMemStates resultStates execStateWithoutMemStateToExec
      False -> do
        return execStateWithoutMemStateToExec

runMemState :: ExecutionState -> MemoryState -> IO [MemoryState]
runMemState es ms =
  let i = nextInstruction es ms in
  executeInstruction i es ms

executeInstruction :: Instr -> ExecutionState -> MemoryState -> IO [MemoryState]
executeInstruction i es ms = do
  case opcode i of
    RET -> return [setCompleted ms]
    RETVAL -> return [setCompleted ms]
    LABEL -> return [incrementIP ms]
    ALLOCA -> return $ execAlloca i ms
    STORE -> return $ execStore i ms
    LOAD -> return $ execLoad i ms
    SDIV -> execSDiv i ms
    ADD -> return $ execSafeArithBinop isum i ms
    SUB -> return $ execSafeArithBinop idiff i ms
    MUL -> return $ execSafeArithBinop iprod i ms
    ICMP -> return $ execICmp i ms 
    CONDBR -> execCondBr i ms $ iStream es
    BR -> return $ execBr i ms $ iStream es

execAlloca i ms =
  let x = receivingOp i
      newMS = addOpSymbol (InstructionSet.allocatedType i) x ms in
  [incrementIP $ newMS]

execStore i ms =
  let a = storeValue i
      b = storeLoc i
      addrToStoreTo = valueSym b ms
      (newBLocVal, newMS1) = freshSymbol (opType a) ms
      newMS2 = addValue addrToStoreTo newBLocVal newMS1 in
  [incrementIP $ setConstraint (\c -> con [c, eq (deref b newMS2) (opValue a newMS2)]) newMS2]

execLoad :: Instr -> MemoryState -> [MemoryState]
execLoad i ms =
  let a = receivingOp i
      b = loadLoc i
      newMS = addOpSymbol (opType a) a ms in
  [incrementIP $ setConstraint (\c -> con [c, eq (opValue a newMS) (deref b newMS)]) newMS]

execSafeArithBinop :: (Term -> Term -> Term) -> Instr -> MemoryState -> [MemoryState]
execSafeArithBinop f i ms =
  let a = lhs i
      b = rhs i
      res = receivingOp i
      ms1 = addOpSymbol (opType res) res ms
      newMS = incrementIP $ setConstraint (\c -> con [c, eq (opValue res ms1) (f (opValue a ms1) (opValue b ms1))]) ms1 in
  [newMS]
  
execICmp :: Instr -> MemoryState -> [MemoryState]
execICmp i ms =
  let res = receivingOp i
      ms1 = addOpSymbol (opType res) res ms
      (l, r) = makeICmpConstraints i ms1
      ms2 = incrementIP ms1
      newStates = [setConstraint (\c -> con [c, l]) ms2, setConstraint (\c -> con [c, r]) ms2] in
  newStates

execBr :: Instr -> MemoryState -> InstructionStream -> [MemoryState]
execBr i ms is =
  let destLabel = InstructionSet.dest i in
  [setIP (labelIndex destLabel is) ms]

execCondBr :: Instr -> MemoryState -> InstructionStream -> IO [MemoryState]
execCondBr i ms is =
  let a = valueSym (conditionVariable i) ms
      c = memConstraint ms
      trueLabel = InstructionSet.trueDest i
      falseLabel = InstructionSet.falseDest i in
  do
    aMightBeFalse <- isSAT $ Constraint.not $ dis [Constraint.not $ memConstraint ms, eq a (intConstant 1 1)]
    case Prelude.not $ aMightBeFalse of
      True -> do
        return [setIP (labelIndex trueLabel is) ms]
      False -> do
        aMightBeTrue <- isSAT $ Constraint.not $ dis [Constraint.not $ memConstraint ms, eq a (intConstant 1 0)]
        case Prelude.not aMightBeTrue of
          True -> return [setIP (labelIndex falseLabel is) ms]
          False -> error "execCondBr: Ambiguous condition variables are not yet supported"
        
execSDiv :: Instr -> MemoryState -> IO [MemoryState]
execSDiv i ms =
  let a = lhs i
      b = rhs i
      res = receivingOp i
      bVal = opValue b ms
      bType = termType bVal
      bWidth = intWidth bType
      ms1 = addOpSymbol (opType res) res ms
      errMS = setConstraint (\c -> con [c, eq (opValue b ms) (intConstant bWidth 0)]) ms1
      safeMS = incrementIP $ setConstraint (\c -> con [c, neq (opValue b ms1) (intConstant bWidth 0), eq (opValue res ms1) (signDivide (opValue a ms1) (opValue b ms1))]) ms1 in
  do
    stateIsSat <- isSatisfiable errMS
    case stateIsSat of
      True -> return [safeMS, setError divZeroError errMS]
      False -> return [safeMS]

opValue :: Op -> MemoryState -> Term
opValue a ms =
  case isConstant a of
    True -> constantToTerm $ constantValue a
    False -> valueSym a ms

makeICmpConstraints :: Instr -> MemoryState -> (Constraint, Constraint)
makeICmpConstraints i ms =
  let a = opValue (lhs i) ms
      b = opValue (rhs i) ms
      c = valueSym (receivingOp i) ms
      f = ipredConstraintFunc $ icmpPred i in
  (con [f a b, eq c (intConstant 1 1)],
   con [Constraint.not $ f a b, eq c (intConstant 1 0)])
