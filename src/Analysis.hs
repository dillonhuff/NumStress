module Analysis(analyzableFunctions,
                moduleErrors) where

import Control.Monad.State.Lazy
import Data.List as L
import Data.Map as M
import LLVM.General
import LLVM.General.Analysis
import LLVM.General.AST
import LLVM.General.Context

import Error
import TypeSystem
import Utils

analyzableFunctions defs = L.filter canAnalyze $ definedFunctions defs

definedFunctions :: [Definition] -> [Global]
definedFunctions defs = L.map extractGlobal $ L.filter isFunctionDef defs

isFunctionDef (GlobalDefinition (Function _ _ _ _ _ _ _ _ _ _ _ _)) = True
isFunctionDef _ = False

extractGlobal (GlobalDefinition f@(Function _ _ _ _ _ _ _ _ _ _ _ _)) = f

functionHasBody (Function _ _ _ _ _ _ _ _ _ _ _ []) = False
functionHasBody (Function _ _ _ _ _ _ _ _ _ _ _ _) = True

canAnalyze :: Global -> Bool
canAnalyze (Function _ _ _ _ retType _ params _ _ _ _ bbs) =
  canAnalyzeType retType && canAnalyzeParams params && canAnalyzeBasicBlocks bbs

canAnalyzeType :: Type -> Bool
canAnalyzeType VoidType = True
canAnalyzeType (IntegerType _) = True
canAnalyzeType (PointerType t _) = canAnalyzeType t
canAnalyzeType _ = False

canAnalyzeParams :: ([Parameter], Bool) -> Bool
canAnalyzeParams (_, True) = False
canAnalyzeParams (ps, _)= canAnalyzeParamList ps

canAnalyzeParamList [] = True
canAnalyzeParamList (p:rest) = canAnalyzeParam p && canAnalyzeParamList rest

canAnalyzeParam (Parameter t _ _) = canAnalyzeType t

canAnalyzeBasicBlocks :: [BasicBlock] -> Bool
canAnalyzeBasicBlocks [] = False
canAnalyzeBasicBlocks bbs = L.and $ L.map canAnalyzeBasicBlock bbs

canAnalyzeBasicBlock :: BasicBlock -> Bool
canAnalyzeBasicBlock (BasicBlock _ instrsWithNames terminatorWithName) =
  (L.and $ L.map (\i -> canAnalyzeInstruction $ valueFromName i) instrsWithNames) &&
  (canAnalyzeTerminator $ valueFromName terminatorWithName)

valueFromName ((:=) _ v) = v
valueFromName (Do v) = v

canAnalyzeInstruction (Add _ _ _ _ _) = True
canAnalyzeInstruction (Mul _ _ _ _ _) = True
canAnalyzeInstruction (Sub _ _ _ _ _) = True
canAnalyzeInstruction (SDiv _ _ _ _) = True
canAnalyzeInstruction (Alloca _ _ _ _) = True
canAnalyzeInstruction (Store _ _ _ _ _ _) = True
canAnalyzeInstruction (Load _ _ _ _ _) = True
canAnalyzeInstruction i = False

canAnalyzeTerminator (Ret _ _) = True
canAnalyzeTerminator t = False

addParam :: MemoryState -> Parameter -> State Int MemoryState
addParam ms (Parameter t n _) = addNamedSymbol t n ms

addNamedSymbol :: Type -> Name -> MemoryState -> State Int MemoryState
addNamedSymbol t n ms = error "addNamedSymbol not implemented"

data ExecutionState
  = ExecutionState {
    memstates :: [MemoryState],
    iStream :: InstructionStream
    } deriving (Eq, Ord, Show)

executionState = ExecutionState

liveStates e = []
errorsDetected e = []

data MemoryState
  = MemoryState {
    nameMap :: Map Name Term,
    addrMap :: Map Term Term,
    memConstraint :: Constraint,
    ip :: Int
    } deriving (Eq, Show)

memoryState = MemoryState

instance Ord MemoryState where
  (<=) m1 m2 = memConstraint m1 <= memConstraint m2

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
  instrStream <- basicBlocksToIStream bbs
  return $ executionState [initMemState] instrStream

initialMemoryState :: [Parameter] -> State Int MemoryState
initialMemoryState ps = return $ memoryState M.empty M.empty T 0

basicBlocksToIStream :: [BasicBlock] -> State Int InstructionStream
basicBlocksToIStream bbs = return instructionStream

data InstructionStream
  = InstructionStream
    deriving (Eq, Ord, Show)

instructionStream = InstructionStream

symExe :: ExecutionState -> State Int [NSError]
symExe execState =
  case liveStates execState of
    [] -> return $ errorsDetected execState
    states -> pickStateAndExecute execState >>= symExe

pickStateAndExecute :: ExecutionState -> State Int ExecutionState
pickStateAndExecute execState = return execState
