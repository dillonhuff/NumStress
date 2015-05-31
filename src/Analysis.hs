module Analysis(moduleErrors) where

import Control.Monad.State.Lazy
import Data.List as L
import Data.Map as M
import LLVM.General
import LLVM.General.Analysis
import LLVM.General.AST
import LLVM.General.Context

import Error
import ExecutionState
import LLVMUtils
import MemoryState
import SymbolicExecution
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
