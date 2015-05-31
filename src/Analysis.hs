module Analysis(moduleErrors,
                moduleErrorsWithTimeLimit,
                analyzeModuleWithTimeLimit) where

import Control.Monad.State.Lazy
import Data.Int
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

moduleErrorsWithTimeLimit :: Int64 -> String -> IO [NSError]
moduleErrorsWithTimeLimit timeLimit str = do
  parseRes <- parseModule str
  case parseRes of
    Left err -> error $ show err
    Right mod -> analyzeModuleWithTimeLimit timeLimit mod

moduleErrors :: String -> IO [NSError]
moduleErrors str = do
  parseRes <- parseModule str
  case parseRes of
    Left err -> error $ show err
    Right mod -> analyzeModule mod

analyzeModule mod = concatMapM (\d -> analyzeFunction $ extractGlobal d) $ L.filter isFunctionDef $ moduleDefinitions mod

analyzeModuleWithTimeLimit t mod =
  concatMapM (\d -> analyzeFunctionWithTimeLimit t $ extractGlobal d) $ L.filter isFunctionDef $ moduleDefinitions mod

analyzeFunction :: Global -> IO [NSError]
analyzeFunction (Function _ _ _ _ _ _ (ps,True) _ _ _ _ _) = return []
analyzeFunction (Function _ _ _ _ _ _ (ps,False) _ _ _ _ bbs) = symExeFunc ps bbs

analyzeFunctionWithTimeLimit :: Int64 -> Global -> IO [NSError]
analyzeFunctionWithTimeLimit t (Function _ _ _ _ _ _ (ps,True) _ _ _ _ _) = return []
analyzeFunctionWithTimeLimit t (Function _ _ _ _ _ _ (ps,False) _ _ _ _ bbs) = symExeFuncWithTimeLimit t ps bbs

initializeExecutionState :: [Parameter] -> [BasicBlock] -> ExecutionState
initializeExecutionState ps bbs =
  let initMemState = initialMemoryState ps in
  case basicBlocksToIStream bbs of
    Left err -> error err
    Right is -> executionState [initMemState] is

initialMemoryState :: [Parameter] -> MemoryState
initialMemoryState ps =
  let initSt = initMemState M.empty M.empty in
  L.foldl addParameter initSt ps

addParameter :: MemoryState -> Parameter -> MemoryState
addParameter ms (Parameter t n _) =
  case llvmTypeToTypeT t of
    Left err -> error err
    Right typeT -> addNamedSymbol typeT n ms

symExeFunc :: [Parameter] -> [BasicBlock] -> IO [NSError]
symExeFunc ps bbs =
  let startingExec = initializeExecutionState ps bbs in
  symExe startingExec

symExeFuncWithTimeLimit :: Int64 -> [Parameter] -> [BasicBlock] -> IO [NSError]
symExeFuncWithTimeLimit t ps bbs =
  let startingExec = initializeExecutionState ps bbs in
  symExeWithTimeLimit t startingExec
