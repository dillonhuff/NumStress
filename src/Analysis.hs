module Analysis(moduleErrors,
                moduleErrorsWithTimeLimit,
                analyzeModuleWithTimeLimit) where

import Control.Monad.State.Lazy
import Data.Either
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

moduleErrors str = do
  res <- moduleAnalysis str
  return $ L.concat $ rights res

moduleErrorsWithTimeLimit t str = do
  res <- moduleAnalysisWithTimeLimit t str
  return $ L.concat $ rights res

moduleAnalysisWithTimeLimit :: Int64 -> String -> IO [Either String [NSError]]
moduleAnalysisWithTimeLimit timeLimit str = do
  parseRes <- parseModule str
  case parseRes of
    Left err -> return $ [Left err]
    Right mod -> analyzeModuleWithTimeLimit timeLimit mod

moduleAnalysis :: String -> IO [Either String [NSError]]
moduleAnalysis str = do
  parseRes <- parseModule str
  case parseRes of
    Left err -> return $ [Left err]
    Right mod -> analyzeModule mod

analyzeModule mod = mapM (\d -> analyzeFunction $ extractGlobal d) $ L.filter isFunctionDef $ moduleDefinitions mod

analyzeModuleWithTimeLimit t mod =
  mapM (\d -> analyzeFunctionWithTimeLimit t $ extractGlobal d) $ L.filter isFunctionDef $ moduleDefinitions mod

analyzeFunction :: Global -> IO (Either String [NSError])
analyzeFunction (Function _ _ _ _ _ _ (ps,True) _ _ _ _ _) = return $ Left "Cannot analyze vararg function"
analyzeFunction (Function _ _ _ _ _ _ (ps,False) _ _ _ _ bbs) = symExeFunc ps bbs

analyzeFunctionWithTimeLimit :: Int64 -> Global -> IO (Either String [NSError])
analyzeFunctionWithTimeLimit t (Function _ _ _ _ _ _ (ps,True) _ _ _ _ _) = return $ Left "Cannot analyze vararg function"
analyzeFunctionWithTimeLimit t (Function _ _ _ _ _ _ (ps,False) _ _ _ _ bbs) = symExeFuncWithTimeLimit t ps bbs

initializeExecutionState :: [Parameter] -> [BasicBlock] -> Either String ExecutionState
initializeExecutionState ps bbs = do
  initMemState <- initialMemoryState ps
  is <- basicBlocksToIStream bbs
  return $ executionState [initMemState] is

initialMemoryState :: [Parameter] -> Either String MemoryState
initialMemoryState ps =
  let initSt = initMemState M.empty M.empty in
  foldM addParameter initSt ps

addParameter :: MemoryState -> Parameter -> Either String MemoryState
addParameter ms (Parameter t n _) =
  case llvmTypeToTypeT t of
    Left err -> Left err
    Right typeT -> Right $ addNamedSymbol typeT n ms

symExeFunc :: [Parameter] -> [BasicBlock] -> IO (Either String [NSError])
symExeFunc ps bbs =
  case initializeExecutionState ps bbs of
    Left err -> return $ Left err
    Right startingExec -> do
      errs <- symExe startingExec
      return $ Right errs

symExeFuncWithTimeLimit :: Int64 -> [Parameter] -> [BasicBlock] -> IO (Either String [NSError])
symExeFuncWithTimeLimit t ps bbs =
  case initializeExecutionState ps bbs of
    Left err -> return $ Left err
    Right startingExec -> do
      errs <- symExeWithTimeLimit t startingExec
      return $ Right errs


