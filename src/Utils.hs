module Utils(cSourceToBitcode,
             printModule,
             parseModule,
             prettyMap,
             parseBitcode,
             concatMapM) where

import Control.Monad.Except
import Data.ByteString as BS
import Data.List as L
import Data.Map as M
import LLVM.General
import LLVM.General.Analysis
import LLVM.General.AST
import LLVM.General.Context
import System.Process

concatMapM :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f as = liftM L.concat $ sequence $ L.map f as

printModule :: FilePath -> IO ()
printModule src = do
  a <- withContext $ \context -> do
    runExceptT $ withModuleFromBitcode context (File src) $ \mod -> do
      ast <- moduleAST mod
      return ast
  Prelude.putStrLn $ show a
  return ()

readModule :: File -> IO (Either String LLVM.General.AST.Module)
readModule src = do
  a <- withContext $ \context -> do
    runExceptT $ withModuleFromBitcode context src $ \mod -> do
      ast <- moduleAST mod
      return ast
  return a

parseModule :: FilePath -> IO (Either String LLVM.General.AST.Module)
parseModule fileName = do
  cSourceToBitcode $ fileName
  readModule $ File $ fileName ++ ".bc"

parseBitcode :: FilePath -> IO (Either String LLVM.General.AST.Module)
parseBitcode filePath = do
  readModule $ File filePath

cSourceToBitcode :: FilePath -> IO ()
cSourceToBitcode path =
  runCommandStrict $ "/Users/dillon/Downloads/clang+llvm-3.4.2-x86_64-apple-darwin10.9/bin/clang -emit-llvm -c " ++ (path ++ ".c") ++ " -o " ++ (path ++ ".bc")

runCommandStrict str = do
  Prelude.putStrLn $ str
  cmdHandle <- runCommand str
  waitForProcess cmdHandle
  return ()

myShowList :: (Show a) => [a] -> String
myShowList as = L.concat $ L.intersperse "\n" $ L.map show as

prettyMap :: (Show a, Show b) => Map a b -> String
prettyMap m = L.concatMap prettyPair $ M.toList m

prettyPair :: (Show a, Show b) => (a, b) -> String
prettyPair (x, y) = show x ++ "\t->\t" ++ show y ++ "\n"
