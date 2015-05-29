module MemoryState(MemoryState,
                   nameMap, addrMap, memConstraint, ip, status,
                   incrementIP,
                   isLive, isError, extractError,
                   setCompleted, initMemState,
                   addNamedSymbol, addOpSymbol,
                   addrSym, valueSym, valueAtAddr, deref,
                   setConstraint) where

import LLVM.General.AST

import Control.Monad.State
import Data.Map as M

import Constraint
import Error
import InstructionSet
import LLVMUtils
import Term
import TypeSystem

data MemoryState
  = MemoryState {
    nameMap :: Map Op Term,
    addrMap :: Map Term Term,
    memConstraint :: Constraint,
    ip :: Int,
    status :: MemStatus
    } deriving (Eq, Show)

initMemState nm am = MemoryState nm am true 1 Live

setConstraint f (MemoryState nm am c ip s) =
  MemoryState nm am (f c) ip s

addrSym n ms =
  case M.lookup n $ nameMap ms of
    Just s -> s
    Nothing -> error $ "addrSym failed for " ++ show n ++ " with map " ++ show ms

valueAtAddr s ms =
  case M.lookup s $ addrMap ms of
    Just v -> v
    Nothing -> error $ "valueAtAddr failed for " ++ show s ++ " with map " ++ show ms

valueSym a ms = valueAtAddr (addrSym a ms) ms

deref a ms = valueAtAddr (valueSym a ms) ms
  
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
    True -> do
      (addrOfVal, newMS) <- newSymbol (typePointedTo t) ms
      addr <- freshSymbol $ TypeSystem.address t
      return $ (addr, addValue addr addrOfVal newMS)
    False -> do
      val <- freshSymbol t
      addr <- freshSymbol $ TypeSystem.address t
      return $ (addr, addValue addr val ms)
