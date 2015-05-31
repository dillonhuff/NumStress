module MemoryState(MemoryState,
                   nameMap, addrMap, memConstraint, ip, status,
                   incrementIP, setIP,
                   isLive, isError, extractError,
                   setCompleted, initMemState, setError,
                   addNamedSymbol, addOpSymbol, newSymbol,
                   addValue, freshSymbol,
                   addrSym, valueSym, valueAtAddr, deref,
                   setConstraint, isSatisfiable) where

import LLVM.General.AST

import Control.Monad.State
import Data.Map as M

import Constraint
import Error
import InstructionSet
import LLVMUtils
import Term
import TypeSystem
import Utils

data MemoryState
  = MemoryState {
    nameMap :: Map Op Term,
    addrMap :: Map Term Term,
    memConstraint :: Constraint,
    ip :: Int,
    symIndex :: Int,
    status :: MemStatus
    } deriving (Eq)

instance Show MemoryState where
  show ms = showHeader ++
            prettyMap (nameMap ms) ++ "\n\n" ++
            prettyMap (addrMap ms) ++ "\n\n" ++
            show (memConstraint ms) ++ "\n\n" ++
            "Instruction pointer: " ++ show (ip ms) ++ "\n\n" ++
            "Symbol index: " ++ show (symIndex ms) ++ "\n\n" ++
            "Status: " ++ show (status ms) ++ "\n\n" ++
            showFooter

showHeader = "\n*************************** Memory State ***************************\n\n"
showFooter = "\n********************************************************************"

initMemState nm am = MemoryState nm am true 1 0 Live

setConstraint f (MemoryState nm am c ip ind s) =
  MemoryState nm am (f c) ip ind s

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
  
isLive (MemoryState _ _ _ _ _ Live) = True
isLive _ = False

isError (MemoryState _ _ _ _ _ (Error _)) = True
isError _ = False

extractError (MemoryState _ _ _ _ _ (Error e)) = e

incrementIP (MemoryState nm am c ip ind s) =
  MemoryState nm am c (ip+1) ind s

setIP newIP (MemoryState nm am c ip ind s) =
  MemoryState nm am c newIP ind s

incrementSymInd (MemoryState nm am c ip ind s) =
  MemoryState nm am c ip (ind+1) s

setCompleted (MemoryState nm am c ip ind _) =
  MemoryState nm am c ip ind Completed

setError e (MemoryState nm am c ip ind _) =
  MemoryState nm am c ip ind (Error e)

addNameSymbol n s (MemoryState nm am c ip ind st) =
  MemoryState (M.insert n s nm) am c ip ind st

addValue addr val (MemoryState nm am c ip ind st) =
  MemoryState nm (M.insert addr val am) c ip ind st

instance Ord MemoryState where
  (<=) m1 m2 = memConstraint m1 <= memConstraint m2

data MemStatus
  = Live
  | Completed
  | Error NSError
    deriving (Eq, Ord, Show)

addNamedSymbol :: TypeT -> Name -> MemoryState -> MemoryState
addNamedSymbol t n ms =
  addOpSymbol t (ref (nameToString n) t) ms

addOpSymbol :: TypeT -> Op -> MemoryState -> MemoryState
addOpSymbol t op ms =
  let (s, newMS) = newSymbol t ms in
  addNameSymbol op s newMS

freshSymbol t ms =
  let i = symIndex ms in
  (symbol t i, incrementSymInd ms)

newSymbol :: TypeT -> MemoryState -> (Term, MemoryState)
newSymbol t ms =
  case isAddress t of
    True ->
      let (addrOfVal, newMS) = newSymbol (typePointedTo t) ms
          (addr, newMS2) = freshSymbol (TypeSystem.address t) newMS in
      (addr, addValue addr addrOfVal newMS2)
    False ->
      let (val, newMS) = freshSymbol t ms
          (addr, newMS2) = freshSymbol (TypeSystem.address t) newMS in
      (addr, addValue addr val newMS2)

isSatisfiable :: MemoryState -> IO Bool
isSatisfiable ms = isSAT $ memConstraint ms
