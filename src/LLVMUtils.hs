module LLVMUtils(llvmTypeToTypeT,
                 basicBlocksToIStream,
                 nameToString) where

import Data.List as L
import Data.Map as M
import LLVM.General.AST as AST
import LLVM.General.AST.IntegerPredicate as IP
import LLVM.General.AST.Constant

import InstructionSet
import InstructionStream
import TypeSystem

llvmTypeToTypeT :: Type -> TypeT
llvmTypeToTypeT (IntegerType w) = integer w
llvmTypeToTypeT (PointerType t _) = TypeSystem.address $ llvmTypeToTypeT t

basicBlockToInstrList (BasicBlock n instrs term) =
  ((label $ nameToString n) : (L.map namedLLVMInstructionToInstr instrs)) ++ [namedLLVMTerminatorToInstr term]

namedLLVMInstructionToInstr ((:=) n i) = llvmInstructionToInstr (nameToString n) i
namedLLVMInstructionToInstr (Do (Store _ addr val _ _ _)) = store (llvmOperandToOp addr) (llvmOperandToOp val)

llvmInstructionToInstr n (Alloca t Nothing _ _) = alloca (ref n tp) tp
  where
    tp = TypeSystem.address $ llvmTypeToTypeT t
llvmInstructionToInstr n (Load _ a _ _ _) = load (ref n $ typePointedTo tp) aOp
  where
    aOp = llvmOperandToOp a
    tp = opType aOp
llvmInstructionToInstr n (AST.SDiv _ a b _) = sdiv (ref n tp) aOp bOp
  where
    aOp = llvmOperandToOp a
    bOp = llvmOperandToOp b
    tp = opType aOp
llvmInstructionToInstr n (AST.Add _ _ a b _) = add (ref n tp) aOp bOp
  where
    aOp = llvmOperandToOp a
    bOp = llvmOperandToOp b
    tp = opType aOp
llvmInstructionToInstr n (AST.Sub _ _ a b _) = sub (ref n tp) aOp bOp
  where
    aOp = llvmOperandToOp a
    bOp = llvmOperandToOp b
    tp = opType aOp
llvmInstructionToInstr n (AST.Mul _ _ a b _) = mul (ref n tp) aOp bOp
  where
    aOp = llvmOperandToOp a
    bOp = llvmOperandToOp b
    tp = opType aOp
llvmInstructionToInstr n (AST.ICmp pred a b _) = icmp (ref n $ integer 1) (llvmIPredicateToIPred pred) aOp bOp
  where
    aOp = llvmOperandToOp a
    bOp = llvmOperandToOp b
llvmInstructionToInstr n i = error $ "llvmInstructionToInstr does not yet support " ++ show i

namedLLVMTerminatorToInstr (Do t) = llvmTerminatorToInstruction t

llvmTerminatorToInstruction (Ret (Just val) _) = retVal $ llvmOperandToOp val
llvmTerminatorToInstruction (CondBr x td fd _) = condbr (llvmOperandToOp x) (label $ nameToString td) (label $ nameToString fd)
llvmTerminatorToInstruction (Br d _) = br (label $ nameToString d)
llvmTerminatorToInstruction other = error $ "llvmTerminatorToInstruction does not support " ++ show other

llvmOperandToOp (LocalReference t n) = ref (nameToString n) (llvmTypeToTypeT t)
llvmOperandToOp (ConstantOperand (Int width val)) = constant $ intConst width val
llvmOperandToOp other = error $ "Error in llvmOperandToOp: Cannot convert " ++ show other

nameToString (Name n) = n
nameToString (UnName w) = "$UN-" ++ show w

basicBlocksToIStream :: [BasicBlock] -> InstructionStream
basicBlocksToIStream bbs =
  let instrList = L.concatMap basicBlockToInstrList bbs
      numberedInstrs = L.zip [1..(length instrList)] instrList in
  instructionStream $ M.fromList numberedInstrs

llvmIPredicateToIPred IP.EQ = ieq
llvmIPredicateToIPred IP.NE = ineq
llvmIPredicateToIPred IP.SGT = isgt
llvmIPredicateToIPred IP.SLT = islt

