module LLVMUtils(llvmTypeToTypeT,
                 basicBlockToInstrList,
                 nameToString) where

import Data.List as L
import LLVM.General.AST as AST
import LLVM.General.AST.Constant

import InstructionSet
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
llvmInstructionToInstr n (Load _ a _ _ _) = load (ref n tp) aOp
  where
    aOp = llvmOperandToOp a
    tp = opType aOp
llvmInstructionToInstr n (AST.SDiv _ a b _) = sdiv (ref n tp) aOp bOp
  where
    aOp = llvmOperandToOp a
    bOp = llvmOperandToOp b
    tp = opType aOp
llvmInstructionToInstr n i = error $ "llvmInstructionToInstr does not yet support " ++ show i

namedLLVMTerminatorToInstr (Do t) = llvmTerminatorToInstruction t

llvmTerminatorToInstruction (Ret (Just val) _) = retVal $ llvmOperandToOp val

llvmOperandToOp (LocalReference t n) = ref (nameToString n) (llvmTypeToTypeT t)
llvmOperandToOp (ConstantOperand (Int width val)) = constant $ intConst width val
llvmOperandToOp other = error $ "Error in llvmOperandToOp: Cannot convert " ++ show other

nameToString (Name n) = n
nameToString (UnName w) = "$UN-" ++ show w
