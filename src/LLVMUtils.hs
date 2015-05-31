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
import Utils

llvmTypeToTypeT :: Type -> Either String TypeT
llvmTypeToTypeT (IntegerType w) = Right $ integer w
llvmTypeToTypeT (PointerType t _) = do
  pt <- llvmTypeToTypeT t
  return $ TypeSystem.address pt
llvmTypeToTypeT other = Left $ "llvmTypeToTypeT does not support " ++ show other

basicBlockToInstrList (BasicBlock n instrs term) = do
  t <- namedLLVMTerminatorToInstr term
  is <- mapM namedLLVMInstructionToInstr instrs
  return $ ((label $ nameToString n) : is) ++ [t]

namedLLVMInstructionToInstr ((:=) n i) = llvmInstructionToInstr (nameToString n) i
namedLLVMInstructionToInstr (Do (Store _ addr val _ _ _)) = do
  ad <- llvmOperandToOp addr
  val <- llvmOperandToOp val
  return $ store ad val
namedLLVMInstructionToInstr other = Left $ "namedLLVMInstructionToInstr does not yet support " ++ show other

llvmInstructionToInstr n (Alloca t Nothing _ _) = do
  tp <- llvmTypeToTypeT t
  return $ alloca (ref n $ TypeSystem.address tp) (TypeSystem.address tp)
llvmInstructionToInstr n (AST.SDiv _ a b _) = llvmArithBinopToInstr sdiv n a b
llvmInstructionToInstr n (AST.Add _ _ a b _) = llvmArithBinopToInstr add n a b
llvmInstructionToInstr n (AST.Sub _ _ a b _) = llvmArithBinopToInstr sub n a b
llvmInstructionToInstr n (AST.Mul _ _ a b _) = llvmArithBinopToInstr mul n a b
llvmInstructionToInstr n (Load _ a _ _ _) = do
  aOp <- llvmOperandToOp a
  return $ load (ref n $ typePointedTo (opType aOp)) aOp
llvmInstructionToInstr n (AST.ICmp pred a b _) = do
  aOp <- llvmOperandToOp a
  bOp <- llvmOperandToOp b
  return $ icmp (ref n $ integer 1) (llvmIPredicateToIPred pred) aOp bOp
llvmInstructionToInstr n i = Left $ "llvmInstructionToInstr does not yet support " ++ show i

llvmArithBinopToInstr binop n a b = do
  aOp <- llvmOperandToOp a
  bOp <- llvmOperandToOp b
  return $ binop (ref n $ opType aOp) aOp bOp

namedLLVMTerminatorToInstr (Do t) = llvmTerminatorToInstruction t
namedLLVMTerminatorToInstr other = Left $ "namedLLVMTerminatorToInstr does not yet support " ++ show other

llvmTerminatorToInstruction (Ret (Just val) _) = do
  rVal <- llvmOperandToOp val
  return $ retVal rVal
llvmTerminatorToInstruction (CondBr x td fd _) = do
  xOp <- llvmOperandToOp x
  return $ condbr xOp (label $ nameToString td) (label $ nameToString fd)
llvmTerminatorToInstruction (Br d _) = Right $ br (label $ nameToString d)
llvmTerminatorToInstruction other = Left $ "llvmTerminatorToInstruction does not support " ++ show other

llvmOperandToOp (LocalReference t n) = do
  tp <- llvmTypeToTypeT t
  return $ ref (nameToString n) tp
llvmOperandToOp (ConstantOperand (Int width val)) = Right $ constant $ intConst width val
llvmOperandToOp other = Left $ "Error in llvmOperandToOp: Cannot convert " ++ show other

nameToString (Name n) = n
nameToString (UnName w) = "$UN-" ++ show w

basicBlocksToIStream :: [BasicBlock] -> Either String InstructionStream
basicBlocksToIStream bbs = do
  instrList <- concatMapM basicBlockToInstrList bbs
  let numberedInstrs = L.zip [1..(length instrList)] instrList in
    do
      return $ instructionStream $ M.fromList numberedInstrs

llvmIPredicateToIPred IP.EQ = ieq
llvmIPredicateToIPred IP.NE = ineq
llvmIPredicateToIPred IP.SGT = isgt
llvmIPredicateToIPred IP.SLT = islt
