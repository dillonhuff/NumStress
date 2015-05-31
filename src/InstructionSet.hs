module InstructionSet(Instr,
                      opcode,
                      receivingOp, allocatedType,
                      Opcode(..),
                      add, sub, mul, sdiv, label,
                      alloca, store, load, icmp,
                      retVal, ret, condbr, br,
                      storeLoc, storeValue, loadLoc,
                      lhs, rhs,
                      Op, ref, constant, opType,
                      Constant,
                      isConstant,
                      intConst, constantToTerm, constantValue,
                      ieq, ineq, icmpPred, ipredConstraintFunc,
                      dest, trueDest, falseDest, conditionVariable) where

import Data.Word

import Constraint
import Term
import TypeSystem

data Instr
  = Add Op Op Op
  | Sub Op Op Op
  | Mul Op Op Op
  | SDiv Op Op Op
  | Alloca Op TypeT
  | Load Op Op
  | Store Op Op
  | Label String
  | ICmp Op IPred Op Op
  | RetVal Op
  | Ret
  | CondBr Op Instr Instr
  | Br Instr
    deriving (Eq, Ord, Show)

add = Add
sub = Sub
mul = Mul
sdiv = SDiv
alloca = Alloca
store = Store
load = Load
label = Label
icmp = ICmp
retVal = RetVal
ret = Ret
condbr = CondBr
br = Br

receivingOp (ICmp x _ _ _) = x
receivingOp (Alloca x _) = x
receivingOp (Add x _ _) = x
receivingOp (Sub x _ _) = x
receivingOp (Mul x _ _) = x
receivingOp (SDiv x _ _) = x
receivingOp (Load x _) = x
receivingOp other = error $ "receivingOp does not support " ++ show other

lhs (ICmp _ _ l _) = l
lhs (Add _ l _) = l
lhs (Sub _ l _) = l
lhs (Mul _ l _) = l
lhs (SDiv _ l _) = l

rhs (ICmp _ _ _ r) = r
rhs (Add _ _ r) = r
rhs (Sub _ _ r) = r
rhs (Mul _ _ r) = r
rhs (SDiv _ _ r) = r

allocatedType (Alloca _ t) = t

storeLoc (Store l _) = l
storeValue (Store _ v) = v

loadLoc (Load _ b) = b

dest (Br d) = d
trueDest (CondBr _ td _) = td
falseDest (CondBr _ _ fd) = fd

conditionVariable (CondBr v _ _) = v

data Opcode
  = RET
  | RETVAL
  | LABEL
  | ADD
  | SUB
  | MUL
  | SDIV
  | ALLOCA
  | STORE
  | LOAD
  | ICMP
  | CONDBR
  | BR
    deriving (Eq, Ord, Show)

opcode (Br _) = BR
opcode (CondBr _ _ _) = CONDBR
opcode (ICmp _ _ _ _) = ICMP
opcode (Add _ _ _) = ADD
opcode (Sub _ _ _) = SUB
opcode (Mul _ _ _) = MUL
opcode (SDiv _ _ _) = SDIV
opcode (Alloca _ _) = ALLOCA
opcode (Load _ _) = LOAD
opcode (Store _ _) = STORE
opcode (Label _) = LABEL
opcode (RetVal _) = RETVAL
opcode Ret = RET

data Op
  = Ref String TypeT
  | Constant Constant
    deriving (Eq, Ord, Show)

ref = Ref
constant = Constant

opType (Ref _ t) = t
opType (Constant c) = constantType c

isRef (Ref _ _) = True
isRef _ = False

isConstant (Constant _) = True
isConstant _ = False

constantValue (Constant c) = c

icmpPred (ICmp _ p _ _) = p

data Constant
  = IntConst Word32 Integer
    deriving (Eq, Ord, Show)

intConst width val = IntConst width val

constantToTerm (IntConst w i) = intConstant w i

constantType (IntConst w _) = integer w

data IPred
  = IEQ
  | INEQ
    deriving (Eq, Ord, Show)

ieq = IEQ
ineq = INEQ

ipredConstraintFunc IEQ = \a b -> eq a b
