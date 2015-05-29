module InstructionSet(Instr,
                      opcode,
                      receivingOp, allocatedType,
                      Opcode(..),
                      add, sub, mul, sdiv, label,
                      alloca, store, load,
                      retVal, ret,
                      Op, ref, constant,
                      Constant,
                      intConst) where

import Data.Word

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
  | RetVal Op
  | Ret
    deriving (Eq, Ord, Show)

add = Add
sub = Sub
mul = Mul
sdiv = SDiv
alloca = Alloca
store = Store
load = Load
label = Label
retVal = RetVal
ret = Ret

receivingOp (Alloca x _) = x
receivingOp (Add x _ _) = x
receivingOp (Sub x _ _) = x
receivingOp (Mul x _ _) = x
receivingOp (SDiv x _ _) = x

allocatedType (Alloca _ t) = t

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
    deriving (Eq, Ord, Show)

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

data Constant
  = IntConst Word32 Integer
    deriving (Eq, Ord, Show)

intConst width val = IntConst width val
