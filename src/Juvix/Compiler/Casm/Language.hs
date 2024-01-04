module Juvix.Compiler.Casm.Language
  ( module Juvix.Compiler.Casm.Language.Base,
    module Juvix.Compiler.Casm.Language,
  )
where

import Juvix.Compiler.Casm.Language.Base

type Offset = Int16

type Address = Int

data Reg
  = Ap
  | Fp

data MemRef = MemRef
  { _memRefReg :: Reg,
    _memRefOff :: Offset
  }

data LabelRef = LabelRef
  { _labelRefSymbol :: Symbol,
    _labelRefName :: Maybe Text
  }

type Immediate = Integer

data Value
  = -- | Immediate constant
    Imm Immediate
  | -- | Indirect memory reference
    Ref MemRef
  | -- | Label reference (translated to immediate in Cairo bytecode)
    Lab LabelRef

data Instruction
  = Assign InstrAssign
  | Binop InstrBinop
  | Load InstrLoad
  | Jump InstrJump
  | JumpIf InstrJumpIf
  | Call InstrCall
  | Return
  | Alloc InstrAlloc
  | Label LabelRef

data InstrAssign = InstrAssign
  { _instrAssignValue :: Value,
    _instrAssignResult :: MemRef,
    _instrAssignIncAp :: Bool
  }

data Opcode
  = FieldAdd
  | FieldSub
  | FieldMul

data InstrBinop = InstrBinop
  { _instrBinopOpcode :: Opcode,
    _instrBinopResult :: MemRef,
    _instrBinopArg1 :: MemRef,
    _instrBinopArg2 :: Value,
    _instrBinopIncAp :: Bool
  }

data InstrLoad = InstrLoad
  { _instrLoadResult :: MemRef,
    _instrLoadSrc :: MemRef,
    _instrLoadOff :: Offset,
    _instrLoadIncAp :: Bool
  }

data InstrJump = InstrJump
  { _instrJumpTarget :: Value,
    _instrJumpIncAp :: Bool
  }

-- | Jump if value is nonzero
data InstrJumpIf = InstrJumpIf
  { _instrJumpIfTarget :: Value,
    _instrJumpIfValue :: Value,
    _instrJumpIfIncAp :: Bool
  }

newtype InstrCall = InstrCall
  { _instrCallTarget :: Value
  }

newtype InstrAlloc = InstrAlloc
  { _instrAllocSize :: Int
  }

type Code = [Instruction]

makeLenses ''MemRef
makeLenses ''LabelRef
makeLenses ''InstrAssign
makeLenses ''InstrBinop
makeLenses ''InstrLoad
makeLenses ''InstrJump
makeLenses ''InstrJumpIf
makeLenses ''InstrCall
makeLenses ''InstrAlloc
