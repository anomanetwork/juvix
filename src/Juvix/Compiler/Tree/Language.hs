module Juvix.Compiler.Tree.Language
  ( module Juvix.Compiler.Tree.Language,
    module Juvix.Compiler.Tree.Language.Base,
    module Juvix.Compiler.Tree.Language.Type,
    module Juvix.Compiler.Tree.Language.Builtins,
  )
where

import Juvix.Compiler.Tree.Language.Base
import Juvix.Compiler.Tree.Language.Builtins
import Juvix.Compiler.Tree.Language.Type

-- | Function call type
data CallType
  = CallFun Symbol
  | CallClosure Node
  deriving stock (Generic)

instance Serialize CallType

data Node
  = Binop NodeBinop
  | Unop NodeUnop
  | Cairo NodeCairo
  | Anoma NodeAnoma
  | ByteArray NodeByteArray
  | -- | A constant value.
    Constant NodeConstant
  | -- | A memory reference.
    MemRef NodeMemRef
  | -- | Allocate constructor data. JVT code: 'alloc[<tag>](x1, .., xn)'.
    AllocConstr NodeAllocConstr
  | -- | Allocate a closure. JVT code: 'calloc[<fun>](x1, .., xn)'.
    AllocClosure NodeAllocClosure
  | -- | Extend a closure with more arguments. JVT code: 'cextend(cl, x1, .., xn)'.
    ExtendClosure NodeExtendClosure
  | -- | Call a function given by an immediate constant Symbol or a closure. JVT
    -- code: 'call[<fun>](x1, .., xn)' or 'call(cl, x1, .., xn)'
    Call NodeCall
  | -- | 'CallClosures' is like 'Call' with 'CallClosure' call type, except that
    -- (1) it either calls or extends the closure depending on the number of
    -- supplied arguments vs the number of expected arguments fetched at runtime
    -- from the closure, and (2) if the number of expected arguments is smaller
    -- than the number of supplied arguments, then the result of the call must
    -- be another closure and the process is repeated until we run out of
    -- supplied arguments. JVT code: 'ccall(cl, x1, .., xn)'.
    CallClosures NodeCallClosures
  | -- | Branch based on a boolean value. JVT code: 'br(x) { true: <code>;
    -- false: <code> }'.
    Branch NodeBranch
  | -- | Branch based on the tag of constructor data.
    -- JVT code: 'case[<ind>](x) { <tag>: <code>; ... <tag>: <code>; default: <code> }'
    -- (any branch may be omitted).
    Case NodeCase
  | -- | Execute nested code with temporary stack extended with a given value.
    -- Used to implement Core.Let. JVT codes: 'save(x) {<code>}',
    -- 'save[<name>](x) {<code>}'.
    Save NodeSave
  deriving stock (Generic)

instance Serialize Node

newtype NodeInfo = NodeInfo
  { _nodeInfoLocation :: Maybe Location
  }
  deriving newtype (Semigroup, Monoid)
  deriving stock (Generic)

instance Serialize NodeInfo

data BinaryOpcode
  = PrimBinop BinaryOp
  | -- | Sequence: evaluate and ignore fist argument, return evaluated second
    -- argument. JVT code: 'seq(x1, x2)'.
    OpSeq
  deriving stock (Generic)

instance Serialize BinaryOpcode

data TreeOp
  = TreeBinaryOpcode BinaryOpcode
  | TreeUnaryOpcode UnaryOpcode
  | TreeByteArrayOp ByteArrayOp
  | TreeCairoOp CairoOp
  | TreeAnomaOp AnomaOp
  deriving stock (Generic)

instance Serialize TreeOp

data UnaryOpcode
  = PrimUnop UnaryOp
  | -- | Assert a boolean and return it
    OpAssert
  | -- | Print a debug log of the argument and return it.
    OpTrace
  | -- | Interrupt execution with a runtime error printing the argument.
    OpFail
  deriving stock (Generic)

instance Serialize UnaryOpcode

data NodeBinop = NodeBinop
  { _nodeBinopInfo :: NodeInfo,
    _nodeBinopOpcode :: BinaryOpcode,
    _nodeBinopArg1 :: Node,
    _nodeBinopArg2 :: Node
  }
  deriving stock (Generic)

instance Serialize NodeBinop

data NodeUnop = NodeUnop
  { _nodeUnopInfo :: NodeInfo,
    _nodeUnopOpcode :: UnaryOpcode,
    _nodeUnopArg :: Node
  }
  deriving stock (Generic)

instance Serialize NodeUnop

data NodeByteArray = NodeByteArray
  { _nodeByteArrayInfo :: NodeInfo,
    _nodeByteArrayOpcode :: ByteArrayOp,
    _nodeByteArrayArgs :: [Node]
  }
  deriving stock (Generic)

instance Serialize NodeByteArray

data NodeCairo = NodeCairo
  { _nodeCairoInfo :: NodeInfo,
    _nodeCairoOpcode :: CairoOp,
    _nodeCairoArgs :: [Node]
  }
  deriving stock (Generic)

instance Serialize NodeCairo

data NodeAnoma = NodeAnoma
  { _nodeAnomaInfo :: NodeInfo,
    _nodeAnomaOpcode :: AnomaOp,
    _nodeAnomaArgs :: [Node]
  }
  deriving stock (Generic)

instance Serialize NodeAnoma

data NodeConstant = NodeConstant
  { _nodeConstantInfo :: NodeInfo,
    _nodeConstant :: Constant
  }
  deriving stock (Generic)

instance Serialize NodeConstant

data NodeMemRef = NodeMemRef
  { _nodeMemRefInfo :: NodeInfo,
    _nodeMemRef :: MemRef
  }
  deriving stock (Generic)

instance Serialize NodeMemRef

data NodeAllocConstr = NodeAllocConstr
  { _nodeAllocConstrInfo :: NodeInfo,
    _nodeAllocConstrTag :: Tag,
    _nodeAllocConstrArgs :: [Node]
  }
  deriving stock (Generic)

instance Serialize NodeAllocConstr

data NodeAllocClosure = NodeAllocClosure
  { _nodeAllocClosureInfo :: NodeInfo,
    _nodeAllocClosureFunSymbol :: Symbol,
    _nodeAllocClosureArgs :: [Node]
  }
  deriving stock (Generic)

instance Serialize NodeAllocClosure

data NodeExtendClosure = NodeExtendClosure
  { _nodeExtendClosureInfo :: NodeInfo,
    _nodeExtendClosureFun :: Node,
    _nodeExtendClosureArgs :: NonEmpty Node
  }
  deriving stock (Generic)

instance Serialize NodeExtendClosure

-- | If _nodeCallType is 'CallClosure', then _nodeCallArgs must be non-empty.
data NodeCall = NodeCall
  { _nodeCallInfo :: NodeInfo,
    _nodeCallType :: CallType,
    _nodeCallArgs :: [Node]
  }
  deriving stock (Generic)

instance Serialize NodeCall

data NodeCallClosures = NodeCallClosures
  { _nodeCallClosuresInfo :: NodeInfo,
    _nodeCallClosuresFun :: Node,
    _nodeCallClosuresArgs :: NonEmpty Node
  }
  deriving stock (Generic)

instance Serialize NodeCallClosures

data NodeBranch = NodeBranch
  { _nodeBranchInfo :: NodeInfo,
    _nodeBranchArg :: Node,
    _nodeBranchTrue :: Node,
    _nodeBranchFalse :: Node
  }
  deriving stock (Generic)

instance Serialize NodeBranch

data NodeCase = NodeCase
  { _nodeCaseInfo :: NodeInfo,
    _nodeCaseInductive :: Symbol,
    _nodeCaseArg :: Node,
    _nodeCaseBranches :: [CaseBranch],
    _nodeCaseDefault :: Maybe Node
  }
  deriving stock (Generic)

instance Serialize NodeCase

data CaseBranch = CaseBranch
  { _caseBranchLocation :: Maybe Location,
    _caseBranchTag :: Tag,
    _caseBranchBody :: Node,
    -- | Indicates whether the evaluated case argument should be pushed onto the
    -- temporary stack in this branch.
    _caseBranchSave :: Bool
  }
  deriving stock (Generic)

instance Serialize CaseBranch

data TempVar = TempVar
  { _tempVarName :: Maybe Text,
    _tempVarLocation :: Maybe Location,
    _tempVarType :: Type
  }
  deriving stock (Generic)

instance Serialize TempVar

data NodeSave = NodeSave
  { _nodeSaveInfo :: NodeInfo,
    _nodeSaveTempVar :: TempVar,
    _nodeSaveArg :: Node,
    _nodeSaveBody :: Node
  }
  deriving stock (Generic)

instance Serialize NodeSave

makeLenses ''NodeBinop
makeLenses ''NodeUnop
makeLenses ''NodeConstant
makeLenses ''NodeMemRef
makeLenses ''NodeAllocClosure
makeLenses ''NodeExtendClosure
makeLenses ''NodeCall
makeLenses ''NodeCallClosures
makeLenses ''NodeBranch
makeLenses ''NodeCase
makeLenses ''NodeSave
makeLenses ''TempVar
makeLenses ''CaseBranch
makeLenses ''NodeInfo
