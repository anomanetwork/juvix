module Juvix.Compiler.Asm.Interpreter.RuntimeState
  ( module Juvix.Compiler.Asm.Interpreter.RuntimeState,
    module Juvix.Compiler.Asm.Interpreter.Base,
  )
where

import Juvix.Compiler.Asm.Data.Module
import Juvix.Compiler.Asm.Data.Stack (Stack)
import Juvix.Compiler.Asm.Data.Stack qualified as Stack
import Juvix.Compiler.Asm.Interpreter.Base

{-
Memory consists of:
\* global heap
  - holds constructor data
  - shared between functions
  - referenced with ConstrRef
  - unbounded (theoretically)
\* global call stack
  - holds return addresses for recursive function invocations
  - manipulated by the Call and Return instructions (accessible only
    implicitly through these)
  - unbounded (theoretically)
\* local argument area
  - holds function arguments
  - local to one function invocation (activation frame)
  - referenced with ArgRef
  - constant number of arguments (depends on the function)
\* local temporary stack
  - holds temporary values
  - referenced with TempRef
  - constant maximum height (depends on the function)
  - current height of the local temporary stack is known at compile-time for each
    instruction; a program violating this assumption (e.g. by having a `Branch`
    instruction whose two branches result in different stack heights) is
    erroneous
  - compiled to a constant number of local variables / registers
  - Core.Let is compiled to store the value in the local temporary area
  - Core.Case is compiled to store the value in the local temporary area,
    to enable accessing constructor arguments in the branches
\* local value stack
  - holds temporary values
  - JuvixAsm instructions manipulate the current local value stack
  - one value stack per each function invocation, not shared among functions
    (or different invocations of the same function)
  - maximum constant height of a local value stack (depends on the function)
  - current height of the local value stack is known at compile-time for each
    instruction; a program violating this assumption is erroneous
  - compiled to a constant number of local variables / registers (unless the
    target IR itself is a stack machine)
-}

-- The heap does not need to be modelled explicitly. Heap values are simply
-- stored in the `Val` datastructure. Pointers are implicit.

newtype AsmCallStack = AsmCallStack
  { _callStack :: [Continuation]
  }

data ArgumentArea = ArgumentArea
  { _argumentArea :: HashMap Offset Val,
    _argumentAreaSize :: Int
  }

newtype TemporaryStack = TemporaryStack {_temporaryStack :: Stack Val}

newtype ValueStack = ValueStack
  { _valueStack :: [Val]
  }

-- | An activation frame contains the function-local memory (local argument area,
-- temporary stack, value stack) for a single function invocation.
data Frame = Frame
  { _frameArgs :: ArgumentArea,
    _frameTemp :: TemporaryStack,
    _frameStack :: ValueStack,
    _frameFunction :: Maybe Symbol,
    _frameCallLocation :: Maybe Location
  }

emptyFrame :: Frame
emptyFrame =
  Frame
    { _frameArgs = ArgumentArea mempty 0,
      _frameTemp = TemporaryStack Stack.empty,
      _frameStack = ValueStack [],
      _frameFunction = Nothing,
      _frameCallLocation = Nothing
    }

data Continuation = Continuation
  { _contFrame :: Frame,
    _contCode :: Code
  }

-- | JuvixAsm runtime state
data RuntimeState = RuntimeState
  { -- | global call stack
    _runtimeCallStack :: AsmCallStack,
    -- | current frame
    _runtimeFrame :: Frame,
    -- | debug messages generated so far
    _runtimeMessages :: [Text],
    -- | current location in the source
    _runtimeLocation :: Maybe Location,
    -- | Module associated with the runtime state
    _runtimeModule :: Module
  }

makeLenses ''AsmCallStack
makeLenses ''Continuation
makeLenses ''ArgumentArea
makeLenses ''TemporaryStack
makeLenses ''ValueStack
makeLenses ''Frame
makeLenses ''RuntimeState
