module Juvix.Compiler.Tree.Translation.FromAsm where

import Juvix.Compiler.Asm.Data.Module qualified as Asm
import Juvix.Compiler.Asm.Extra.Base qualified as Asm
import Juvix.Compiler.Asm.Language qualified as Asm
import Juvix.Compiler.Tree.Data.Module
import Juvix.Compiler.Tree.Error
import Juvix.Compiler.Tree.Extra.Base
import Juvix.Compiler.Tree.Language
import Juvix.Compiler.Tree.Translation.FromAsm.Translator

newtype TempSize = TempSize
  { _tempSize :: Int
  }

makeLenses ''TempSize

fromAsm :: (Member (Error TreeError) r) => Asm.Module -> Sem r Module
fromAsm md = do
  fns <- mapM (goFunction md) (md ^. moduleInfoTable . Asm.infoFunctions)
  let tab =
        InfoTable
          { _infoMainFunction = md ^. moduleInfoTable . Asm.infoMainFunction,
            _infoFunctions = fns,
            _infoInductives = md ^. moduleInfoTable . Asm.infoInductives,
            _infoConstrs = md ^. moduleInfoTable . Asm.infoConstrs
          }
  return $
    Module
      { _moduleId = md ^. moduleId,
        _moduleInfoTable = tab,
        _moduleImports = md ^. moduleImports,
        _moduleImportsTable = mempty,
        _moduleSHA256 = md ^. moduleSHA256
      }

goFunction :: (Member (Error TreeError) r') => Asm.Module -> Asm.FunctionInfo -> Sem r' FunctionInfo
goFunction md fi = do
  node' <- runReader (TempSize 0) $ goCodeBlock (fi ^. Asm.functionCode)
  return $
    FunctionInfo
      { _functionName = fi ^. Asm.functionName,
        _functionLocation = fi ^. Asm.functionLocation,
        _functionSymbol = fi ^. Asm.functionSymbol,
        _functionArgsNum = fi ^. Asm.functionArgsNum,
        _functionArgNames = fi ^. Asm.functionArgNames,
        _functionType = fi ^. Asm.functionType,
        _functionCode = node',
        _functionExtra = ()
      }
  where
    unsupported :: (Member (Error TreeError) r) => Maybe Location -> Sem r a
    unsupported loc = throw $ TreeError loc "unsupported"

    goCodeBlock :: forall r. (Members '[Error TreeError, Reader TempSize] r) => Asm.Code -> Sem r Node
    goCodeBlock cmds = runTranslator (reverse cmds) go
      where
        go :: Sem (Translator ': r) Node
        go = do
          node <- goCode
          hasMore <- hasNextCommand
          if
              | hasMore -> do
                  cmd <- nextCommand
                  case cmd of
                    Asm.Instr (Asm.CmdInstr _ Asm.Pop) -> do
                      node' <- go
                      return $
                        Binop
                          NodeBinop
                            { _nodeBinopInfo = mempty,
                              _nodeBinopOpcode = OpSeq,
                              _nodeBinopArg1 = node',
                              _nodeBinopArg2 = node
                            }
                    _ ->
                      throw $
                        TreeError
                          { _treeErrorLoc = Asm.getCommandLocation cmd,
                            _treeErrorMsg = "extra instructions"
                          }
              | otherwise ->
                  return node

    goCode :: forall r. (Members '[Error TreeError, Translator, Reader TempSize] r) => Sem r Node
    goCode = do
      cmd <- nextCommand
      case cmd of
        Asm.Instr x -> goInstr x
        Asm.Branch x -> goBranch x
        Asm.Case x -> goCase x
        Asm.Save x -> goSave x
      where
        goInstr :: Asm.CmdInstr -> Sem r Node
        goInstr Asm.CmdInstr {..} = case _cmdInstrInstruction of
          Asm.Binop op -> goBinop (PrimBinop op)
          Asm.Unop op -> goUnop (PrimUnop op)
          Asm.Cairo op -> goCairo op
          Asm.Push (Asm.Constant c) -> return (mkConst c)
          Asm.Push (Asm.Ref r) -> return (mkMemRef r)
          Asm.Pop -> goPop
          Asm.Assert -> goAssert
          Asm.Trace -> goTrace
          Asm.Dump -> unsupported (_cmdInstrInfo ^. Asm.commandInfoLocation)
          Asm.Failure -> goUnop OpFail
          Asm.Prealloc {} -> unsupported (_cmdInstrInfo ^. Asm.commandInfoLocation)
          Asm.AllocConstr tag -> goAllocConstr tag
          Asm.AllocClosure x -> goAllocClosure x
          Asm.ExtendClosure x -> goExtendClosure x
          Asm.Call x -> goCall x
          Asm.TailCall x -> goCall x
          Asm.CallClosures x -> goCallClosures x
          Asm.TailCallClosures x -> goCallClosures x
          Asm.Return -> goCode

        goBranch :: Asm.CmdBranch -> Sem r Node
        goBranch Asm.CmdBranch {..} = do
          arg <- goCode
          br1 <- goCodeBlock _cmdBranchTrue
          br2 <- goCodeBlock _cmdBranchFalse
          return $
            Branch
              NodeBranch
                { _nodeBranchInfo = mempty,
                  _nodeBranchArg = arg,
                  _nodeBranchTrue = br1,
                  _nodeBranchFalse = br2
                }

        goCase :: Asm.CmdCase -> Sem r Node
        goCase Asm.CmdCase {..} = do
          arg <- goCode
          brs <- mapM (goCaseBranch loc) _cmdCaseBranches
          def <- maybe (return Nothing) (fmap Just . goDefaultBranch loc) _cmdCaseDefault
          return $
            Case
              NodeCase
                { _nodeCaseInfo = mempty,
                  _nodeCaseInductive = _cmdCaseInductive,
                  _nodeCaseArg = arg,
                  _nodeCaseBranches = brs,
                  _nodeCaseDefault = def
                }
          where
            loc = _cmdCaseInfo ^. Asm.commandInfoLocation

        goCaseBranch :: Maybe Location -> Asm.CaseBranch -> Sem r CaseBranch
        goCaseBranch loc Asm.CaseBranch {..} = case _caseBranchCode of
          [Asm.Save Asm.CmdSave {..}] -> do
            body <- pushTempStack $ goCodeBlock _cmdSaveCode
            return $
              CaseBranch
                { _caseBranchLocation = Nothing,
                  _caseBranchTag,
                  _caseBranchBody = body,
                  _caseBranchSave = True
                }
          Asm.Instr (Asm.CmdInstr _ Asm.Pop) : cmds -> do
            body <- goCodeBlock cmds
            return $
              CaseBranch
                { _caseBranchLocation = Nothing,
                  _caseBranchTag,
                  _caseBranchBody = body,
                  _caseBranchSave = False
                }
          [Asm.Instr (Asm.CmdInstr _ Asm.Return)] -> do
            off <- asks (^. tempSize)
            return $
              CaseBranch
                { _caseBranchLocation = Nothing,
                  _caseBranchTag,
                  _caseBranchBody = mkMemRef $ DRef $ mkTempRef $ OffsetRef off Nothing,
                  _caseBranchSave = True
                }
          _ ->
            throw
              TreeError
                { _treeErrorMsg = "expected 'save', 'pop' or 'ret' at the beginning of case branch code",
                  _treeErrorLoc = loc
                }

        goDefaultBranch :: Maybe Location -> Asm.Code -> Sem r Node
        goDefaultBranch loc = \case
          Asm.Instr (Asm.CmdInstr _ Asm.Pop) : cmds ->
            goCodeBlock cmds
          _ ->
            throw $
              TreeError
                { _treeErrorMsg = "expected 'pop' at the beginning of default case branch code",
                  _treeErrorLoc = loc
                }

        goSave :: Asm.CmdSave -> Sem r Node
        goSave Asm.CmdSave {..} = do
          arg <- goCode
          body <- pushTempStack $ goCodeBlock _cmdSaveCode
          return $
            Save
              NodeSave
                { _nodeSaveInfo = mempty,
                  _nodeSaveTempVar =
                    TempVar
                      { _tempVarName = _cmdSaveName,
                        _tempVarLocation = _cmdSaveInfo ^. Asm.commandInfoLocation,
                        _tempVarType = TyDynamic
                      },
                  _nodeSaveArg = arg,
                  _nodeSaveBody = body
                }

        goBinop :: BinaryOpcode -> Sem r Node
        goBinop op = do
          arg1 <- goCode
          arg2 <- goCode
          return $
            Binop
              NodeBinop
                { _nodeBinopInfo = mempty,
                  _nodeBinopOpcode = op,
                  _nodeBinopArg1 = arg1,
                  _nodeBinopArg2 = arg2
                }

        goUnop :: UnaryOpcode -> Sem r Node
        goUnop op = do
          arg <- goCode
          return $
            Unop
              NodeUnop
                { _nodeUnopInfo = mempty,
                  _nodeUnopArg = arg,
                  _nodeUnopOpcode = op
                }

        goCairo :: CairoOp -> Sem r Node
        goCairo op = do
          args <- replicateM (cairoOpArgsNum op) goCode
          return $
            Cairo
              NodeCairo
                { _nodeCairoInfo = mempty,
                  _nodeCairoOpcode = op,
                  _nodeCairoArgs = args
                }

        goPop :: Sem r Node
        goPop = do
          arg2 <- goCode
          arg1 <- goCode
          return $
            Binop
              NodeBinop
                { _nodeBinopInfo = mempty,
                  _nodeBinopOpcode = OpSeq,
                  _nodeBinopArg1 = arg1,
                  _nodeBinopArg2 = arg2
                }

        goSeqOp :: UnaryOpcode -> Sem r Node
        goSeqOp op = do
          arg <- goCode
          off <- asks (^. tempSize)
          let ref = mkMemRef $ DRef $ mkTempRef $ OffsetRef off Nothing
          return $
            Save
              NodeSave
                { _nodeSaveInfo = mempty,
                  _nodeSaveArg = arg,
                  _nodeSaveTempVar = TempVar Nothing Nothing TyDynamic,
                  _nodeSaveBody =
                    Binop
                      NodeBinop
                        { _nodeBinopInfo = mempty,
                          _nodeBinopOpcode = OpSeq,
                          _nodeBinopArg1 =
                            Unop
                              NodeUnop
                                { _nodeUnopInfo = mempty,
                                  _nodeUnopOpcode = op,
                                  _nodeUnopArg = ref
                                },
                          _nodeBinopArg2 = ref
                        }
                }

        goAssert :: Sem r Node
        goAssert = goSeqOp OpAssert

        goTrace :: Sem r Node
        goTrace = goSeqOp OpTrace

        goArgs :: Int -> Sem r [Node]
        goArgs n = mapM (const goCode) [1 .. n]

        goAllocConstr :: Tag -> Sem r Node
        goAllocConstr tag = do
          args <- goArgs argsNum
          return $
            AllocConstr
              NodeAllocConstr
                { _nodeAllocConstrInfo = mempty,
                  _nodeAllocConstrTag = tag,
                  _nodeAllocConstrArgs = args
                }
          where
            argsNum = Asm.lookupConstrInfo md tag ^. constructorArgsNum

        goAllocClosure :: Asm.InstrAllocClosure -> Sem r Node
        goAllocClosure Asm.InstrAllocClosure {..} = do
          args <- goArgs _allocClosureArgsNum
          return $
            AllocClosure
              NodeAllocClosure
                { _nodeAllocClosureInfo = mempty,
                  _nodeAllocClosureArgs = args,
                  _nodeAllocClosureFunSymbol = _allocClosureFunSymbol
                }

        goExtendClosure :: Asm.InstrExtendClosure -> Sem r Node
        goExtendClosure Asm.InstrExtendClosure {..} = do
          cl <- goCode
          args <- goArgs _extendClosureArgsNum
          return $
            ExtendClosure
              NodeExtendClosure
                { _nodeExtendClosureInfo = mempty,
                  _nodeExtendClosureArgs = nonEmpty' args,
                  _nodeExtendClosureFun = cl
                }

        goCall :: Asm.InstrCall -> Sem r Node
        goCall Asm.InstrCall {..} = case _callType of
          Asm.CallFun sym -> do
            args <- goArgs _callArgsNum
            return $
              Call
                NodeCall
                  { _nodeCallInfo = mempty,
                    _nodeCallType = CallFun sym,
                    _nodeCallArgs = args
                  }
          Asm.CallClosure -> do
            cl <- goCode
            args <- goArgs _callArgsNum
            return $
              Call
                NodeCall
                  { _nodeCallInfo = mempty,
                    _nodeCallType = CallClosure cl,
                    _nodeCallArgs = args
                  }

        goCallClosures :: Asm.InstrCallClosures -> Sem r Node
        goCallClosures Asm.InstrCallClosures {..} = do
          cl <- goCode
          args <- goArgs _callClosuresArgsNum
          return $
            CallClosures
              NodeCallClosures
                { _nodeCallClosuresInfo = mempty,
                  _nodeCallClosuresFun = cl,
                  _nodeCallClosuresArgs = nonEmpty' args
                }

        pushTempStack :: Sem r a -> Sem r a
        pushTempStack = local (over tempSize (+ 1))
