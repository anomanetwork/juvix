module Juvix.Compiler.Asm.Translation.FromTree (fromTree) where

import Data.DList qualified as DL
import Juvix.Compiler.Asm.Data.Module
import Juvix.Compiler.Asm.Extra.Base
import Juvix.Compiler.Asm.Language
import Juvix.Compiler.Tree.Data.Module qualified as Tree
import Juvix.Compiler.Tree.Language qualified as Tree

-- DList for O(1) snoc and append
type Code' = DL.DList Command

fromTree :: Tree.Module -> Module
fromTree md =
  Module
    { _moduleId = md ^. moduleId,
      _moduleInfoTable = tab',
      _moduleImports = md ^. moduleImports,
      _moduleImportsTable = mempty,
      _moduleSHA256 = md ^. moduleSHA256
    }
  where
    tab = computeCombinedInfoTable md
    tab' =
      InfoTable
        { _infoMainFunction = tab ^. Tree.infoMainFunction,
          _infoFunctions = genCode <$> tab ^. Tree.infoFunctions,
          _infoInductives = tab ^. Tree.infoInductives,
          _infoConstrs = tab ^. Tree.infoConstrs
        }

-- Generate code for a single function.
genCode :: Tree.FunctionInfo -> FunctionInfo
genCode fi =
  FunctionInfo
    { _functionName = fi ^. Tree.functionName,
      _functionLocation = fi ^. Tree.functionLocation,
      _functionSymbol = fi ^. Tree.functionSymbol,
      _functionArgsNum = fi ^. Tree.functionArgsNum,
      _functionArgNames = fi ^. Tree.functionArgNames,
      _functionType = fi ^. Tree.functionType,
      _functionCode = DL.toList $ go True (fi ^. Tree.functionCode),
      _functionExtra = Nothing -- computed later
    }
  where
    go :: Bool -> Tree.Node -> Code'
    go isTail node = case node of
      Tree.Binop x -> goBinop isTail x
      Tree.Unop x -> goUnop isTail x
      Tree.Cairo x -> goCairo isTail x
      Tree.ByteArray {} -> error "ByteArray instructions are not supported in the Asm backend"
      Tree.Anoma {} -> error "Anoma instructions are not supported in the Asm backend"
      Tree.Constant x -> goConstant isTail x
      Tree.MemRef x -> goMemRef isTail x
      Tree.AllocConstr x -> goAllocConstr isTail x
      Tree.AllocClosure x -> goAllocClosure isTail x
      Tree.ExtendClosure x -> goExtendClosure isTail x
      Tree.Call x -> goCall isTail x
      Tree.CallClosures x -> goCallClosures isTail x
      Tree.Branch x -> goBranch isTail x
      Tree.Case x -> goCase isTail x
      Tree.Save x -> goSave isTail x

    goBinop :: Bool -> Tree.NodeBinop -> Code'
    goBinop isTail Tree.NodeBinop {..} = case _nodeBinopOpcode of
      Tree.OpSeq ->
        DL.append
          (go False _nodeBinopArg1)
          ( DL.cons
              (mkInstr Pop)
              (go isTail _nodeBinopArg2)
          )
      Tree.PrimBinop op ->
        snocReturn isTail $
          DL.append
            (go False _nodeBinopArg2)
            ( DL.snoc
                (go False _nodeBinopArg1)
                (mkBinop op)
            )

    goUnop :: Bool -> Tree.NodeUnop -> Code'
    goUnop isTail Tree.NodeUnop {..} =
      snocReturn isTail $
        DL.snoc (go False _nodeUnopArg) (genUnOp _nodeUnopOpcode)

    goCairo :: Bool -> Tree.NodeCairo -> Code'
    goCairo isTail Tree.NodeCairo {..} =
      snocReturn isTail $
        DL.snoc (goArgs _nodeCairoArgs) (mkInstr $ Cairo _nodeCairoOpcode)

    goConstant :: Bool -> Tree.NodeConstant -> Code'
    goConstant isTail Tree.NodeConstant {..} =
      snocReturn isTail $
        DL.singleton $
          mkInstr $
            Push (Constant _nodeConstant)

    goMemRef :: Bool -> Tree.NodeMemRef -> Code'
    goMemRef isTail Tree.NodeMemRef {..} =
      snocReturn isTail $
        DL.singleton $
          mkInstr (Push (Ref _nodeMemRef))

    goAllocConstr :: Bool -> Tree.NodeAllocConstr -> Code'
    goAllocConstr isTail Tree.NodeAllocConstr {..} =
      snocReturn isTail $
        DL.snoc
          (goArgs _nodeAllocConstrArgs)
          (mkInstr (AllocConstr _nodeAllocConstrTag))

    goAllocClosure :: Bool -> Tree.NodeAllocClosure -> Code'
    goAllocClosure isTail Tree.NodeAllocClosure {..} =
      snocReturn isTail $
        DL.snoc
          (goArgs _nodeAllocClosureArgs)
          ( mkInstr $
              AllocClosure $
                InstrAllocClosure
                  { _allocClosureFunSymbol = _nodeAllocClosureFunSymbol,
                    _allocClosureArgsNum = length _nodeAllocClosureArgs
                  }
          )

    goExtendClosure :: Bool -> Tree.NodeExtendClosure -> Code'
    goExtendClosure isTail Tree.NodeExtendClosure {..} =
      snocReturn isTail $
        DL.append
          (goArgs (toList _nodeExtendClosureArgs))
          ( DL.snoc
              (go False _nodeExtendClosureFun)
              ( mkInstr $
                  ExtendClosure $
                    InstrExtendClosure
                      { _extendClosureArgsNum = length _nodeExtendClosureArgs
                      }
              )
          )

    goCall :: Bool -> Tree.NodeCall -> Code'
    goCall isTail Tree.NodeCall {..} = case _nodeCallType of
      Tree.CallFun sym ->
        DL.snoc
          (goArgs _nodeCallArgs)
          ( mkInstr $
              (if isTail then TailCall else Call) $
                InstrCall
                  { _callType = CallFun sym,
                    _callArgsNum = length _nodeCallArgs
                  }
          )
      Tree.CallClosure arg ->
        DL.append
          (goArgs _nodeCallArgs)
          ( DL.snoc
              (go False arg)
              ( mkInstr $
                  (if isTail then TailCall else Call) $
                    InstrCall
                      { _callType = CallClosure,
                        _callArgsNum = length _nodeCallArgs
                      }
              )
          )

    goCallClosures :: Bool -> Tree.NodeCallClosures -> Code'
    goCallClosures isTail Tree.NodeCallClosures {..} =
      DL.append
        (goArgs (toList _nodeCallClosuresArgs))
        ( DL.snoc (go False _nodeCallClosuresFun) $
            mkInstr $
              (if isTail then TailCallClosures else CallClosures) $
                InstrCallClosures
                  { _callClosuresArgsNum = length _nodeCallClosuresArgs
                  }
        )

    goBranch :: Bool -> Tree.NodeBranch -> Code'
    goBranch isTail Tree.NodeBranch {..} =
      DL.snoc
        (go False _nodeBranchArg)
        ( Branch
            CmdBranch
              { _cmdBranchInfo = emptyInfo,
                _cmdBranchTrue = DL.toList $ go isTail _nodeBranchTrue,
                _cmdBranchFalse = DL.toList $ go isTail _nodeBranchFalse
              }
        )

    goCase :: Bool -> Tree.NodeCase -> Code'
    goCase isTail Tree.NodeCase {..} =
      DL.snoc
        (go False _nodeCaseArg)
        ( Case
            CmdCase
              { _cmdCaseInfo = emptyInfo,
                _cmdCaseInductive = _nodeCaseInductive,
                _cmdCaseBranches = goCaseBranch isTail <$> _nodeCaseBranches,
                _cmdCaseDefault =
                  DL.toList . DL.cons (mkInstr Pop) . go isTail <$> _nodeCaseDefault
              }
        )

    goCaseBranch :: Bool -> Tree.CaseBranch -> CaseBranch
    goCaseBranch isTail Tree.CaseBranch {..}
      | _caseBranchSave =
          CaseBranch
            { _caseBranchTag,
              _caseBranchCode =
                [ Save $
                    CmdSave
                      { _cmdSaveInfo = emptyInfo,
                        _cmdSaveName = Nothing,
                        _cmdSaveIsTail = isTail,
                        _cmdSaveCode = DL.toList $ go isTail _caseBranchBody
                      }
                ]
            }
      | otherwise =
          CaseBranch
            { _caseBranchTag,
              _caseBranchCode =
                DL.toList $ DL.cons (mkInstr Pop) $ go isTail _caseBranchBody
            }

    goSave :: Bool -> Tree.NodeSave -> Code'
    goSave isTail Tree.NodeSave {..} =
      DL.snoc
        (go False _nodeSaveArg)
        ( Save
            CmdSave
              { _cmdSaveInfo = emptyInfo,
                _cmdSaveName = _nodeSaveTempVar ^. Tree.tempVarName,
                _cmdSaveIsTail = isTail,
                _cmdSaveCode = DL.toList $ go isTail _nodeSaveBody
              }
        )

    goArgs :: [Tree.Node] -> Code'
    goArgs args = DL.concat (map (go False) (reverse args))

    genUnOp :: Tree.UnaryOpcode -> Command
    genUnOp op = case op of
      Tree.PrimUnop op' -> mkUnop op'
      Tree.OpAssert -> mkInstr Assert
      Tree.OpTrace -> mkInstr Trace
      Tree.OpFail -> mkInstr Failure

    snocReturn :: Bool -> Code' -> Code'
    snocReturn True code = DL.snoc code (mkInstr Return)
    snocReturn False code = code
