module Juvix.Compiler.Asm.Extra.Recursors
  ( module Juvix.Compiler.Asm.Extra.Recursors,
    module Juvix.Compiler.Asm.Extra.Memory,
  )
where

import Juvix.Compiler.Asm.Data.Module
import Juvix.Compiler.Asm.Data.Stack qualified as Stack
import Juvix.Compiler.Asm.Error
import Juvix.Compiler.Asm.Extra.Base
import Juvix.Compiler.Asm.Extra.Memory
import Juvix.Compiler.Asm.Extra.Type
import Juvix.Compiler.Asm.Language
import Juvix.Compiler.Asm.Pretty

-- | Recursor signature. Contains read-only recursor parameters.
data RecursorSig m r a = RecursorSig
  { _recursorModule :: Module,
    _recurseInstr :: m -> CmdInstr -> Sem r a,
    _recurseBranch :: Bool -> m -> CmdBranch -> [a] -> [a] -> Sem r a,
    _recurseCase :: Bool -> m -> CmdCase -> [[a]] -> Maybe [a] -> Sem r a,
    _recurseSave :: m -> CmdSave -> [a] -> Sem r a
  }

makeLenses ''RecursorSig

-- | General recursor function. For most uses it is probably an overkill.
-- Consider using `recurseS` if you only need stack height information.
recurseFun :: (Member (Error AsmError) r) => RecursorSig Memory r a -> FunctionInfo -> Sem r [a]
recurseFun sig fi = recurse sig (argumentsFromFunctionInfo fi) (fi ^. functionCode)

recurse :: (Member (Error AsmError) r) => RecursorSig Memory r a -> Arguments -> Code -> Sem r [a]
recurse sig args = fmap snd . recurse' sig (mkMemory args)

recurse' :: forall r a. (Member (Error AsmError) r) => RecursorSig Memory r a -> Memory -> Code -> Sem r (Memory, [a])
recurse' sig = go True
  where
    go :: Bool -> Memory -> Code -> Sem r (Memory, [a])
    go isTail mem = \case
      [] -> return (mem, [])
      h : t -> case h of
        Instr x -> do
          checkNextInstr isTail (x ^. (cmdInstrInfo . commandInfoLocation)) (x ^. cmdInstrInstruction) t
          goNextCmd isTail (x ^. (cmdInstrInfo . commandInfoLocation)) (goInstr mem x) t
        Branch x ->
          goNextCmd isTail (x ^. (cmdBranchInfo . commandInfoLocation)) (goBranch (isTail && null t) mem x) t
        Case x ->
          goNextCmd isTail (x ^. (cmdCaseInfo . commandInfoLocation)) (goCase (isTail && null t) mem x) t
        Save x ->
          goNextCmd isTail (x ^. (cmdSaveInfo . commandInfoLocation)) (goSave (isTail && null t) mem x) t

    goNextCmd :: Bool -> Maybe Location -> Sem r (Memory, a) -> Code -> Sem r (Memory, [a])
    goNextCmd isTail loc mp t = do
      (mem', r) <- mp
      when (isTail && null t && length (mem' ^. memoryValueStack) /= 1) $
        throw $
          AsmError loc "expected value stack height 1 on function exit"
      (mem'', rs) <- go isTail mem' t
      return (mem'', r : rs)

    checkNextInstr :: Bool -> Maybe Location -> Instruction -> Code -> Sem r ()
    checkNextInstr isTail loc instr code
      | isFinalInstr instr && not (null code) =
          throw $ AsmError loc "unreachable code"
      | isTail && null code && not (isFinalInstr instr) =
          throw $ AsmError loc "expected a return or a tail call"
      | otherwise =
          return ()

    goInstr :: Memory -> CmdInstr -> Sem r (Memory, a)
    goInstr memory cmd = do
      a <- (sig ^. recurseInstr) memory cmd
      mem <- fixMemInstr memory (cmd ^. cmdInstrInstruction)
      return (mem, a)
      where
        loc = cmd ^. cmdInstrInfo . commandInfoLocation

        fixMemInstr :: Memory -> Instruction -> Sem r Memory
        fixMemInstr mem instr =
          case instr of
            Binop op ->
              fixMemBinop mem op
            Unop op ->
              fixMemUnop mem op
            Cairo op ->
              fixMemCairo mem op
            Push val -> do
              ty <- getValueType' loc (sig ^. recursorModule) mem val
              return (pushValueStack ty mem)
            Pop -> do
              when (null (mem ^. memoryValueStack)) $
                throw $
                  AsmError loc "popping empty value stack"
              return (popValueStack 1 mem)
            Assert ->
              return mem
            Trace ->
              return mem
            Dump ->
              return mem
            Failure ->
              return $ pushValueStack TyDynamic (popValueStack 1 mem)
            Prealloc {} ->
              return mem
            AllocConstr tag -> do
              let ci = lookupConstrInfo (sig ^. recursorModule) tag
                  n = ci ^. constructorArgsNum
                  tyargs = typeArgs (ci ^. constructorType)
              checkValueStack' loc (sig ^. recursorModule) tyargs mem
              tys <-
                zipWithM
                  (\ty idx -> unifyTypes'' loc (sig ^. recursorModule) ty (topValueStack' idx mem))
                  tyargs
                  [0 ..]
              return $
                pushValueStack (mkTypeConstr (ci ^. constructorInductive) tag tys) $
                  popValueStack n mem
            AllocClosure InstrAllocClosure {..} -> do
              let fi = lookupFunInfo (sig ^. recursorModule) _allocClosureFunSymbol
                  (tyargs, tgt) = unfoldType (fi ^. functionType)
              checkValueStack' loc (sig ^. recursorModule) (take _allocClosureArgsNum tyargs) mem
              return $
                pushValueStack (mkTypeFun (drop _allocClosureArgsNum tyargs) tgt) $
                  popValueStack _allocClosureArgsNum mem
            ExtendClosure x ->
              fixMemExtendClosure mem x
            Call x ->
              fixMemCall mem x
            TailCall x ->
              fixMemCall (dropTempStack mem) x
            CallClosures x ->
              fixMemCallClosures mem x
            TailCallClosures x ->
              fixMemCallClosures (dropTempStack mem) x
            Return ->
              return (dropTempStack mem)

        fixMemBinOp' :: Memory -> Type -> Type -> Type -> Sem r Memory
        fixMemBinOp' mem ty0 ty1 rty = do
          checkValueStack' loc (sig ^. recursorModule) [ty0, ty1] mem
          return $ pushValueStack rty (popValueStack 2 mem)

        fixMemIntOp :: Memory -> Sem r Memory
        fixMemIntOp mem = fixMemBinOp' mem mkTypeInteger mkTypeInteger mkTypeInteger

        fixMemFieldOp :: Memory -> Sem r Memory
        fixMemFieldOp mem = fixMemBinOp' mem TyField TyField TyField

        fixMemBinop :: Memory -> BinaryOp -> Sem r Memory
        fixMemBinop mem op = case op of
          OpIntAdd ->
            fixMemIntOp mem
          OpIntSub ->
            fixMemIntOp mem
          OpIntMul ->
            fixMemIntOp mem
          OpIntDiv ->
            fixMemIntOp mem
          OpIntMod ->
            fixMemIntOp mem
          OpBool OpIntLt ->
            fixMemBinOp' mem mkTypeInteger mkTypeInteger mkTypeBool
          OpBool OpIntLe ->
            fixMemBinOp' mem mkTypeInteger mkTypeInteger mkTypeBool
          OpFieldAdd ->
            fixMemFieldOp mem
          OpFieldSub ->
            fixMemFieldOp mem
          OpFieldMul ->
            fixMemFieldOp mem
          OpFieldDiv ->
            fixMemFieldOp mem
          OpBool OpEq ->
            fixMemBinOp' mem TyDynamic TyDynamic mkTypeBool
          OpStrConcat ->
            fixMemBinOp' mem TyString TyString TyString

        fixMemUnop :: Memory -> UnaryOp -> Sem r Memory
        fixMemUnop mem op = case op of
          OpShow ->
            return (pushValueStack TyString (popValueStack 1 mem))
          OpStrToInt ->
            checkUnop TyString mkTypeInteger
          OpArgsNum -> do
            when (null (mem ^. memoryValueStack)) $
              throw $
                AsmError loc "empty value stack"
            checkFunType (topValueStack' 0 mem)
            return $ pushValueStack mkTypeInteger (popValueStack 1 mem)
          OpIntToField ->
            checkUnop mkTypeInteger TyField
          OpFieldToInt ->
            checkUnop TyField mkTypeInteger
          OpUInt8ToInt ->
            checkUnop mkTypeUInt8 mkTypeInteger
          OpIntToUInt8 ->
            checkUnop mkTypeInteger mkTypeUInt8
          where
            checkUnop :: Type -> Type -> Sem r Memory
            checkUnop ty1 ty2 = do
              checkValueStack' loc (sig ^. recursorModule) [ty1] mem
              return (pushValueStack ty2 (popValueStack 1 mem))

        fixMemCairo :: Memory -> CairoOp -> Sem r Memory
        fixMemCairo mem op = do
          return (pushValueStack TyDynamic (popValueStack (cairoOpArgsNum op) mem))

        fixMemExtendClosure :: Memory -> InstrExtendClosure -> Sem r Memory
        fixMemExtendClosure mem InstrExtendClosure {..} = do
          when (length (mem ^. memoryValueStack) < _extendClosureArgsNum + 1) $
            throw $
              AsmError loc "invalid closure extension: not enough values on the stack"
          let ty = topValueStack' 0 mem
          checkFunType ty
          when (ty /= TyDynamic && length (typeArgs ty) < _extendClosureArgsNum) $
            throw $
              AsmError loc "invalid closure extension: too many supplied arguments"
          fixMemValueStackArgs mem 1 _extendClosureArgsNum ty

        fixMemCall :: Memory -> InstrCall -> Sem r Memory
        fixMemCall mem InstrCall {..} = do
          let k = if _callType == CallClosure then 1 else 0
          when (length (mem ^. memoryValueStack) < _callArgsNum + k) $
            throw $
              AsmError loc "invalid call: not enough values on the stack"
          let ty = case _callType of
                CallClosure -> topValueStack' 0 mem
                CallFun sym -> lookupFunInfo (sig ^. recursorModule) sym ^. functionType
          let argsNum = case _callType of
                CallClosure -> length (typeArgs ty)
                CallFun sym -> lookupFunInfo (sig ^. recursorModule) sym ^. functionArgsNum
          when (argsNum /= 0) $
            checkFunType ty
          when (ty /= TyDynamic && argsNum /= _callArgsNum) $
            throw $
              AsmError loc "invalid call: the number of supplied arguments doesn't match the number of expected arguments"
          fixMemValueStackArgs mem k _callArgsNum ty

        fixMemCallClosures :: Memory -> InstrCallClosures -> Sem r Memory
        fixMemCallClosures mem InstrCallClosures {..} = do
          when (_callClosuresArgsNum < 1) $
            throw $
              AsmError loc "invalid closure call: expected at least one supplied argument"
          when (null (mem ^. memoryValueStack)) $
            throw $
              AsmError loc "invalid closure call: value stack is empty"
          let ty = topValueStack' 0 mem
          checkFunType ty
          if
              | ty == TyDynamic -> do
                  let mem' = popValueStack 1 mem
                  return $ pushValueStack TyDynamic (popValueStack _callClosuresArgsNum mem')
              | length (typeArgs ty) < _callClosuresArgsNum -> do
                  let n = length (typeArgs ty)
                  mem' <- fixMemCall mem (InstrCall CallClosure n)
                  fixMemCallClosures mem' (InstrCallClosures (_callClosuresArgsNum - n))
              | length (typeArgs ty) > _callClosuresArgsNum ->
                  fixMemExtendClosure mem (InstrExtendClosure _callClosuresArgsNum)
              | otherwise ->
                  fixMemCall mem (InstrCall CallClosure (length (typeArgs ty)))

        fixMemValueStackArgs :: Memory -> Int -> Int -> Type -> Sem r Memory
        fixMemValueStackArgs mem k argsNum ty = do
          checkValueStackHeight' loc (argsNum + k) mem
          let mem' = popValueStack k mem
          unless (ty == TyDynamic) $
            checkValueStack' loc (sig ^. recursorModule) (take argsNum (typeArgs ty)) mem'
          let tyargs = topValuesFromValueStack' argsNum mem'
          -- `typeArgs ty` may be shorter than `tyargs` only if `ty` is dynamic
          zipWithM_ (unifyTypes'' loc (sig ^. recursorModule)) tyargs (typeArgs ty)
          return $
            pushValueStack (mkTypeFun (drop argsNum (typeArgs ty)) (typeTarget ty)) $
              popValueStack argsNum mem'

        dropTempStack :: Memory -> Memory
        dropTempStack mem = mem {_memoryTempStack = Stack.empty}

        checkFunType :: Type -> Sem r ()
        checkFunType = \case
          TyDynamic -> return ()
          TyFun {} -> return ()
          ty ->
            throw $
              AsmError
                loc
                ( "expected a function, got value of type "
                    <> ppTrace (sig ^. recursorModule) ty
                )

    goBranch :: Bool -> Memory -> CmdBranch -> Sem r (Memory, a)
    goBranch isTail mem cmd@CmdBranch {..} = do
      checkValueStack' loc (sig ^. recursorModule) [mkTypeBool] mem
      let mem0 = popValueStack 1 mem
      (mem1, as1) <- go isTail mem0 _cmdBranchTrue
      (mem2, as2) <- go isTail mem0 _cmdBranchFalse
      a' <- (sig ^. recurseBranch) isTail mem cmd as1 as2
      mem' <- unifyMemory' loc (sig ^. recursorModule) mem1 mem2
      checkBranchInvariant 1 loc mem0 mem'
      return (mem', a')
      where
        loc = cmd ^. cmdBranchInfo . commandInfoLocation

    goCase :: Bool -> Memory -> CmdCase -> Sem r (Memory, a)
    goCase isTail mem cmd@CmdCase {..} = do
      checkValueStack' loc (sig ^. recursorModule) [mkTypeInductive _cmdCaseInductive] mem
      rs <- mapM (go isTail mem . (^. caseBranchCode)) _cmdCaseBranches
      let mems = map fst rs
          ass = map snd rs
      rd <- maybe (return Nothing) (fmap Just . go isTail mem) _cmdCaseDefault
      let md = fmap fst rd
          ad = fmap snd rd
      a' <- (sig ^. recurseCase) isTail mem cmd ass ad
      case mems of
        [] -> return (fromMaybe mem md, a')
        mem0 : mems' -> do
          mem' <- foldr (\m rm -> rm >>= unifyMemory' loc (sig ^. recursorModule) m) (return mem0) mems'
          mem'' <- maybe (return mem') (unifyMemory' loc (sig ^. recursorModule) mem') md
          checkBranchInvariant 0 loc mem mem''
          return (mem'', a')
      where
        loc = cmd ^. (cmdCaseInfo . commandInfoLocation)

    goSave :: Bool -> Memory -> CmdSave -> Sem r (Memory, a)
    goSave isTail mem cmd@CmdSave {..} = do
      when (null (mem ^. memoryValueStack)) $
        throw $
          AsmError loc "popping empty value stack"
      let mem1 = pushTempStack (topValueStack' 0 mem) (popValueStack 1 mem)
      (mem2, a) <- go isTail mem1 _cmdSaveCode
      a' <- (sig ^. recurseSave) mem cmd a
      when (not isTail && _cmdSaveIsTail) $
        throw $
          AsmError loc "'tsave' not in tail position"
      when (isTail && not _cmdSaveIsTail) $
        throw $
          AsmError loc "'save' in tail position"
      when (not isTail && null (mem2 ^. memoryTempStack)) $
        throw $
          AsmError loc "popping empty temporary stack"
      return (if isTail then mem2 else popTempStack 1 mem2, a')
      where
        loc = _cmdSaveInfo ^. commandInfoLocation

    checkBranchInvariant :: Int -> Maybe Location -> Memory -> Memory -> Sem r ()
    checkBranchInvariant k loc mem mem' = do
      unless (length (mem' ^. memoryValueStack) == length (mem ^. memoryValueStack) + k) $
        throw $
          AsmError loc ("wrong value stack height after branching (must increase by " <> show k <> ")")
      unless
        ( null (mem' ^. memoryTempStack)
            || length (mem' ^. memoryTempStack) == length (mem ^. memoryTempStack)
        )
        $ throw
        $ AsmError loc "temporary stack height changed after branching"

data StackInfo = StackInfo
  { _stackInfoValueStackHeight :: Int,
    _stackInfoTempStackHeight :: Int
  }
  deriving stock (Eq)

makeLenses ''StackInfo

initialStackInfo :: StackInfo
initialStackInfo = StackInfo {_stackInfoValueStackHeight = 0, _stackInfoTempStackHeight = 0}

-- | A simplified recursor which doesn't perform validity checking and only
-- computes stack height information. This makes a significant performance
-- difference, since we'll have many passes recursing over the entire JuvixAsm
-- program code which need only stack height information. Also, the code using
-- the simplified recursor can itself be simpler if it doesn't need the extra
-- info provided by the full recursor.
recurseS :: forall r a. (Member (Error AsmError) r) => RecursorSig StackInfo r a -> Code -> Sem r [a]
recurseS sig code = snd <$> recurseS' sig initialStackInfo code

recurseS' :: forall r a. (Member (Error AsmError) r) => RecursorSig StackInfo r a -> StackInfo -> Code -> Sem r (StackInfo, [a])
recurseS' sig = go True
  where
    go :: Bool -> StackInfo -> Code -> Sem r (StackInfo, [a])
    go isTail si = \case
      [] -> return (si, [])
      h : t -> case h of
        Instr x -> do
          goNextCmd isTail (goInstr si x) t
        Branch x ->
          goNextCmd isTail (goBranch (isTail && null t) si x) t
        Case x ->
          goNextCmd isTail (goCase (isTail && null t) si x) t
        Save x ->
          goNextCmd isTail (goSave si x) t

    goNextCmd :: Bool -> Sem r (StackInfo, a) -> Code -> Sem r (StackInfo, [a])
    goNextCmd isTail mp t = do
      (si', r) <- mp
      (si'', rs) <- go isTail si' t
      return (si'', r : rs)

    goInstr :: StackInfo -> CmdInstr -> Sem r (StackInfo, a)
    goInstr stackInfo cmd = do
      a <- (sig ^. recurseInstr) stackInfo cmd
      si' <- fixStackInstr stackInfo (cmd ^. cmdInstrInstruction)
      return (si', a)
      where
        fixStackInstr :: StackInfo -> Instruction -> Sem r StackInfo
        fixStackInstr si instr =
          case instr of
            Binop {} ->
              fixStackBinOp si
            Unop {} ->
              return si
            Cairo op ->
              return (stackInfoPopValueStack (cairoOpArgsNum op - 1) si)
            Push {} -> do
              return (stackInfoPushValueStack 1 si)
            Pop -> do
              return (stackInfoPopValueStack 1 si)
            Assert ->
              return si
            Trace ->
              return si
            Dump ->
              return si
            Failure ->
              return si
            Prealloc {} ->
              return si
            AllocConstr tag -> do
              let ci = lookupConstrInfo (sig ^. recursorModule) tag
                  n = ci ^. constructorArgsNum
              return $
                stackInfoPopValueStack (n - 1) si
            AllocClosure InstrAllocClosure {..} -> do
              return $
                stackInfoPopValueStack (_allocClosureArgsNum - 1) si
            ExtendClosure InstrExtendClosure {..} ->
              return $
                stackInfoPopValueStack _extendClosureArgsNum si
            Call x ->
              fixStackCall si x
            TailCall x ->
              fixStackCall (dropTempStack si) x
            CallClosures x ->
              fixStackCallClosures si x
            TailCallClosures x ->
              fixStackCallClosures (dropTempStack si) x
            Return ->
              return (dropTempStack si)

        fixStackBinOp :: StackInfo -> Sem r StackInfo
        fixStackBinOp si = return $ stackInfoPopValueStack 1 si

        fixStackCall :: StackInfo -> InstrCall -> Sem r StackInfo
        fixStackCall si InstrCall {..} = do
          return $ stackInfoPopValueStack (_callArgsNum + (if _callType == CallClosure then 1 else 0) - 1) si

        fixStackCallClosures :: StackInfo -> InstrCallClosures -> Sem r StackInfo
        fixStackCallClosures si InstrCallClosures {..} = do
          return $ stackInfoPopValueStack _callClosuresArgsNum si

    goBranch :: Bool -> StackInfo -> CmdBranch -> Sem r (StackInfo, a)
    goBranch isTail si cmd@CmdBranch {..} = do
      let si0 = stackInfoPopValueStack 1 si
      (si1, as1) <- go isTail si0 _cmdBranchTrue
      (si2, as2) <- go isTail si0 _cmdBranchFalse
      a' <- (sig ^. recurseBranch) isTail si cmd as1 as2
      checkStackInfo loc si1 si2
      return (si1, a')
      where
        loc = cmd ^. cmdBranchInfo . commandInfoLocation

    goCase :: Bool -> StackInfo -> CmdCase -> Sem r (StackInfo, a)
    goCase isTail si cmd@CmdCase {..} = do
      rs <- mapM (go isTail si . (^. caseBranchCode)) _cmdCaseBranches
      let sis = map fst rs
          ass = map snd rs
      rd <- maybe (return Nothing) (fmap Just . go isTail si) _cmdCaseDefault
      let sd = fmap fst rd
          ad = fmap snd rd
      a' <- (sig ^. recurseCase) isTail si cmd ass ad
      case sis of
        [] -> return (fromMaybe si sd, a')
        si0 : sis' -> do
          mapM_ (checkStackInfo loc si0) sis'
          forM_ sd (checkStackInfo loc si0)
          return (si0, a')
      where
        loc = cmd ^. (cmdCaseInfo . commandInfoLocation)

    goSave :: StackInfo -> CmdSave -> Sem r (StackInfo, a)
    goSave si cmd@CmdSave {..} = do
      let si1 = stackInfoPushTempStack 1 (stackInfoPopValueStack 1 si)
      (si2, c) <- go _cmdSaveIsTail si1 _cmdSaveCode
      c' <- (sig ^. recurseSave) si cmd c
      let si' = if _cmdSaveIsTail then si2 else stackInfoPopTempStack 1 si2
      return (si', c')

    checkStackInfo :: Maybe Location -> StackInfo -> StackInfo -> Sem r ()
    checkStackInfo loc si1 si2 =
      when (si1 /= si2) $
        throw $
          AsmError loc "stack height mismatch"

    stackInfoPushValueStack :: Int -> StackInfo -> StackInfo
    stackInfoPushValueStack n si = si {_stackInfoValueStackHeight = si ^. stackInfoValueStackHeight + n}

    stackInfoPopValueStack :: Int -> StackInfo -> StackInfo
    stackInfoPopValueStack n si = si {_stackInfoValueStackHeight = si ^. stackInfoValueStackHeight - n}

    stackInfoPushTempStack :: Int -> StackInfo -> StackInfo
    stackInfoPushTempStack n si = si {_stackInfoTempStackHeight = si ^. stackInfoTempStackHeight + n}

    stackInfoPopTempStack :: Int -> StackInfo -> StackInfo
    stackInfoPopTempStack n si = si {_stackInfoTempStackHeight = si ^. stackInfoTempStackHeight - n}

    dropTempStack :: StackInfo -> StackInfo
    dropTempStack si = si {_stackInfoTempStackHeight = 0}

-- | Fold signature. Contains read-only fold parameters. A fold (`foldS`) goes
-- through the code from right to left (from end to beginning) accumulating
-- values. The `a` is the type of the accumulated values. The last argument to
-- the `_fold*` functions below is the accumulator. The `_foldAdjust` function
-- adjusts the accumulator when entering a block (in `CmdBranch`, `CmdCase`,
-- `CmdSave`). For example, for `save { P1 }; P2` let `a2` be the accumulator
-- value after folding `P2`. Then `P1` is folded with the initial accumulator
-- `_foldAdjust a2`. However, `_foldSave` is called with `_foldSave m c a1 a2`,
-- i.e., with the original `a2`, where `a1` is the result of folding `P1` with
-- initial accumulator `_foldAdjust a2`. In most simple cases, one can set
-- `_foldAdjust` to `const empty` where `empty` is the empty accumulator value
-- (e.g. `mempty` for a monoid).
data FoldSig m r a = FoldSig
  { _foldModule :: Module,
    _foldAdjust :: a -> a,
    _foldInstr :: m -> CmdInstr -> a -> Sem r a,
    _foldBranch :: m -> CmdBranch -> a -> a -> a -> Sem r a,
    _foldCase :: m -> CmdCase -> [a] -> Maybe a -> a -> Sem r a,
    _foldSave :: m -> CmdSave -> a -> a -> Sem r a
  }

makeLenses ''FoldSig

foldS :: forall r a. (Member (Error AsmError) r) => FoldSig StackInfo r a -> Code -> a -> Sem r a
foldS sig code a = snd <$> foldS' sig initialStackInfo code a

foldS' :: forall r a. (Member (Error AsmError) r) => FoldSig StackInfo r a -> StackInfo -> Code -> a -> Sem r (StackInfo, a)
foldS' sig si code acc = do
  (si', fs) <- recurseS' sig' si code
  a' <- compose' fs acc
  return (si', a')
  where
    sig' :: RecursorSig StackInfo r (a -> Sem r a)
    sig' =
      RecursorSig
        { _recursorModule = sig ^. foldModule,
          _recurseInstr = \s cmd -> return ((sig ^. foldInstr) s cmd),
          _recurseBranch = \_ s cmd br1 br2 ->
            return
              ( \a -> do
                  let a' = (sig ^. foldAdjust) a
                  a1 <- compose' br1 a'
                  a2 <- compose' br2 a'
                  (sig ^. foldBranch) s cmd a1 a2 a
              ),
          _recurseCase = \_ s cmd brs md ->
            return
              ( \a -> do
                  let a' = (sig ^. foldAdjust) a
                  as <- mapM (`compose'` a') brs
                  ad <- case md of
                    Just d -> Just <$> compose' d a'
                    Nothing -> return Nothing
                  (sig ^. foldCase) s cmd as ad a
              ),
          _recurseSave = \s cmd br ->
            return
              ( \a -> do
                  let a' = (sig ^. foldAdjust) a
                  a'' <- compose' br a'
                  (sig ^. foldSave) s cmd a'' a
              )
        }

    compose' :: [a -> Sem r a] -> a -> Sem r a
    compose' lst x = foldr (=<<) (return x) lst
