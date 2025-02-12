module Juvix.Compiler.Casm.Translation.FromReg where

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.Text qualified as Text
import Juvix.Compiler.Casm.Data.Builtins
import Juvix.Compiler.Casm.Data.LabelInfoBuilder
import Juvix.Compiler.Casm.Data.Limits
import Juvix.Compiler.Casm.Data.Result
import Juvix.Compiler.Casm.Extra.Base
import Juvix.Compiler.Casm.Extra.Stdlib
import Juvix.Compiler.Casm.Language
import Juvix.Compiler.Casm.Translation.FromReg.CasmBuilder
import Juvix.Compiler.Reg.Data.Blocks.Module qualified as Reg
import Juvix.Compiler.Reg.Extra.Blocks.Info qualified as Reg
import Juvix.Compiler.Reg.Language.Blocks qualified as Reg
import Juvix.Compiler.Reg.Pretty qualified as Reg
import Juvix.Compiler.Tree.Evaluator.Builtins qualified as Reg
import Juvix.Compiler.Tree.Extra.Rep qualified as Reg
import Juvix.Data.Field

fromReg :: Reg.InfoTable -> Result
fromReg tab = mkResult $ run $ runLabelInfoBuilderWithNextId (Reg.nextSymbolId tab) $ do
  let startAddr :: Address = 2
  startSym <- freshSymbol
  endSym <- freshSymbol
  let startName :: Text = "__juvix_start"
      startLab = LabelRef startSym (Just startName)
      endName :: Text = "__juvix_end"
      endLab = LabelRef endSym (Just endName)
  registerLabelName startSym startName
  registerLabelAddress startSym startAddr
  let mainSym = fromJust $ tab ^. Reg.infoMainFunction
      mainInfo = Reg.lookupTabFunInfo tab mainSym
      mainName = mainInfo ^. Reg.functionName
      mainResultType = Reg.typeTarget (mainInfo ^. Reg.functionType)
      mainArgs = getInputArgs (mainInfo ^. Reg.functionArgsNum) (mainInfo ^. Reg.functionArgNames)
      bnum = toOffset builtinsNum
      callStartInstr = mkCallRel (Lab startLab)
      initBuiltinsInstr = mkAssignAp (Binop $ BinopValue FieldAdd (MemRef Fp (-2)) (Imm 1))
      callMainInstr = mkCallRel (Lab $ LabelRef mainSym (Just mainName))
      jmpEndInstr = mkJumpRel (Val $ Lab endLab)
      loadInputArgsInstrs = concat $ reverse $ map mkLoadInputArg mainArgs
      -- [ap] = [[ap - 2 - k] + k]; ap++
      bltsRet = map (\k -> mkAssignAp (Load $ LoadValue (MemRef Ap (-2 - k)) k)) [0 .. bnum - 1]
      resRetInstrs = mkResultInstrs bnum mainResultType
      pinstrs =
        callStartInstr
          : jmpEndInstr
          : Label startLab
          : initBuiltinsInstr
          : loadInputArgsInstrs
          ++ callMainInstr
          : bltsRet
          ++ resRetInstrs
          ++ [Return]
  (blts, binstrs) <- addStdlibBuiltins (length pinstrs)
  let cinstrs = concatMap (mkFunCall . fst) $ sortOn snd $ HashMap.toList (info ^. Reg.extraInfoFUIDs)
  (addr, instrs) <- second (concat . reverse) <$> foldM (goFun blts endLab) (length pinstrs + length binstrs + length cinstrs, []) (tab ^. Reg.infoFunctions)
  massert (addr == length instrs + length cinstrs + length binstrs + length pinstrs)
  registerLabelName endSym endName
  registerLabelAddress endSym addr
  return
    ( length resRetInstrs,
      allElements,
      pinstrs
        ++ binstrs
        ++ cinstrs
        ++ instrs
        ++ [Label endLab]
    )
  where
    mkResult :: (LabelInfo, (Int, [Builtin], Code)) -> Result
    mkResult (labi, (outSize, blts, code)) =
      Result
        { _resultLabelInfo = labi,
          _resultCode = code,
          _resultBuiltins = blts,
          _resultOutputSize = outSize
        }

    mkResultInstrs :: Offset -> Reg.Type -> [Instruction]
    mkResultInstrs off = \case
      Reg.TyInductive Reg.TypeInductive {..} -> goRecord _typeInductiveSymbol
      Reg.TyConstr Reg.TypeConstr {..} -> goRecord _typeConstrInductive
      _ -> [mkAssignAp (Val $ Ref $ MemRef Ap (-off - 1))]
      where
        goRecord :: Symbol -> [Instruction]
        goRecord sym = case indInfo ^. Reg.inductiveConstructors of
          [tag] -> case Reg.lookupTabConstrInfo tab tag of
            Reg.ConstructorInfo {..} ->
              map mkOutInstr [0 .. toOffset _constructorArgsNum - 1]
              where
                mkOutInstr :: Offset -> Instruction
                mkOutInstr i = mkAssignAp (Load $ LoadValue (MemRef Ap (-off - i - 1)) i)
          _ -> impossible
          where
            indInfo = Reg.lookupTabInductiveInfo tab sym

    mkLoadInputArg :: Text -> [Instruction]
    mkLoadInputArg arg = [Hint (HintInput arg), mkAssignAp (Val $ Ref $ MemRef Ap 0)]

    info :: Reg.ExtraInfo
    info = Reg.computeExtraInfo tab

    getInputArgs :: Int -> [Maybe Text] -> [Text]
    getInputArgs n argnames = zipWith fromMaybe args (argnames ++ repeat Nothing)
      where
        args :: [Text]
        args = if n == 1 then ["in"] else map (\k -> "in" <> show k) [1 .. n]

    mkFunCall :: Symbol -> [Instruction]
    mkFunCall sym =
      [ mkCallRel $ Lab $ LabelRef sym (Just $ quoteName $ Reg.lookupTabFunInfo tab sym ^. Reg.functionName),
        Return,
        Nop
      ]

    -- To make it convenient with relative jumps, Cairo constructor tag is `2 *
    -- tag + 1` where `tag` is the 0-based constructor number within the
    -- inductive type. Make sure this corresponds with the relative jump code in
    -- `goCase`.
    getTagId :: Tag -> Int
    getTagId tag =
      1 + 2 * fromJust (HashMap.lookup tag (info ^. Reg.extraInfoCIDs))

    unsupported :: Text -> a
    unsupported what = error ("Cairo backend: unsupported: " <> what)

    quoteName :: Text -> Text
    quoteName txt =
      foldr
        (uncurry Text.replace)
        txt
        [ ("$", "__dollar__"),
          (":", "__colon__"),
          ("@", "__at__"),
          ("ap", "__ap__"),
          ("fp", "__fp__")
        ]

    argsOffset :: Int
    argsOffset = 3

    ppVarComment :: Reg.VarRef -> Int -> Text
    ppVarComment var off = Reg.ppPrint (Reg.moduleFromInfoTable tab) var <> " is [fp + " <> show off <> "]"

    goFun :: forall r. (Member LabelInfoBuilder r) => StdlibBuiltins -> LabelRef -> (Address, [[Instruction]]) -> Reg.FunctionInfo -> Sem r (Address, [[Instruction]])
    goFun blts failLab (addr0, acc) funInfo = do
      let sym = funInfo ^. Reg.functionSymbol
          funName = quoteName (funInfo ^. Reg.functionName)
      registerLabelName sym funName
      registerLabelAddress sym addr0
      let lab = Label $ LabelRef sym (Just funName)
          block = funInfo ^. Reg.functionCode
          pre = [lab]
          addr1 = addr0 + length pre
          n = funInfo ^. Reg.functionArgsNum
      let vars =
            HashMap.fromList $
              map (\k -> (Reg.VarRef Reg.VarGroupArgs k Nothing, -argsOffset - k)) [0 .. n - 1]
      instrs <-
        fmap fst
          . runCasmBuilder addr1 vars (-argsOffset - n)
          . runOutputList
          $ goBlock blts failLab mempty Nothing block
      return (addr1 + length instrs, (pre ++ instrs) : acc)

    -- To ensure that memory is accessed sequentially at all times, we divide
    -- instructions into basic blocks. Within each basic block, the `ap` offset
    -- (i.e. how much `ap` increased since the start of the basic block) is
    -- known at each instruction, which allows to statically associate `fp`
    -- offsets (i.e. offsets relative to `fp`) to variables while still
    -- generating only sequential assignments to `[ap]` with increasing `ap`.
    -- When the `ap` offset can no longer be statically determined for new
    -- variables (e.g. due to an intervening recursive call), we switch to the
    -- next basic block by "calling" it with the `call` instruction (see
    -- `goCallBlock`). The arguments of the basic block call are the variables
    -- live at the beginning of the block. Note that the `fp` offsets of "old"
    -- variables are still statically determined even after the current `ap`
    -- offset becomes unknown -- the arbitrary increase of `ap` does not
    -- influence the previous variable associations.
    goBlock :: forall r. (Members '[LabelInfoBuilder, CasmBuilder, Output Instruction] r) => StdlibBuiltins -> LabelRef -> HashSet Reg.VarRef -> Maybe Reg.VarRef -> Reg.Block -> Sem r ()
    goBlock blts failLab liveVars0 mout Reg.Block {..} = do
      mapM_ goInstr _blockBody
      case _blockNext of
        Just block' -> do
          massert (isJust _blockFinal)
          goFinalInstr (block' ^. Reg.blockLiveVars) (fromJust _blockFinal)
          goBlock blts failLab liveVars0 mout block'
        Nothing -> case _blockFinal of
          Just instr ->
            goFinalInstr liveVars0 instr
          Nothing -> do
            massert (isJust mout)
            massert (HashSet.member (fromJust mout) liveVars0)
            goAssignApBuiltins
            whenJust mout saveLiveVar
            goCallBlock mout liveVars0
      where
        output'' :: Instruction -> Sem r ()
        output'' i = do
          output i
          incPC 1

        output' :: Int -> Instruction -> Sem r ()
        output' apOff i = do
          output'' i
          incAP apOff

        saveLiveVar :: Reg.VarRef -> Sem r ()
        saveLiveVar var = do
          ref <- mkMemRef var
          let comment = Reg.ppPrint (Reg.moduleFromInfoTable tab) var
          goAssignAp' (Just comment) (Val (Ref ref))

        -- The `goCallBlock` function is used to switch to a new basic block.
        -- Assumes that the builtins pointer and outVar (if present) were
        -- already saved (in this order).
        goCallBlock :: Maybe Reg.VarRef -> HashSet Reg.VarRef -> Sem r ()
        goCallBlock outVar liveVars = do
          let liveVars' = sort $ toList (maybe liveVars (`HashSet.delete` liveVars) outVar)
              n = length liveVars'
              bltOff = -argsOffset - n - fromEnum (isJust outVar)
              vars =
                HashMap.fromList $
                  maybe [] (\var -> [(var, -argsOffset - n)]) outVar
                    ++ zipWithExact (\var k -> (var, -argsOffset - k)) liveVars' [0 .. n - 1]
          mapM_ saveLiveVar (reverse liveVars')
          output'' (mkCallRel $ Imm 3)
          output'' Return
          -- we need the Nop instruction to ensure that the relative call offset
          -- (constant 3) in our CASM interpreter corresponds to the relative
          -- call offset in the Cairo binary representation
          output'' Nop
          setAP 0
          setVars vars
          setBuiltinOffset bltOff

        goLocalBlock :: Int -> HashMap Reg.VarRef Int -> Int -> HashSet Reg.VarRef -> Maybe Reg.VarRef -> Reg.Block -> Sem r ()
        goLocalBlock ap0 vars bltOff liveVars mout' block = do
          setAP ap0
          setVars vars
          setBuiltinOffset bltOff
          goBlock blts failLab liveVars mout' block

        ----------------------------------------------------------------------
        -- The mk* functions don't change the builder state, may only read it
        ----------------------------------------------------------------------

        mkConst :: Reg.Constant -> Integer
        mkConst = \case
          Reg.ConstInt x -> x
          Reg.ConstBool True -> 0
          Reg.ConstBool False -> 1
          Reg.ConstField f -> fieldToInteger f
          Reg.ConstUnit -> 0
          Reg.ConstVoid -> 0
          Reg.ConstString {} -> unsupported "strings"
          Reg.ConstUInt8 {} -> unsupported "uint8"
          Reg.ConstByteArray {} -> unsupported "bytearray"

        mkLoad :: Reg.ConstrField -> Sem r RValue
        mkLoad Reg.ConstrField {..} = do
          let tagOffset = if Reg.isTabConstrRecord tab _constrFieldTag then 0 else 1
          v <- mkMemRef _constrFieldRef
          return $ Load $ LoadValue v (toOffset _constrFieldIndex + tagOffset)

        mkMemRef :: Reg.VarRef -> Sem r MemRef
        mkMemRef vr = do
          v <- lookupVar' vr
          return $ MemRef Fp (toOffset v)

        mkBuiltinRef :: Sem r MemRef
        mkBuiltinRef = do
          off <- getBuiltinOffset
          return $ MemRef Fp (toOffset off)

        mkRValue :: Reg.Value -> Sem r RValue
        mkRValue = \case
          Reg.ValConst c -> return $ Val $ Imm $ mkConst c
          Reg.CRef x -> mkLoad x
          Reg.VRef x -> Val . Ref <$> mkMemRef x

        ---------------------------------------------------------------------
        -- Instruction
        ---------------------------------------------------------------------

        goInstr :: Reg.Instruction -> Sem r ()
        goInstr = \case
          Reg.Binop x -> goBinop x
          Reg.Unop x -> goUnop x
          Reg.Cairo x -> goCairo x
          Reg.Assign x -> goAssign x
          Reg.Alloc x -> goAlloc x
          Reg.AllocClosure x -> goAllocClosure x
          Reg.Assert x -> goAssert x
          Reg.Trace x -> goTrace x
          Reg.Dump -> unsupported "dump"
          Reg.Failure x -> goFail x

        goAssignVar :: Reg.VarRef -> RValue -> Sem r ()
        goAssignVar vr val = do
          off <- getAP
          insertVar vr off
          let comment = ppVarComment vr off
          goAssignAp' (Just comment) val

        goAssignAp' :: Maybe Text -> RValue -> Sem r ()
        goAssignAp' comment val = do
          output' 1 (mkAssignAp' comment val)

        goAssignAp :: RValue -> Sem r ()
        goAssignAp = goAssignAp' Nothing

        goAssignValue :: Reg.VarRef -> Reg.Value -> Sem r ()
        goAssignValue vr v = mkRValue v >>= goAssignVar vr

        goAssignApValue :: Reg.Value -> Sem r ()
        goAssignApValue v = mkRValue v >>= goAssignAp

        goAssignApBuiltins :: Sem r ()
        goAssignApBuiltins = mkBuiltinRef >>= goAssignAp' (Just "builtins pointer") . Val . Ref

        -- Warning: the result may depend on Ap. Use adjustAp when changing Ap
        -- afterwards.
        goValue :: Reg.Value -> Sem r Value
        goValue = \case
          Reg.ValConst c -> return $ Imm $ mkConst c
          Reg.CRef x -> do
            v <- mkLoad x
            goAssignAp v
            return $ Ref $ MemRef Ap (-1)
          Reg.VRef x -> do
            v <- Ref <$> mkMemRef x
            return v

        goExtraBinop :: ExtraOpcode -> Reg.VarRef -> MemRef -> Value -> Sem r ()
        goExtraBinop op res arg1 arg2 = do
          off <- getAP
          insertVar res off
          output' 1 $
            ExtraBinop
              InstrExtraBinop
                { _instrExtraBinopOpcode = op,
                  _instrExtraBinopResult = MemRef Ap 0,
                  _instrExtraBinopArg1 = arg1,
                  _instrExtraBinopArg2 = arg2,
                  _instrExtraBinopIncAp = True,
                  _instrExtraBinopComment = Just (ppVarComment res off)
                }

        goNativeBinop :: Opcode -> Reg.VarRef -> MemRef -> Value -> Sem r ()
        goNativeBinop op res arg1 arg2 = goAssignVar res binop
          where
            binop =
              Binop
                BinopValue
                  { _binopValueOpcode = op,
                    _binopValueArg1 = arg1,
                    _binopValueArg2 = arg2
                  }

        goEq :: Reg.VarRef -> MemRef -> Value -> Sem r ()
        goEq res arg1 arg2 = goExtraBinop FieldSub res arg1 arg2

        goIntLe :: Reg.VarRef -> MemRef -> Value -> Sem r ()
        goIntLe res arg1 arg2 = case arg2 of
          Imm v ->
            goExtraBinop IntLt res arg1 (Imm (v + 1))
          Ref mref -> do
            output' 1 inc
            goExtraBinop IntLt res (adjustAp 1 arg1) (Ref $ MemRef Ap (-1))
            where
              inc =
                Assign
                  InstrAssign
                    { _instrAssignResult = MemRef Ap 0,
                      _instrAssignValue =
                        Binop
                          BinopValue
                            { _binopValueArg1 = mref,
                              _binopValueArg2 = Imm 1,
                              _binopValueOpcode = FieldAdd
                            },
                      _instrAssignIncAp = True,
                      _instrAssignComment = Nothing
                    }
          Lab {} -> impossible

        goOpArgsNum :: Reg.VarRef -> MemRef -> Sem r ()
        goOpArgsNum res v = do
          goAssignAp (Val $ Imm $ fromIntegral casmMaxFunctionArgs + 1)
          goAssignAp (Load $ LoadValue (adjustAp 1 v) casmClosureArgsNumOffset)
          goExtraBinop FieldSub res (MemRef Ap (-2)) (Ref $ MemRef Ap (-1))

        goBinop' :: Reg.BinaryOp -> Reg.VarRef -> MemRef -> Value -> Sem r ()
        goBinop' op res arg1 arg2 = case op of
          Reg.OpIntAdd ->
            goExtraBinop IntAdd res arg1 arg2
          Reg.OpIntSub ->
            goExtraBinop IntSub res arg1 arg2
          Reg.OpIntMul ->
            goExtraBinop IntMul res arg1 arg2
          Reg.OpIntDiv ->
            goExtraBinop IntDiv res arg1 arg2
          Reg.OpIntMod ->
            goExtraBinop IntMod res arg1 arg2
          Reg.OpBool Reg.OpIntLt ->
            goExtraBinop IntLt res arg1 arg2
          Reg.OpBool Reg.OpIntLe ->
            goIntLe res arg1 arg2
          Reg.OpFieldAdd ->
            goNativeBinop FieldAdd res arg1 arg2
          Reg.OpFieldSub ->
            goExtraBinop FieldSub res arg1 arg2
          Reg.OpFieldMul ->
            goNativeBinop FieldMul res arg1 arg2
          Reg.OpFieldDiv ->
            goExtraBinop FieldDiv res arg1 arg2
          Reg.OpBool Reg.OpEq ->
            goEq res arg1 arg2
          Reg.OpStrConcat ->
            unsupported "strings"

        goBinop :: Reg.InstrBinop -> Sem r ()
        goBinop x@Reg.InstrBinop {..} = case _instrBinopArg1 of
          Reg.ValConst c1 -> case _instrBinopArg2 of
            Reg.ValConst c2 -> case Reg.evalBinop' _instrBinopOpcode c1 c2 of
              Left err -> error err
              Right c ->
                goAssignVar _instrBinopResult (Val $ Imm $ mkConst c)
            _
              | Reg.isCommutative _instrBinopOpcode ->
                  goBinop
                    x
                      { Reg._instrBinopArg1 = _instrBinopArg2,
                        Reg._instrBinopArg2 = _instrBinopArg1
                      }
              | otherwise -> do
                  goAssignAp (Val $ Imm $ mkConst c1)
                  v2 <- goValue _instrBinopArg2
                  case _instrBinopArg2 of
                    Reg.CRef {} -> do
                      goBinop' _instrBinopOpcode _instrBinopResult (MemRef Ap (-2)) v2
                    _ -> do
                      goBinop' _instrBinopOpcode _instrBinopResult (MemRef Ap (-1)) v2
          Reg.CRef ctr1 -> do
            v1 <- mkLoad ctr1
            goAssignAp v1
            v2 <- goValue _instrBinopArg2
            case _instrBinopArg2 of
              Reg.CRef {} -> do
                goBinop' _instrBinopOpcode _instrBinopResult (MemRef Ap (-2)) v2
              _ -> do
                goBinop' _instrBinopOpcode _instrBinopResult (MemRef Ap (-1)) v2
          Reg.VRef var1 -> do
            ref <- mkMemRef var1
            v2 <- goValue _instrBinopArg2
            goBinop' _instrBinopOpcode _instrBinopResult ref v2

        goUnop' :: (Reg.VarRef -> MemRef -> Sem r ()) -> Reg.VarRef -> Reg.Value -> Sem r ()
        goUnop' f res val = do
          v <- goValue val
          case v of
            Ref mr -> do
              f res mr
            Imm {} -> impossible
            Lab {} -> impossible

        goUnop :: Reg.InstrUnop -> Sem r ()
        goUnop Reg.InstrUnop {..} = case _instrUnopOpcode of
          Reg.OpShow -> unsupported "strings"
          Reg.OpStrToInt -> unsupported "strings"
          Reg.OpFieldToInt -> goAssignValue _instrUnopResult _instrUnopArg
          Reg.OpIntToField -> goAssignValue _instrUnopResult _instrUnopArg
          Reg.OpArgsNum -> goUnop' goOpArgsNum _instrUnopResult _instrUnopArg
          Reg.OpUInt8ToInt -> unsupported "OpUInt8ToInt"
          Reg.OpIntToUInt8 -> unsupported "OpIntToUInt8"

        goCairo :: Reg.InstrCairo -> Sem r ()
        goCairo Reg.InstrCairo {..} = case _instrCairoOpcode of
          Reg.OpCairoRandomEcPoint -> do
            off <- getAP
            insertVar _instrCairoResult off
            output'' (Hint HintRandomEcPoint)
            goAssignAp (Val $ Ref $ MemRef Ap 0)
          _ -> do
            goAssignApBuiltins
            mapM_ goAssignApValue (reverse _instrCairoArgs)
            output' apOff (mkCallRel (Lab (LabelRef sym (Just name))))
            off <- getAP
            insertVar _instrCairoResult (off - 1)
            setBuiltinOffset (off - 2)
            where
              (apOff, sym, name) = case _instrCairoOpcode of
                Reg.OpCairoPoseidon -> (blts ^. stdlibPoseidonApOffset, blts ^. stdlibPoseidon, blts ^. stdlibPoseidonName)
                Reg.OpCairoEc -> (blts ^. stdlibEcOpApOffset, blts ^. stdlibEcOp, blts ^. stdlibEcOpName)

        goAssign :: Reg.InstrAssign -> Sem r ()
        goAssign Reg.InstrAssign {..} =
          goAssignValue _instrAssignResult _instrAssignValue

        goAllocCall :: Reg.VarRef -> Sem r ()
        goAllocCall res = do
          output' (blts ^. stdlibGetRegsApOffset) $ mkCallRel $ Lab $ LabelRef (blts ^. stdlibGetRegs) (Just (blts ^. stdlibGetRegsName))
          goNativeBinop FieldAdd res (MemRef Ap (-2)) (Imm 3)

        goAlloc :: Reg.InstrAlloc -> Sem r ()
        goAlloc Reg.InstrAlloc {..} = do
          goAllocCall _instrAllocResult
          unless (Reg.isTabConstrRecord tab _instrAllocTag) $
            goAssignAp (Val $ Imm $ fromIntegral tagId)
          mapM_ goAssignApValue _instrAllocArgs
          where
            tagId = getTagId _instrAllocTag

        goAllocClosure :: Reg.InstrAllocClosure -> Sem r ()
        goAllocClosure Reg.InstrAllocClosure {..} = do
          goAllocCall _instrAllocClosureResult
          goAssignAp (Val $ Imm $ fromIntegral $ 1 + 3 * fuid)
          goAssignAp (Val $ Imm $ fromIntegral $ casmMaxFunctionArgs + 1 - storedArgsNum)
          goAssignAp (Val $ Imm $ fromIntegral $ casmMaxFunctionArgs + 1 - leftArgsNum)
          mapM_ goAssignApValue _instrAllocClosureArgs
          where
            fuid = fromJust $ HashMap.lookup _instrAllocClosureSymbol (info ^. Reg.extraInfoFUIDs)
            storedArgsNum = length _instrAllocClosureArgs
            leftArgsNum = _instrAllocClosureExpectedArgsNum - storedArgsNum

        goAssert :: Reg.InstrAssert -> Sem r ()
        goAssert Reg.InstrAssert {..} = do
          v <- goValue _instrAssertValue
          case v of
            Imm c
              | c == 0 -> return ()
              | otherwise ->
                  output' 0 $ mkAssign (MemRef Ap 0) (Binop $ BinopValue FieldAdd (MemRef Ap 0) (Imm 1))
            Ref r ->
              output' 0 $ Assert (InstrAssert r)
            Lab {} -> unsupported "assert label"

        goTrace :: Reg.InstrTrace -> Sem r ()
        goTrace Reg.InstrTrace {..} = do
          v <- mkRValue _instrTraceValue
          output' 0 $ Trace (InstrTrace v)

        goFail :: Reg.InstrFailure -> Sem r ()
        goFail Reg.InstrFailure {..} = do
          v <- mkRValue _instrFailureValue
          output' 0 $ Trace (InstrTrace v)
          output' 0 $ mkJumpRel (Val $ Lab failLab)

        ---------------------------------------------------------------------
        -- FinalInstruction
        ---------------------------------------------------------------------

        goFinalInstr :: HashSet Reg.VarRef -> Reg.FinalInstruction -> Sem r ()
        goFinalInstr liveVars = \case
          Reg.ExtendClosure x -> goExtendClosure liveVars x
          Reg.Call x -> goCall liveVars x
          Reg.TailCall x -> goTailCall x
          Reg.Return x -> goReturn x
          Reg.If x -> goIf liveVars x
          Reg.Branch x -> goBranch liveVars x
          Reg.Case x -> goCase liveVars x

        goExtendClosure :: HashSet Reg.VarRef -> Reg.InstrExtendClosure -> Sem r ()
        goExtendClosure liveVars Reg.InstrExtendClosure {..} = do
          mapM_ goAssignApValue _instrExtendClosureArgs
          goAssignAp (Val $ Imm $ fromIntegral $ length _instrExtendClosureArgs)
          val <- mkMemRef _instrExtendClosureValue
          goAssignAp (Val $ Ref val)
          output'' $ mkCallRel $ Lab $ LabelRef (blts ^. stdlibExtendClosure) (Just (blts ^. stdlibExtendClosureName))
          -- the `juvix_extend_closure` runtime function does not accept or
          -- return the builtins pointer
          goAssignApBuiltins
          goAssignAp (Val $ Ref $ MemRef Ap (-2))
          goCallBlock (Just _instrExtendClosureResult) liveVars

        goCall' :: Reg.CallType -> [Reg.Value] -> Sem r ()
        goCall' ct args = case ct of
          Reg.CallFun sym -> do
            goAssignApBuiltins
            mapM_ goAssignApValue (reverse args)
            output'' $ mkCallRel $ Lab $ LabelRef sym (Just funName)
            where
              funName = quoteName (Reg.lookupTabFunInfo tab sym ^. Reg.functionName)
          Reg.CallClosure cl -> do
            goAssignApBuiltins
            mapM_ goAssignApValue (reverse args)
            r <- mkMemRef cl
            goAssignAp (Val $ Ref r)
            output'' $ mkCallRel $ Lab $ LabelRef (blts ^. stdlibCallClosure) (Just (blts ^. stdlibCallClosureName))

        goCall :: HashSet Reg.VarRef -> Reg.InstrCall -> Sem r ()
        goCall liveVars Reg.InstrCall {..} = do
          goCall' _instrCallType _instrCallArgs
          goCallBlock (Just _instrCallResult) liveVars

        -- There is no way to make "proper" tail calls in Cairo, because
        -- the only way to set the `fp` register is via the `call` instruction.
        -- So we just translate tail calls into `call` followed by `ret`.
        goTailCall :: Reg.InstrTailCall -> Sem r ()
        goTailCall Reg.InstrTailCall {..} = do
          goCall' _instrTailCallType _instrTailCallArgs
          output'' Return

        goReturn :: Reg.InstrReturn -> Sem r ()
        goReturn Reg.InstrReturn {..} = do
          goAssignApBuiltins
          goAssignApValue _instrReturnValue
          output'' Return

        goIf :: HashSet Reg.VarRef -> Reg.InstrIf -> Sem r ()
        goIf liveVars Reg.InstrIf {..} = case _instrIfOp of
          Reg.OpEq
            | Reg.ValConst (Reg.ConstInt 0) <- _instrIfArg1 -> do
                v <- goValue _instrIfArg2
                goBranch' liveVars _instrIfOutVar _instrIfTrue _instrIfFalse v
            | Reg.ValConst (Reg.ConstInt 0) <- _instrIfArg2 -> do
                v <- goValue _instrIfArg1
                goBranch' liveVars _instrIfOutVar _instrIfTrue _instrIfFalse v
          _ -> impossible

        goBranch :: HashSet Reg.VarRef -> Reg.InstrBranch -> Sem r ()
        goBranch liveVars Reg.InstrBranch {..} = do
          v <- goValue _instrBranchValue
          goBranch' liveVars _instrBranchOutVar _instrBranchTrue _instrBranchFalse v

        goBranch' :: HashSet Reg.VarRef -> Maybe Reg.VarRef -> Reg.Block -> Reg.Block -> Value -> Sem r ()
        goBranch' liveVars outVar branchTrue branchFalse = \case
          Imm c
            | c == 0 -> goBlock blts failLab liveVars outVar branchTrue
            | otherwise -> goBlock blts failLab liveVars outVar branchFalse
          Ref r -> do
            symFalse <- freshSymbol
            symEnd <- freshSymbol
            let labFalse = LabelRef symFalse Nothing
                labEnd = LabelRef symEnd Nothing
            output'' $ mkJumpIf (Lab labFalse) r
            ap0 <- getAP
            vars <- getVars
            bltOff <- getBuiltinOffset
            goLocalBlock ap0 vars bltOff liveVars outVar branchTrue
            -- outVar is Nothing iff the branch returns
            when (isJust outVar) $
              output'' (mkJumpRel (Val $ Lab labEnd))
            addrFalse <- getPC
            registerLabelAddress symFalse addrFalse
            output'' $ Label labFalse
            goLocalBlock ap0 vars bltOff liveVars outVar branchFalse
            addrEnd <- getPC
            registerLabelAddress symEnd addrEnd
            output'' $ Label labEnd
          Lab {} -> impossible

        goLoad :: Reg.Value -> Offset -> Sem r RValue
        goLoad val off = do
          v <- mkRValue val
          case v of
            Val (Ref r) -> return $ Load $ LoadValue r off
            _ -> do
              goAssignAp v
              return $ Load $ LoadValue (MemRef Ap (-1)) off

        goCase :: HashSet Reg.VarRef -> Reg.InstrCase -> Sem r ()
        goCase liveVars Reg.InstrCase {..} = do
          massert (not (Reg.isTabInductiveRecord tab _instrCaseInductive))
          syms <- replicateM (length tags) freshSymbol
          symEnd <- freshSymbol
          let symMap = HashMap.fromList $ zip tags syms
              labs = map (`LabelRef` Nothing) syms
              labEnd = LabelRef symEnd Nothing
              jmps = map (mkJumpRel . Val . Lab) labs
              -- we need the Nop instructions to ensure that the relative jump
              -- offsets in our CASM interpreter correspond to the relative jump
              -- offsets in the Cairo binary representation
              jmps' = concatMap (\i -> [i, Nop]) jmps
          v <- goLoad _instrCaseValue 0
          output'' (mkJumpRel v)
          mapM_ output'' jmps'
          ap0 <- getAP
          vars <- getVars
          bltOff <- getBuiltinOffset
          -- reversing `_instrCaseBranches` typically results in better
          -- opportunities for peephole optimization (the last jump to branch
          -- may be removed by the peephole optimizer)
          mapM_ (goCaseBranch ap0 vars bltOff symMap labEnd) (reverse _instrCaseBranches)
          mapM_ (goDefaultLabel symMap) defaultTags
          whenJust _instrCaseDefault $
            goLocalBlock ap0 vars bltOff liveVars _instrCaseOutVar
          addrEnd <- getPC
          registerLabelAddress symEnd addrEnd
          output'' $ Label labEnd
          where
            tags = Reg.lookupTabInductiveInfo tab _instrCaseInductive ^. Reg.inductiveConstructors
            ctrTags = HashSet.fromList $ map (^. Reg.caseBranchTag) _instrCaseBranches
            defaultTags = filter (not . flip HashSet.member ctrTags) tags

            goCaseBranch :: Int -> HashMap Reg.VarRef Int -> Int -> HashMap Tag Symbol -> LabelRef -> Reg.CaseBranch -> Sem r ()
            goCaseBranch ap0 vars bltOff symMap labEnd Reg.CaseBranch {..} = do
              let sym = fromJust $ HashMap.lookup _caseBranchTag symMap
                  lab = LabelRef sym Nothing
              addr <- getPC
              registerLabelAddress sym addr
              output'' $ Label lab
              goLocalBlock ap0 vars bltOff liveVars _instrCaseOutVar _caseBranchCode
              -- _instrCaseOutVar is Nothing iff the branch returns
              when (isJust _instrCaseOutVar) $
                output'' (mkJumpRel (Val $ Lab labEnd))

            goDefaultLabel :: HashMap Tag Symbol -> Reg.Tag -> Sem r ()
            goDefaultLabel symMap tag = do
              let sym = fromJust $ HashMap.lookup tag symMap
                  lab = LabelRef sym Nothing
              addr <- getPC
              registerLabelAddress sym addr
              output'' $ Label lab
