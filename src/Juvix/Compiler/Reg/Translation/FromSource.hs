module Juvix.Compiler.Reg.Translation.FromSource
  ( module Juvix.Compiler.Reg.Translation.FromSource,
    module Juvix.Parser.Error,
    BuilderState,
  )
where

import Control.Monad.Trans.Class (lift)
import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Reg.Data.InfoTableBuilder
import Juvix.Compiler.Reg.Data.Module
import Juvix.Compiler.Reg.Language
import Juvix.Compiler.Reg.Translation.FromSource.Lexer
import Juvix.Compiler.Tree.Translation.FromSource.Base
import Juvix.Compiler.Tree.Translation.FromSource.Sig qualified as S
import Juvix.Extra.Paths
import Juvix.Parser.Error
import Text.Megaparsec qualified as P

type ParserSig = S.ParserSig Code () VarRef

type LocalParams = LocalParams' VarRef

parseRegSig :: ParserSig
parseRegSig =
  S.ParserSig
    { _parserSigBareIdentifier = bareIdentifier,
      _parserSigParseCode = parseCode,
      _parserSigArgRef = VarRef VarGroupArgs,
      _parserSigEmptyCode = [],
      _parserSigEmptyExtra = ()
    }

parseText :: Text -> Either MegaparsecError Module
parseText = runParser noFile

parseText' :: BuilderState -> Text -> Either MegaparsecError BuilderState
parseText' bs = runParser' bs noFile

runParser :: Path Abs File -> Text -> Either MegaparsecError Module
runParser = runParserS parseRegSig

runParser' :: BuilderState -> Path Abs File -> Text -> Either MegaparsecError BuilderState
runParser' = runParserS' parseRegSig

parseCode ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  ParsecS r Code
parseCode = P.sepEndBy instruction (kw delimSemicolon)

instruction ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  ParsecS r Instruction
instruction =
  (instrNop >> return Nop)
    <|> (Assert <$> instrAssert)
    <|> (Trace <$> instrTrace)
    <|> (instrDump >> return Dump)
    <|> (Failure <$> instrFailure)
    <|> (Prealloc <$> instrPrealloc)
    <|> (TailCall <$> instrTailCall)
    <|> (TailCallClosures <$> instrTailCallClosures)
    <|> (Return <$> instrReturn)
    <|> (If <$> instrIf)
    <|> (Branch <$> instrBranch)
    <|> (Case <$> instrCase)
    <|> (Block <$> instrBlock)
    <|> instrWithResult

instrWithResult ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  ParsecS r Instruction
instrWithResult = do
  vref <- declVarRef
  kw kwEq
  (Binop <$> instrBinop vref)
    <|> (Unop <$> instrUnop vref)
    <|> (Cairo <$> instrCairo vref)
    <|> (Alloc <$> instrAlloc vref)
    <|> (AllocClosure <$> instrAllocClosure vref)
    <|> (ExtendClosure <$> instrExtendClosure vref)
    <|> (Call <$> instrCall vref)
    <|> (CallClosures <$> instrCallClosures vref)
    <|> (Assign <$> instrAssign vref)

instrNop :: ParsecS r ()
instrNop = kw kwNop

instrBinop ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  VarRef ->
  ParsecS r InstrBinop
instrBinop vref =
  parseBinaryOp kwAdd_ OpIntAdd vref
    <|> parseBinaryOp kwSub_ OpIntSub vref
    <|> parseBinaryOp kwMul_ OpIntMul vref
    <|> parseBinaryOp kwDiv_ OpIntDiv vref
    <|> parseBinaryOp kwMod_ OpIntMod vref
    <|> parseBinaryOp kwLt_ (OpBool OpIntLt) vref
    <|> parseBinaryOp kwLe_ (OpBool OpIntLe) vref
    <|> parseBinaryOp kwEq_ (OpBool OpEq) vref
    <|> parseBinaryOp kwFieldAdd OpFieldAdd vref
    <|> parseBinaryOp kwFieldSub OpFieldSub vref
    <|> parseBinaryOp kwFieldMul OpFieldMul vref
    <|> parseBinaryOp kwFieldDiv OpFieldDiv vref
    <|> parseBinaryOp kwStrcat OpStrConcat vref

parseBinaryOp ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  Keyword ->
  BinaryOp ->
  VarRef ->
  ParsecS r InstrBinop
parseBinaryOp kwd op vref = do
  kw kwd
  arg1 <- value
  arg2 <- value
  return $
    InstrBinop
      { _instrBinopOpcode = op,
        _instrBinopResult = vref,
        _instrBinopArg1 = arg1,
        _instrBinopArg2 = arg2
      }

instrUnop ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  VarRef ->
  ParsecS r InstrUnop
instrUnop vref =
  parseUnaryOp kwShow OpShow vref
    <|> parseUnaryOp kwAtoi OpStrToInt vref
    <|> parseUnaryOp kwIntToField OpIntToField vref
    <|> parseUnaryOp kwFieldToInt OpFieldToInt vref
    <|> parseUnaryOp kwArgsNum OpArgsNum vref

parseUnaryOp ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  Keyword ->
  UnaryOp ->
  VarRef ->
  ParsecS r InstrUnop
parseUnaryOp kwd op vref = do
  kw kwd
  arg <- value
  return $
    InstrUnop
      { _instrUnopOpcode = op,
        _instrUnopResult = vref,
        _instrUnopArg = arg
      }

instrCairo ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  VarRef ->
  ParsecS r InstrCairo
instrCairo vref =
  parseCairoOp kwPoseidon OpCairoPoseidon vref
    <|> parseCairoOp kwEcOp OpCairoEc vref
    <|> parseCairoOp kwRandomEcPoint OpCairoRandomEcPoint vref

parseCairoOp ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  Keyword ->
  CairoOp ->
  VarRef ->
  ParsecS r InstrCairo
parseCairoOp kwd op vref = do
  kw kwd
  args <- replicateM (cairoOpArgsNum op) value
  return $
    InstrCairo
      { _instrCairoOpcode = op,
        _instrCairoResult = vref,
        _instrCairoArgs = args
      }

instrAssign ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  VarRef ->
  ParsecS r InstrAssign
instrAssign vref = do
  val <- value
  return
    InstrAssign
      { _instrAssignResult = vref,
        _instrAssignValue = val
      }

instrTrace ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  ParsecS r InstrTrace
instrTrace = do
  kw kwTrace
  val <- value
  return
    InstrTrace
      { _instrTraceValue = val
      }

instrAssert ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  ParsecS r InstrAssert
instrAssert = do
  kw kwAssert
  val <- value
  return
    InstrAssert
      { _instrAssertValue = val
      }

instrDump :: ParsecS r ()
instrDump = kw kwDump

instrFailure ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  ParsecS r InstrFailure
instrFailure = do
  kw kwFail
  val <- value
  return
    InstrFailure
      { _instrFailureValue = val
      }

instrPrealloc ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  ParsecS r InstrPrealloc
instrPrealloc = do
  kw kwPrealloc
  n <- int
  vars <- fromMaybe [] <$> optional liveVars
  return
    InstrPrealloc
      { _instrPreallocWordsNum = n,
        _instrPreallocLiveVars = vars
      }

instrAlloc ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  VarRef ->
  ParsecS r InstrAlloc
instrAlloc vref = do
  kw kwAlloc
  tag <- constrTag @Code @() @VarRef
  args <- parseArgs
  return
    InstrAlloc
      { _instrAllocResult = vref,
        _instrAllocTag = tag,
        _instrAllocArgs = args,
        _instrAllocMemRep = MemRepConstr
      }

instrAllocClosure ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  VarRef ->
  ParsecS r InstrAllocClosure
instrAllocClosure vref = do
  kw kwCAlloc
  sym <- funSymbol @Code @() @VarRef
  args <- parseArgs
  fi <- lift $ getFunctionInfo sym
  return
    InstrAllocClosure
      { _instrAllocClosureResult = vref,
        _instrAllocClosureSymbol = sym,
        _instrAllocClosureExpectedArgsNum = fi ^. functionArgsNum,
        _instrAllocClosureArgs = args
      }

instrExtendClosure ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  VarRef ->
  ParsecS r InstrExtendClosure
instrExtendClosure vref = do
  kw kwCExtend
  cl <- varRef
  args <- parseArgs
  return
    InstrExtendClosure
      { _instrExtendClosureResult = vref,
        _instrExtendClosureValue = cl,
        _instrExtendClosureArgs = args
      }

liveVars ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  ParsecS r [VarRef]
liveVars = do
  P.try (comma >> symbol "live:")
  parens (P.sepBy varRef comma)

outVar ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  ParsecS r VarRef
outVar = do
  P.try (comma >> symbol "out:")
  varRef

parseArgs ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  ParsecS r [Value]
parseArgs = do
  parens (P.sepBy value comma)

parseCallType ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  ParsecS r CallType
parseCallType =
  (CallFun <$> funSymbol @Code @() @VarRef)
    <|> (CallClosure <$> varRef)

instrCall ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  VarRef ->
  ParsecS r InstrCall
instrCall vref = do
  kw kwCall
  ct <- parseCallType
  args <- parseArgs
  vars <- fromMaybe [] <$> optional liveVars
  return
    InstrCall
      { _instrCallResult = vref,
        _instrCallType = ct,
        _instrCallArgs = args,
        _instrCallLiveVars = vars
      }

instrCallClosures ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  VarRef ->
  ParsecS r InstrCallClosures
instrCallClosures vref = do
  kw kwCCall
  val <- varRef
  args <- parseArgs
  vars <- fromMaybe [] <$> optional liveVars
  return
    InstrCallClosures
      { _instrCallClosuresResult = vref,
        _instrCallClosuresValue = val,
        _instrCallClosuresArgs = args,
        _instrCallClosuresLiveVars = vars
      }

instrTailCall ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  ParsecS r InstrTailCall
instrTailCall = do
  kw kwCallTail
  ct <- parseCallType
  args <- parseArgs
  return
    InstrTailCall
      { _instrTailCallType = ct,
        _instrTailCallArgs = args
      }

instrTailCallClosures ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  ParsecS r InstrTailCallClosures
instrTailCallClosures = do
  kw kwCCallTail
  val <- varRef
  args <- parseArgs
  return
    InstrTailCallClosures
      { _instrTailCallClosuresValue = val,
        _instrTailCallClosuresArgs = args
      }

instrReturn ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  ParsecS r InstrReturn
instrReturn = do
  kw kwRet
  val <- value
  return
    InstrReturn
      { _instrReturnValue = val
      }

parseBoolOp :: ParsecS r BoolOp
parseBoolOp =
  (kw kwLt_ >> return OpIntLt)
    <|> (kw kwLe_ >> return OpIntLe)
    <|> (kw kwEq_ >> return OpEq)

instrIf ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  ParsecS r InstrIf
instrIf = do
  kw kwIf
  op <- parseBoolOp
  arg1 <- value
  arg2 <- value
  var <- optional outVar
  (br1, br2) <- braces $ do
    symbol "true:"
    br1 <- braces parseCode
    kw delimSemicolon
    symbol "false:"
    br2 <- braces parseCode
    kw delimSemicolon
    return (br1, br2)
  return
    InstrIf
      { _instrIfOp = op,
        _instrIfArg1 = arg1,
        _instrIfArg2 = arg2,
        _instrIfTrue = br1,
        _instrIfFalse = br2,
        _instrIfOutVar = var
      }

instrBranch ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  ParsecS r InstrBranch
instrBranch = do
  kw kwBr
  val <- value
  var <- optional outVar
  (br1, br2) <- braces $ do
    symbol "true:"
    br1 <- braces parseCode
    kw delimSemicolon
    symbol "false:"
    br2 <- braces parseCode
    kw delimSemicolon
    return (br1, br2)
  return
    InstrBranch
      { _instrBranchValue = val,
        _instrBranchTrue = br1,
        _instrBranchFalse = br2,
        _instrBranchOutVar = var
      }

instrCase ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  ParsecS r InstrCase
instrCase = do
  kw kwCase
  sym <- brackets (indSymbol @Code @() @VarRef)
  val <- value
  var <- optional outVar
  lbrace
  brs <- many caseBranch
  def <- optional defaultBranch
  rbrace
  return
    InstrCase
      { _instrCaseValue = val,
        _instrCaseInductive = sym,
        _instrCaseIndRep = IndRepStandard,
        _instrCaseBranches = brs,
        _instrCaseDefault = def,
        _instrCaseOutVar = var
      }

caseBranch ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  ParsecS r CaseBranch
caseBranch = do
  tag <- P.try $ do
    tag <- constrTag @Code @() @VarRef
    kw kwColon
    return tag
  body <- braces parseCode
  kw delimSemicolon
  ci <- lift $ getConstructorInfo tag
  return
    CaseBranch
      { _caseBranchTag = tag,
        _caseBranchArgsNum = ci ^. constructorArgsNum,
        _caseBranchCode = body,
        _caseBranchMemRep = MemRepConstr
      }

defaultBranch ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  ParsecS r Code
defaultBranch = do
  symbol "default:"
  c <- braces parseCode
  kw delimSemicolon
  return c

instrBlock ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  ParsecS r InstrBlock
instrBlock = InstrBlock <$> braces parseCode

varRef ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  ParsecS r VarRef
varRef = varArg <|> varTmp <|> namedRef @Code @() @VarRef

declVarRef ::
  forall r.
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  ParsecS r VarRef
declVarRef = varArg <|> varTmp <|> namedRef' @Code @() @VarRef decl
  where
    decl :: Int -> Text -> ParsecS r VarRef
    decl _ txt = do
      idx <- lift $ gets @LocalParams (^. localParamsTempIndex)
      lift $ modify' @LocalParams (over localParamsTempIndex (+ 1))
      let vref = VarRef VarGroupLocal idx (Just txt)
      lift $ modify' (over localParamsNameMap (HashMap.insert txt vref))
      return vref

varArg :: ParsecS r VarRef
varArg = do
  kw kwArg
  off <- brackets int
  return
    VarRef
      { _varRefGroup = VarGroupArgs,
        _varRefIndex = off,
        _varRefName = Nothing
      }

varTmp :: ParsecS r VarRef
varTmp = do
  kw kwTmp
  off <- brackets int
  return
    VarRef
      { _varRefGroup = VarGroupLocal,
        _varRefIndex = off,
        _varRefName = Nothing
      }

value ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  ParsecS r Value
value = (ValConst <$> constant) <|> varOrConstrRef

varOrConstrRef ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  ParsecS r Value
varOrConstrRef = do
  r <- varRef
  (CRef <$> constrField r) <|> return (VRef r)

constrField ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  VarRef ->
  ParsecS r ConstrField
constrField ref = do
  dot
  tag <- constrTag @Code @() @VarRef
  off <- brackets int
  return
    ConstrField
      { _constrFieldTag = tag,
        _constrFieldMemRep = MemRepConstr,
        _constrFieldRef = ref,
        _constrFieldIndex = off
      }
