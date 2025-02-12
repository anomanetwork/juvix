module Juvix.Compiler.Asm.Translation.FromSource
  ( module Juvix.Compiler.Asm.Translation.FromSource,
    module Juvix.Parser.Error,
    BuilderState,
  )
where

import Control.Monad.Trans.Class (lift)
import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Asm.Data.InfoTableBuilder
import Juvix.Compiler.Asm.Data.Module
import Juvix.Compiler.Asm.Extra.Base
import Juvix.Compiler.Asm.Language
import Juvix.Compiler.Asm.Translation.FromSource.Lexer
import Juvix.Compiler.Tree.Translation.FromSource.Base
import Juvix.Compiler.Tree.Translation.FromSource.Sig qualified as S
import Juvix.Extra.Paths
import Juvix.Parser.Error
import Text.Megaparsec qualified as P

type ParserSig = S.ParserSig Code (Maybe FunctionInfoExtra) DirectRef

type LocalParams = LocalParams' DirectRef

parseAsmSig :: ParserSig
parseAsmSig =
  S.ParserSig
    { _parserSigBareIdentifier = bareIdentifier,
      _parserSigParseCode = parseCode,
      _parserSigArgRef = \x y -> ArgRef (OffsetRef x y),
      _parserSigEmptyCode = [],
      _parserSigEmptyExtra = mempty
    }

parseText :: Text -> Either MegaparsecError Module
parseText = runParser noFile

parseText' :: BuilderState -> Text -> Either MegaparsecError BuilderState
parseText' bs = runParser' bs noFile

runParser :: Path Abs File -> Text -> Either MegaparsecError Module
runParser = runParserS parseAsmSig

runParser' :: BuilderState -> Path Abs File -> Text -> Either MegaparsecError BuilderState
runParser' = runParserS' parseAsmSig

parseCode ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  ParsecS r Code
parseCode = P.sepEndBy command (kw delimSemicolon)

command ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  ParsecS r Command
command = do
  off <- P.getOffset
  (txt, i) <- identifierL
  let loc = Just i
  case txt of
    "add" ->
      return $ mkBinop' loc OpIntAdd
    "sub" ->
      return $ mkBinop' loc OpIntSub
    "mul" ->
      return $ mkBinop' loc OpIntMul
    "div" ->
      return $ mkBinop' loc OpIntDiv
    "mod" ->
      return $ mkBinop' loc OpIntMod
    "lt" ->
      return $ mkBinop' loc (OpBool OpIntLt)
    "le" ->
      return $ mkBinop' loc (OpBool OpIntLe)
    "eq" ->
      return $ mkBinop' loc (OpBool OpEq)
    "fadd" ->
      return $ mkBinop' loc OpFieldAdd
    "fsub" ->
      return $ mkBinop' loc OpFieldSub
    "fmul" ->
      return $ mkBinop' loc OpFieldMul
    "fdiv" ->
      return $ mkBinop' loc OpFieldDiv
    "strcat" ->
      return $ mkBinop' loc OpStrConcat
    "show" ->
      return $ mkUnop' loc OpShow
    "atoi" ->
      return $ mkUnop' loc OpStrToInt
    "push" ->
      mkInstr' loc . Push <$> value
    "pop" ->
      return $ mkInstr' loc Pop
    "assert" ->
      return $ mkInstr' loc Assert
    "trace" ->
      return $ mkInstr' loc Trace
    "dump" ->
      return $ mkInstr' loc Dump
    "fail" ->
      return $ mkInstr' loc Failure
    "argsnum" ->
      return $ mkUnop' loc OpArgsNum
    "poseidon" ->
      return $ mkCairo' loc OpCairoPoseidon
    "ec_op" ->
      return $ mkCairo' loc OpCairoEc
    "random_ec_point" ->
      return $ mkCairo' loc OpCairoRandomEcPoint
    "alloc" ->
      mkInstr' loc . AllocConstr <$> constrTag @Code @(Maybe FunctionInfoExtra) @DirectRef
    "calloc" ->
      mkInstr' loc . AllocClosure <$> instrAllocClosure
    "cextend" ->
      mkInstr' loc . ExtendClosure <$> instrExtendClosure
    "call" ->
      mkInstr' loc . Call <$> instrCall
    "tcall" ->
      mkInstr' loc . TailCall <$> instrCall
    "ccall" ->
      mkInstr' loc . CallClosures <$> instrCallClosures
    "tccall" ->
      mkInstr' loc . TailCallClosures <$> instrCallClosures
    "ret" ->
      return $ mkInstr' loc Return
    "br" -> do
      lbrace
      br1 <- trueBranch
      br2 <- falseBranch
      rbrace
      return $ Branch $ CmdBranch (CommandInfo loc) br1 br2
    "case" -> do
      sym <- indSymbol @Code @(Maybe FunctionInfoExtra) @DirectRef
      lbrace
      brs <- P.many caseBranch
      def <- optional defaultBranch
      rbrace
      return $ Case (CmdCase (CommandInfo loc) sym brs def)
    "save" ->
      parseSave loc False
    "tsave" ->
      parseSave loc True
    _ ->
      parseFailure off ("unknown instruction: " ++ fromText txt)

parseSave ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  Maybe Interval ->
  Bool ->
  ParsecS r Command
parseSave loc isTail = do
  mn <- optional identifier
  tmpNum <- lift $ gets @LocalParams (^. localParamsTempIndex)
  let updateNames :: LocalNameMap DirectRef -> LocalNameMap DirectRef
      updateNames mp = maybe mp (\n -> HashMap.insert n (mkTempRef (OffsetRef tmpNum (Just n))) mp) mn
  c <- braces (localS @LocalParams (over localParamsTempIndex (+ 1)) $ localS @LocalParams (over localParamsNameMap updateNames) parseCode)
  return $
    Save
      ( CmdSave
          { _cmdSaveInfo = CommandInfo loc,
            _cmdSaveIsTail = isTail,
            _cmdSaveCode = c,
            _cmdSaveName = mn
          }
      )

value ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  ParsecS r Value
value = (Constant <$> constant) <|> (Ref <$> memRef @Code @(Maybe FunctionInfoExtra))

instrAllocClosure ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  ParsecS r InstrAllocClosure
instrAllocClosure = do
  sym <- funSymbol @Code @(Maybe FunctionInfoExtra) @DirectRef
  argsNum <- (^. withLocParam) <$> integer
  return $ InstrAllocClosure sym (fromInteger argsNum)

instrExtendClosure :: ParsecS r InstrExtendClosure
instrExtendClosure = do
  argsNum <- (^. withLocParam) <$> integer
  return $ InstrExtendClosure (fromInteger argsNum)

instrCall ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  ParsecS r InstrCall
instrCall = do
  ct <- parseCallType
  argsNum <- case ct of
    CallFun sym -> do
      fi <- lift $ getFunctionInfo sym
      return (fi ^. functionArgsNum)
    CallClosure -> do
      n <- (^. withLocParam) <$> integer
      return (fromInteger n)
  return (InstrCall ct argsNum)

parseCallType ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  ParsecS r CallType
parseCallType = (kw kwDollar $> CallClosure) <|> (CallFun <$> funSymbol @Code @(Maybe FunctionInfoExtra) @DirectRef)

instrCallClosures :: ParsecS r InstrCallClosures
instrCallClosures = do
  argsNum <- (^. withLocParam) <$> integer
  return (InstrCallClosures (fromInteger argsNum))

branchCode ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  ParsecS r Code
branchCode = braces parseCode <|> (command >>= \x -> return [x])

trueBranch ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  ParsecS r Code
trueBranch = do
  symbol "true:"
  c <- branchCode
  kw delimSemicolon
  return c

falseBranch ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  ParsecS r Code
falseBranch = do
  symbol "false:"
  c <- branchCode
  kw delimSemicolon
  return c

caseBranch ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  ParsecS r CaseBranch
caseBranch = do
  tag <- P.try $ constrTag @Code @(Maybe FunctionInfoExtra) @DirectRef
  kw kwColon
  c <- CaseBranch tag <$> branchCode
  kw delimSemicolon
  return c

defaultBranch ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  ParsecS r Code
defaultBranch = do
  symbol "default:"
  c <- branchCode
  kw delimSemicolon
  return c
