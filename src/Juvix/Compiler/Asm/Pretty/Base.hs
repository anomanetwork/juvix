module Juvix.Compiler.Asm.Pretty.Base
  ( module Juvix.Compiler.Asm.Pretty.Base,
    module Juvix.Compiler.Asm.Pretty.Options,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Asm.Data.InfoTable
import Juvix.Compiler.Asm.Extra.Base
import Juvix.Compiler.Asm.Interpreter.Runtime
import Juvix.Compiler.Asm.Language
import Juvix.Compiler.Asm.Language.Type
import Juvix.Compiler.Asm.Pretty.Options
import Juvix.Compiler.Core.Pretty.Base qualified as Core
import Juvix.Data.CodeAnn

doc :: PrettyCode c => Options -> c -> Doc Ann
doc opts =
  run
    . runReader opts
    . ppCode

class PrettyCode c where
  ppCode :: Member (Reader Options) r => c -> Sem r (Doc Ann)

runPrettyCode :: PrettyCode c => Options -> c -> Doc Ann
runPrettyCode opts = run . runReader opts . ppCode

wrapCore ::
  forall r' c.
  Member (Reader Options) r' =>
  (forall r. Member (Reader Core.Options) r => c -> Sem r (Doc Ann)) ->
  c ->
  Sem r' (Doc Ann)
wrapCore f c = do
  opts <- ask
  return $ run $ runReader (toCoreOptions opts) $ f c

instance PrettyCode BuiltinDataTag where
  ppCode = wrapCore Core.ppCode

instance PrettyCode Tag where
  ppCode = wrapCore Core.ppCode

instance PrettyCode Constr where
  ppCode (Constr tag args) = do
    opts <- ask
    let tab = opts ^. optInfoTable
    n' <-
      maybe
        (ppCode tag)
        (\ci -> return $ annotate (AnnKind KNameConstructor) (pretty (ci ^. constructorName)))
        (HashMap.lookup tag (tab ^. infoConstrs))
    args' <- mapM (ppRightExpression appFixity) args
    return $ foldl' (<+>) n' args'

instance PrettyCode Closure where
  ppCode (Closure sym args) = do
    opts <- ask
    let tab = opts ^. optInfoTable
    n' <-
      maybe
        (return $ annotate (AnnKind KNameFunction) (pretty ("unnamed_function_" ++ show sym :: String)))
        (\fi -> return $ annotate (AnnKind KNameFunction) (pretty (fi ^. functionName)))
        (HashMap.lookup sym (tab ^. infoFunctions))
    args' <- mapM (ppRightExpression appFixity) args
    return $ foldl' (<+>) n' args'

instance PrettyCode Val where
  ppCode = \case
    ValInteger i ->
      return $ annotate AnnLiteralInteger (pretty i)
    ValBool True ->
      return $ annotate (AnnKind KNameConstructor) (pretty ("true" :: String))
    ValBool False ->
      return $ annotate (AnnKind KNameConstructor) (pretty ("false" :: String))
    ValString txt ->
      return $ annotate AnnLiteralString (pretty (show txt :: String))
    ValUnit {} ->
      return $ annotate (AnnKind KNameConstructor) (pretty ("unit" :: String))
    ValConstr c ->
      ppCode c
    ValClosure cl ->
      ppCode cl

instance PrettyCode TypeInductive where
  ppCode TypeInductive {..} = do
    opts <- ask
    let ii = getInductiveInfo (opts ^. optInfoTable) _typeInductiveSymbol
    return $ annotate (AnnKind KNameInductive) (pretty (ii ^. inductiveName))

instance PrettyCode TypeConstr where
  ppCode TypeConstr {..} = do
    opts <- ask
    let tab = opts ^. optInfoTable
    let ii = getInductiveInfo tab _typeConstrInductive
    let iname = annotate (AnnKind KNameInductive) (pretty (ii ^. inductiveName))
    let ci = getConstrInfo tab _typeConstrTag
    let cname = annotate (AnnKind KNameConstructor) (pretty (ci ^. constructorName))
    args <- mapM ppCode _typeConstrFields
    return $ iname <> kwColon <> cname <> encloseSep "(" ")" ", " args

instance PrettyCode Type where
  ppCode = \case
    TyDynamic ->
      return $ annotate (AnnKind KNameInductive) (pretty ("*" :: String))
    TyInteger {} ->
      return $ annotate (AnnKind KNameInductive) (pretty ("integer" :: String))
    TyBool {} ->
      return $ annotate (AnnKind KNameInductive) (pretty ("bool" :: String))
    TyString ->
      return $ annotate (AnnKind KNameInductive) (pretty ("string" :: String))
    TyUnit ->
      return $ annotate (AnnKind KNameInductive) (pretty ("unit" :: String))
    TyInductive x ->
      ppCode x
    TyConstr x ->
      ppCode x
    TyFun ty1 ty2 -> do
      l <- ppLeftExpression funFixity ty1
      r <- ppRightExpression funFixity ty2
      return $ l <+> kwArrow <+> r

{--------------------------------------------------------------------------------}
{- helper functions -}

parensIf :: Bool -> Doc Ann -> Doc Ann
parensIf t = if t then parens else id

ppRightExpression ::
  (PrettyCode a, HasAtomicity a, Member (Reader Options) r) =>
  Fixity ->
  a ->
  Sem r (Doc Ann)
ppRightExpression = ppLRExpression isRightAssoc

ppLeftExpression ::
  (PrettyCode a, HasAtomicity a, Member (Reader Options) r) =>
  Fixity ->
  a ->
  Sem r (Doc Ann)
ppLeftExpression = ppLRExpression isLeftAssoc

ppLRExpression ::
  (HasAtomicity a, PrettyCode a, Member (Reader Options) r) =>
  (Fixity -> Bool) ->
  Fixity ->
  a ->
  Sem r (Doc Ann)
ppLRExpression associates fixlr e =
  parensIf (atomParens associates (atomicity e) fixlr)
    <$> ppCode e
