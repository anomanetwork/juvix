{-# LANGUAGE QuantifiedConstraints #-}

module Juvix.Compiler.Concrete.Print.Base
  ( module Juvix.Compiler.Concrete.Print.Base,
    module Juvix.Data.CodeAnn,
    module Juvix.Compiler.Concrete.Pretty.Options,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Data.List.NonEmpty.Extra qualified as NonEmpty
import Data.Map qualified as Map
import Juvix.Compiler.Concrete.Data.Scope.Base
import Juvix.Compiler.Concrete.Data.ScopedName qualified as S
import Juvix.Compiler.Concrete.Extra qualified as Concrete
import Juvix.Compiler.Concrete.Gen qualified as Gen
import Juvix.Compiler.Concrete.Keywords
import Juvix.Compiler.Concrete.Keywords qualified as Kw
import Juvix.Compiler.Concrete.Language.Base
import Juvix.Compiler.Concrete.Pretty.Options
import Juvix.Compiler.Pipeline.Loader.PathResolver.Data
import Juvix.Compiler.Pipeline.Loader.PathResolver.PackageInfo
import Juvix.Compiler.Store.Scoped.Language
import Juvix.Data.Ape.Base
import Juvix.Data.Ape.Print
import Juvix.Data.CodeAnn (Ann, CodeAnn (..), ppCodeAnn, ppStringLit)
import Juvix.Data.CodeAnn qualified as C
import Juvix.Data.CodeReference
import Juvix.Data.Effect.ExactPrint
import Juvix.Data.Keyword.All qualified as Kw
import Juvix.Data.NameKind
import Juvix.Extra.Strings qualified as Str
import Juvix.Prelude hiding ((<+>), (<+?>), (<?+>), (?<>))
import Juvix.Prelude.Pretty (annotate, pretty)
import Juvix.Prelude.Pretty qualified as P

--- An expression is `Top` if it is:
--- * immediately at the top of a function (clause) body (including in let
---   bindings) or a lambda body
--- * immediately inside parens or braces
--- * the body of a `Top` let expression,
--- * the body of a `Top` iterator,
--- * the else branch body of a `Top` if expression,
--- * the last branch body of a `Top` case expresssion.
data IsTop
  = Top
  | NotTop

type PrettyPrintingMaybe a = forall r. (Members '[ExactPrint, Reader Options] r) => a -> Maybe (Sem r ())

type PrettyPrinting a = forall r. (Members '[ExactPrint, Reader Options] r) => a -> Sem r ()

class PrettyPrint a where
  ppCode :: (Members '[ExactPrint, Reader Options] r) => a -> Sem r ()

instance PrettyPrint Keyword where
  ppCode p = noLoc . annotate ann . pretty $ p
    where
      ann = case p ^. keywordType of
        KeywordTypeDelimiter -> AnnDelimiter
        KeywordTypeKeyword -> AnnKeyword
        KeywordTypeJudoc -> AnnJudoc

instance PrettyPrint KeywordRef where
  ppCode p =
    morphemeM
      (getLoc p)
      ( annotated (C.kwTypeAnn (p ^. keywordRefKeyword . keywordType))
          . noLoc
          . pretty
          $ p
      )

docNoCommentsDefault :: (PrettyPrint c) => c -> Doc Ann
docNoCommentsDefault = docHelper Nothing defaultOptions

docNoComments :: (PrettyPrint c) => Options -> c -> Doc Ann
docNoComments = docHelper Nothing

docHelper :: (PrettyPrint c) => Maybe FileComments -> Options -> c -> Doc Ann
docHelper cs opts =
  run
    . execExactPrint cs
    . runReader opts
    . ppCode

docNoLoc :: (PrettyPrint c) => Options -> c -> Doc Ann
docNoLoc = docHelper Nothing

doc :: (PrettyPrint c, HasLoc c) => Options -> Maybe Comments -> c -> Doc Ann
doc opts cs x = docHelper (fileComments file <$> cs) opts x
  where
    file :: Path Abs File
    file = getLoc x ^. intervalFile

docDefault :: (PrettyPrint c, HasLoc c) => Maybe Comments -> c -> Doc Ann
docDefault cs = doc defaultOptions cs

ppModulePathType ::
  forall t s r.
  (SingI t, SingI s, Members '[ExactPrint, Reader Options] r) =>
  ModulePathType s t ->
  Sem r ()
ppModulePathType x = case sing :: SStage s of
  SParsed -> case sing :: SModuleIsTop t of
    SModuleLocal -> ppCode x
    SModuleTop -> ppCode x
  SScoped -> case sing :: SModuleIsTop t of
    SModuleLocal -> annSDef x (ppCode x)
    SModuleTop -> annSDef x (ppCode x)

ppSymbolType :: forall s. (SingI s) => PrettyPrinting (SymbolType s)
ppSymbolType = case sing :: SStage s of
  SParsed -> ppCode
  SScoped -> ppCode

ppModuleNameType :: forall s. (SingI s) => PrettyPrinting (ModuleNameType s)
ppModuleNameType = case sing :: SStage s of
  SParsed -> ppCode
  SScoped -> ppCode

ppIdentifierType :: forall s. (SingI s) => PrettyPrinting (IdentifierType s)
ppIdentifierType = case sing :: SStage s of
  SParsed -> ppCode
  SScoped -> ppCode

ppHoleType :: forall s. (SingI s) => PrettyPrinting (HoleType s)
ppHoleType = case sing :: SStage s of
  SParsed -> ppCode
  SScoped -> ppCode

ppPatternAtomIdenType :: forall s. (SingI s) => PrettyPrinting (PatternAtomIdenType s)
ppPatternAtomIdenType = case sing :: SStage s of
  SParsed -> ppCode
  SScoped -> ppCode

ppExpressionType :: forall s. (SingI s) => PrettyPrinting (ExpressionType s)
ppExpressionType = case sing :: SStage s of
  SParsed -> ppCode
  SScoped -> ppCode

ppTopExpressionType :: forall s. (SingI s) => PrettyPrinting (ExpressionType s)
ppTopExpressionType e = case sing :: SStage s of
  SParsed -> ppCode e
  SScoped -> case e of
    ExpressionLet l -> ppLet Top l
    ExpressionCase c -> ppCase Top c
    ExpressionIf i -> ppIf Top i
    ExpressionIterator i -> ppIterator Top i
    _ -> ppCode e

ppExpressionAtomType :: forall s. (SingI s) => PrettyPrinting (ExpressionType s)
ppExpressionAtomType = case sing :: SStage s of
  SParsed -> ppCodeAtom
  SScoped -> ppCodeAtom

ppPatternAtomType :: forall s. (SingI s) => PrettyPrinting (PatternAtomType s)
ppPatternAtomType = case sing :: SStage s of
  SParsed -> ppCodeAtom
  SScoped -> ppCodeAtom

ppPatternParensType :: forall s. (SingI s) => PrettyPrinting (PatternParensType s)
ppPatternParensType = case sing :: SStage s of
  SParsed -> ppCode
  SScoped -> ppCode

ppPatternAtType :: forall s. (SingI s) => PrettyPrinting (PatternAtType s)
ppPatternAtType = case sing :: SStage s of
  SParsed -> ppCode
  SScoped -> ppCode

ppAnyStage :: forall k. (forall s. (SingI s) => PrettyPrint (k s)) => PrettyPrinting (AnyStage k)
ppAnyStage (s :&: p) = case s of
  SParsed -> ppCode p
  SScoped -> ppCode p

instance PrettyPrint AbsModulePath where
  ppCode AbsModulePath {..} = do
    let absLocalPath' = ppCode <$> _absLocalPath
        absTopModulePath' = ppCode _absTopModulePath
    dotted (absTopModulePath' : absLocalPath')

instance PrettyPrint PatternBinding where
  ppCode PatternBinding {..} = do
    let n' = ppSymbolType _patternBindingName
        p' = ppCode _patternBindingPattern
    n' <> ppCode _patternBindingAtKw <> p'

instance (SingI s) => PrettyPrint (ListPattern s) where
  ppCode ListPattern {..} = do
    let l = ppCode _listpBracketL
        r = ppCode _listpBracketR
        e = case sing :: SStage s of
          SParsed -> ppBlockOrList _listpItems
          SScoped -> ppBlockOrList _listpItems
    grouped (align (l <> e <> r))

instance PrettyPrint Interval where
  ppCode = noLoc . pretty

instance PrettyPrint Int where
  ppCode = noLoc . pretty

instance PrettyPrint Void where
  ppCode = absurd

instance (SingI s) => PrettyPrint (NameItem s) where
  ppCode :: forall r. (Members '[ExactPrint, Reader Options] r) => NameItem s -> Sem r ()
  ppCode NameItem {..} = do
    let defaultVal = do
          d <- _nameItemDefault
          return (noLoc C.kwAssign <+> ppExpressionType (d ^. argDefaultValue))
        ppSym :: Maybe (SymbolType s) -> Sem r ()
        ppSym = \case
          Nothing -> ppCode Kw.kwWildcard
          Just s -> ppSymbolType s
    isImplicitDelims _nameItemImplicit (ppSym _nameItemSymbol)
      <> ppCode Kw.kwExclamation
      <> noLoc (pretty _nameItemIndex)
      <+> ppCode Kw.kwColon
      <+> ppExpressionType _nameItemType
      <+?> defaultVal

isImplicitDelims :: (Member ExactPrint r) => IsImplicit -> Sem r () -> Sem r ()
isImplicitDelims = \case
  Implicit -> braces
  ImplicitInstance -> doubleBraces
  Explicit -> parens

instance (SingI s) => PrettyPrint (NameBlock s) where
  ppCode :: forall r. (Members '[ExactPrint, Reader Options] r) => NameBlock s -> Sem r ()
  ppCode NameBlock {..} =
    isImplicitDelims _nameBlockImplicit
      . vsepSemicolon
      $ fmap ppCode _nameBlockItems

instance (PrettyPrint a, PrettyPrint b) => PrettyPrint (HashMap a b) where
  ppCode :: forall r. (Members '[ExactPrint, Reader Options] r) => HashMap a b -> Sem r ()
  ppCode m = do
    let ppAssoc :: (a, b) -> Sem r ()
        ppAssoc (k, v) =
          ppCode k
            <+> ppCode Kw.kwAssign
            <+> ppCode v
    braces (vsepSemicolon (map ppAssoc (HashMap.toList m)))

instance (SingI s) => PrettyPrint (RecordNameSignature s) where
  ppCode RecordNameSignature {..} = ppCode _recordNames

instance (PrettyPrint a, PrettyPrint b) => PrettyPrint (a, b) where
  ppCode (a, b) = tuple [ppCode a, ppCode b]

instance (SingI s) => PrettyPrint (NameSignature s) where
  ppCode NameSignature {..}
    | null _nameSignatureArgs = noLoc (pretty @Text "<empty name signature>")
    | otherwise = itemize . map ppCode $ _nameSignatureArgs

instance (SingI s) => PrettyPrint (WildcardConstructor s) where
  ppCode WildcardConstructor {..} = do
    let (l, r) = _wildcardConstructorDelims ^. unIrrelevant
    ppIdentifierType _wildcardConstructor
      <> ppCode _wildcardConstructorAtKw
      <> ppCode l
      <> ppCode r

instance (SingI s) => PrettyPrint (PatternAtom s) where
  ppCode = \case
    PatternAtomIden n -> ppPatternAtomIdenType n
    PatternAtomWildcard w -> ppCode w
    PatternAtomWildcardConstructor w -> ppCode w
    PatternAtomList l -> ppCode l
    PatternAtomEmpty {} -> parens (return ())
    PatternAtomParens p -> parens (ppPatternParensType p)
    PatternAtomBraces p -> braces (ppPatternParensType p)
    PatternAtomDoubleBraces p -> doubleBraces (ppPatternParensType p)
    PatternAtomAt p -> ppPatternAtType p
    PatternAtomRecord p -> ppCode p

instance (SingI s) => PrettyPrint (PatternAtoms s) where
  ppCode (PatternAtoms ps _) = hsep (ppCode <$> ps)

instance (SingI s) => PrettyPrint (ExpressionAtoms s) where
  ppCode ::
    forall r.
    (Members '[ExactPrint, Reader Options] r) =>
    ExpressionAtoms s ->
    Sem r ()
  ppCode as = go (toList $ as ^. expressionAtoms)
    where
      go :: [ExpressionAtom s] -> Sem r ()
      go = \case
        [] -> return ()
        [x] -> ppCode x
        (x : xs@(AtomRecordUpdate {} : _)) -> ppCode x >> go xs
        (x : xs) -> ppCode x >> space >> go xs

instance (SingI s) => PrettyPrint (Initializer s) where
  ppCode Initializer {..} = do
    let n = ppPatternParensType _initializerPattern
        e = ppExpressionType _initializerExpression
    n <+> ppCode _initializerAssignKw <+> e

instance (SingI s) => PrettyPrint (Range s) where
  ppCode Range {..} = do
    let n = ppPatternParensType _rangePattern
        e = ppExpressionType _rangeExpression
    n <+> ppCode _rangeInKw <+> e

ppIterator :: forall r s. (Members '[ExactPrint, Reader Options] r, SingI s) => IsTop -> Iterator s -> Sem r ()
ppIterator _isTop Iterator {..} = do
  let n = ppIdentifierType _iteratorName
      is = ppCode <$> _iteratorInitializers
      rngs = ppCode <$> _iteratorRanges
      is' = parens . oneLineOrNextNoIndent . hsepSemicolon <$> nonEmpty is
      rngs' = parens . oneLineOrNextNoIndent . hsepSemicolon <$> nonEmpty rngs
      b
        | _iteratorBodyBraces = space <> braces (blockIndent (ppTopExpressionType _iteratorBody))
        | otherwise = parens (oneLineOrNextNoIndent (ppTopExpressionType _iteratorBody))
  parensIf _iteratorParens $
    n <>? is' <>? rngs' <> b

instance PrettyPrint S.AName where
  ppCode n = annotated (AnnKind (S.getNameKind n)) (noLoc (pretty (n ^. S.anameVerbatim)))

instance (SingI s) => PrettyPrint (List s) where
  ppCode List {..} = do
    let l = ppCode _listBracketL
        r = ppCode _listBracketR
        es = case sing :: SStage s of
          SParsed -> ppBlockOrList _listItems
          SScoped -> ppBlockOrList _listItems
    grouped (align (l <> es <> r))

instance (SingI s) => PrettyPrint (NamedArgumentAssign s) where
  ppCode NamedArgumentAssign {..} = do
    let s = ppSymbolType _namedArgName
        kwassign = ppCode _namedArgAssignKw
        val = ppExpressionType _namedArgValue
    s <+> kwassign <+> val

instance (SingI s) => PrettyPrint (ArgumentBlock s) where
  ppCode ArgumentBlock {..} = do
    let args' = ppCode <$> _argBlockArgs
        (l, r) = case d of
          Nothing -> (enqueue C.kwParenL, noLoc C.kwParenR)
          Just (l', r') -> (ppCode l', ppCode r')
    l <> align (sepSemicolon args') <> r
    where
      Irrelevant d = _argBlockDelims

instance (SingI s) => PrettyPrint (NamedApplication s) where
  ppCode NamedApplication {..} = do
    let args'
          | null _namedApplicationArguments = mempty
          | otherwise = ppBlock _namedApplicationArguments
    grouped
      ( align
          ( ppIdentifierType _namedApplicationName
              <> ppCode _namedApplicationAtKw
              <> braces args'
          )
      )

instance (SingI s) => PrettyPrint (NamedArgumentFunctionDef s) where
  ppCode (NamedArgumentFunctionDef f) = ppCode f

instance PrettyPrint (RecordUpdatePun s) where
  ppCode = ppCode . (^. recordUpdatePunSymbol)

instance PrettyPrint (NamedArgumentPun s) where
  ppCode = ppCode . (^. namedArgumentPunSymbol)

instance (SingI s) => PrettyPrint (NamedArgument s) where
  ppCode = \case
    NamedArgumentFunction f -> ppCode f
    NamedArgumentItemPun f -> ppCode f

instance (SingI s) => PrettyPrint (RecordSyntaxDef s) where
  ppCode = \case
    RecordSyntaxOperator d -> ppCode d
    RecordSyntaxIterator d -> ppCode d

instance (SingI s) => PrettyPrint (RecordStatement s) where
  ppCode = \case
    RecordStatementField f -> ppCode f
    RecordStatementSyntax f -> ppCode f

instance (SingI s) => PrettyPrint (RecordUpdateFieldItemAssign s) where
  ppCode RecordUpdateFieldItemAssign {..} =
    ppSymbolType _fieldUpdateName <+> ppCode _fieldUpdateAssignKw <+> ppExpressionType _fieldUpdateValue

instance (SingI s) => PrettyPrint (RecordUpdateField s) where
  ppCode = \case
    RecordUpdateFieldAssign a -> ppCode a
    RecordUpdateFieldPun a -> ppCode a

instance (SingI s) => PrettyPrint (RecordUpdate s) where
  ppCode RecordUpdate {..} = do
    let Irrelevant (l, r) = _recordUpdateDelims
        fields'
          | [f] <- _recordUpdateFields = ppCode f
          | otherwise = ppBlockOrList _recordUpdateFields
    ppCode _recordUpdateAtKw
      <> ppIdentifierType _recordUpdateTypeName
      <> ppCode l
      <> fields'
      <> ppCode r

instance (SingI s) => PrettyPrint (DoubleBracesExpression s) where
  ppCode DoubleBracesExpression {..} = do
    let (l, r) = _doubleBracesDelims ^. unIrrelevant
    ppCode l <> ppTopExpressionType _doubleBracesExpression <> ppCode r

instance (SingI s) => PrettyPrint (DoLet s) where
  ppCode DoLet {..} = do
    let letFunDefs' = ppBlock _doLetStatements
    ppCode _doLetKw
      <> letFunDefs'
      <> ppCode _doLetInKw

instance (SingI s) => PrettyPrint (DoBind s) where
  ppCode DoBind {..} = do
    ppPatternParensType _doBindPattern
      <+> ppCode _doBindArrowKw
      <+> ppTopExpressionType _doBindExpression

instance (Foldable l, SingI s) => PrettyPrint (l (DoStatement s)) where
  ppCode :: forall r. (Members '[ExactPrint, Reader Options] r) => l (DoStatement s) -> Sem r ()
  ppCode = vsepHard . map go . toList
    where
      go :: DoStatement s -> Sem r ()
      go = \case
        DoStatementBind b -> ppCode b >> semicolon
        DoStatementExpression b -> ppExpressionType b >> semicolon
        DoStatementLet b -> ppCode b

instance (SingI s) => PrettyPrint (DoStatement s) where
  ppCode = \case
    DoStatementExpression e -> ppExpressionType e
    DoStatementLet l -> ppCode l
    DoStatementBind l -> ppCode l

instance (SingI s) => PrettyPrint (Do s) where
  ppCode Do {..} = do
    let (openbr, closebr) = over both ppCode (_doDelims ^. unIrrelevant)
    ppCode _doKeyword
      <+> openbr
        <> hardline
        <> indent (ppCode _doStatements)
        <> hardline
        <> closebr

instance (SingI s) => PrettyPrint (ExpressionAtom s) where
  ppCode = \case
    AtomIdentifier n -> ppIdentifierType n
    AtomLambda l -> ppCode l
    AtomDo l -> ppCode l
    AtomLet lb -> ppLet NotTop lb
    AtomCase c -> ppCase NotTop c
    AtomIf c -> ppIf NotTop c
    AtomList l -> ppCode l
    AtomUniverse uni -> ppCode uni
    AtomRecordUpdate u -> ppCode u
    AtomFunction fun -> ppCode fun
    AtomLiteral lit -> ppCode lit
    AtomFunArrow a -> ppCode a
    AtomParens e -> parens (ppTopExpressionType e)
    AtomDoubleBraces e -> ppCode e
    AtomBraces e -> braces (ppTopExpressionType (e ^. withLocParam))
    AtomHole w -> ppHoleType w
    AtomInstanceHole w -> ppHoleType w
    AtomIterator i -> ppIterator NotTop i
    AtomNamedApplication i -> ppCode i

instance PrettyPrint PatternScopedIden where
  ppCode = \case
    PatternScopedVar v -> ppCode v
    PatternScopedConstructor c -> ppCode c

instance PrettyPrint Hole where
  ppCode h = do
    let uid = h ^. holeId
    withNameIdSuffix uid (ppCode (h ^. holeKw))

withNameIdSuffix :: (Members '[ExactPrint, Reader Options] r) => S.NameId -> Sem r () -> Sem r ()
withNameIdSuffix nid a = do
  showNameId <- asks (^. optShowNameIds)
  a
  when showNameId (noLoc "@" <> ppCode nid)

instance PrettyPrint S.NameId where
  ppCode = noLoc . pretty

ppModuleHeader :: (SingI t, SingI s) => PrettyPrinting (Module s t)
ppModuleHeader Module {..} = do
  let modulePath' = ppModulePathType _modulePath
  ppCode _moduleKw
    <+> modulePath'

instance (SingI t, SingI s) => PrettyPrint (Module s t) where
  ppCode :: forall r. (Members '[ExactPrint, Reader Options] r) => Module s t -> Sem r ()
  ppCode m@Module {..} = do
    let moduleBody' = localIndent (ppStatements _moduleBody)
        moduleDoc' = whenJust _moduleDoc ppCode
        modulePragmas' = whenJust _modulePragmas ppCode
        header' = ppModuleHeader m
        body'
          | null _moduleBody = ensureEmptyLine
          | otherwise =
              topSpace
                <> moduleBody'
                <> line
    moduleDoc'
      <> modulePragmas'
      <> header'
      <> ppCode Kw.delimSemicolon
      <> line
      <> body'
      <> ending
    where
      topSpace :: Sem r ()
      topSpace = case sing :: SModuleIsTop t of
        SModuleLocal -> mempty
        SModuleTop -> ensureEmptyLine

      localIndent :: Sem r () -> Sem r ()
      localIndent = case sing :: SModuleIsTop t of
        SModuleLocal -> indent
        SModuleTop -> id

      ending :: Sem r ()
      ending = case sing :: SModuleIsTop t of
        SModuleLocal -> ppCode _moduleKwEnd
        SModuleTop -> end

instance PrettyPrint BuiltinPrim where
  ppCode = noLoc . annotate AnnKeyword . pretty

instance (PrettyPrint a) => PrettyPrint [a] where
  ppCode x = do
    let cs = map ppCode (toList x)
    encloseSep (ppCode @Text "[") (ppCode @Text "]") (ppCode @Text ", ") cs

instance PrettyPrint TopModulePathKey where
  ppCode = noLoc . annotate (AnnKind KNameTopModule) . pretty

instance (SingI s) => PrettyPrint (Statements s) where
  ppCode = ppStatements . (^. statements)

ppStatements :: forall s r. (SingI s, Members '[ExactPrint, Reader Options] r) => [Statement s] -> Sem r ()
ppStatements ss = paragraphs (ppGroup <$> Concrete.groupStatements (filter shouldBePrinted ss))
  where
    shouldBePrinted :: Statement s -> Bool
    shouldBePrinted = \case
      StatementModule m -> m ^. moduleOrigin == LocalModuleSource
      _ -> True

    ppGroup :: NonEmpty (Statement s) -> Sem r ()
    ppGroup = vsep . sepEndSemicolon . fmap ppCode

instance PrettyPrint TopModulePath where
  ppCode m =
    morpheme (getLoc m)
      . annotate (AnnKind KNameTopModule)
      . topModulePathToDottedPath
      $ m

instance (PrettyPrint n) => PrettyPrint (S.Name' n) where
  ppCode :: forall r. (Members '[ExactPrint, Reader Options] r) => S.Name' n -> Sem r ()
  ppCode n@S.Name' {..} = do
    let nameConcrete' = region (C.annotateKind _nameKind) (ppCode _nameConcrete)
    annSRef (withNameIdSuffix _nameId nameConcrete')
    where
      annSRef :: Sem r () -> Sem r ()
      annSRef = annotated (AnnRef (nameReference n))

instance PrettyPrint Name where
  ppCode n = case n of
    NameUnqualified s -> ppCode s
    NameQualified s -> ppCode s

instance PrettyPrint QualifiedName where
  ppCode :: (Members '[ExactPrint, Reader Options] r) => QualifiedName -> Sem r ()
  ppCode QualifiedName {..} = do
    let symbols = _qualifiedPath ^. pathParts NonEmpty.|> _qualifiedSymbol
    dotted (ppSymbolType <$> symbols)

instance PrettyPrint ReservedModule where
  ppCode m = ppCode (m ^. reservedModuleName)

instance PrettyPrint ScopedModule where
  ppCode m = ppCode (m ^. scopedModuleName)

instance PrettyPrint ScopedIden where
  ppCode = ppCode . (^. scopedIdenSrcName)

instance (SingI s) => PrettyPrint (AliasDef s) where
  ppCode AliasDef {..} = do
    let doc' = ppCode <$> _aliasDefDoc
    doc'
      ?<> ppCode _aliasDefSyntaxKw
      <+> ppCode _aliasDefAliasKw
      <+> ppSymbolType _aliasDefName
      <+> ppCode Kw.kwAssign
      <+> ppIdentifierType _aliasDefAsName

instance (SingI s) => PrettyPrint (SyntaxDef s) where
  ppCode = \case
    SyntaxFixity f -> ppCode f
    SyntaxOperator op -> ppCode op
    SyntaxIterator it -> ppCode it
    SyntaxAlias it -> ppCode it

instance PrettyPrint Literal where
  ppCode = noLoc . ppLiteral

ppLiteral :: Literal -> Doc Ann
ppLiteral = \case
  LitIntegerWithBase n -> annotate AnnLiteralInteger (pretty n)
  LitString s -> ppStringLit s

instance (SingI s) => PrettyPrint (LambdaClause s) where
  ppCode LambdaClause {..} = do
    let lambdaParameters' = hsep (ppPatternAtom <$> _lambdaParameters)
        lambdaBody' = ppTopExpressionType _lambdaBody
        lambdaPipe' = ppCode <$> _lambdaPipe ^. unIrrelevant
    lambdaPipe' <?+> lambdaParameters' <+> ppCode _lambdaAssignKw <> oneLineOrNext lambdaBody'

instance (SingI s) => PrettyPrint (LetStatement s) where
  ppCode = \case
    LetFunctionDef f -> ppCode f
    LetAliasDef f -> ppCode f
    LetOpen f -> ppCode f

ppMaybeTopExpression :: (Members '[ExactPrint, Reader Options] r, SingI s) => IsTop -> ExpressionType s -> Sem r ()
ppMaybeTopExpression isTop e = case isTop of
  Top -> ppTopExpressionType e
  NotTop -> ppExpressionType e

ppLet :: forall r s. (Members '[ExactPrint, Reader Options] r, SingI s) => IsTop -> Let s -> Sem r ()
ppLet isTop Let {..} = do
  let letFunDefs' = ppBlock _letFunDefs
      letExpression' = ppMaybeTopExpression isTop _letExpression
  align $ ppCode _letKw <> letFunDefs' <> ppCode _letInKw <+> letExpression'

instance (SingI s, SingI k) => PrettyPrint (SideIfBranch s k) where
  ppCode SideIfBranch {..} = do
    let kwPipe' = ppCode <$> _sideIfBranchPipe ^. unIrrelevant
        kwIfElse' = ppCode _sideIfBranchKw
        kwAssign' = ppCode _sideIfBranchAssignKw
        condition' = case sing :: SIfBranchKind k of
          SBranchIfBool -> Just (ppExpressionType _sideIfBranchCondition)
          SBranchIfElse -> Nothing
        body' = ppExpressionType _sideIfBranchBody
    kwPipe'
      <?+> ( kwIfElse'
               <+?> condition'
               <+> kwAssign'
                 <> oneLineOrNext body'
           )

instance (SingI s) => PrettyPrint (SideIfs s) where
  ppCode :: forall r. (Members '[ExactPrint, Reader Options] r) => SideIfs s -> Sem r ()
  ppCode SideIfs {..} =
    case (_sideIfBranches, _sideIfElse) of
      (b :| [], Nothing) -> ppCode (set sideIfBranchPipe (Irrelevant Nothing) b)
      (b :| bs, _) -> do
        let putPipe :: Maybe KeywordRef -> Maybe KeywordRef
            putPipe = \case
              Nothing -> Just (run (runReader (getLoc b) (Gen.kw kwPipe)))
              Just p -> Just p
            firstBr = over (sideIfBranchPipe . unIrrelevant) putPipe b
            ifbranches = map ppCode (toList (firstBr : bs))
            allBranches :: [Sem r ()] = snocMaybe ifbranches (ppCode <$> _sideIfElse)
        line <> indent (vsepHard allBranches)

ppCaseBranchRhs :: forall r s. (Members '[ExactPrint, Reader Options] r, SingI s) => IsTop -> CaseBranchRhs s -> Sem r ()
ppCaseBranchRhs isTop = \case
  CaseBranchRhsExpression e -> ppExpressionRhs isTop e
  CaseBranchRhsIf ifCond -> ppCode ifCond

ppExpressionRhs :: (Member (Reader Options) r, Member ExactPrint r, SingI s) => IsTop -> RhsExpression s -> Sem r ()
ppExpressionRhs isTop RhsExpression {..} = do
  let expr' = ppMaybeTopExpression isTop _rhsExpression
  ppCode _rhsExpressionAssignKw <> oneLineOrNext expr'

ppCaseBranch :: forall r s. (Members '[ExactPrint, Reader Options] r, SingI s) => IsTop -> CaseBranch s -> Sem r ()
ppCaseBranch isTop CaseBranch {..} = do
  let pat' = ppPatternParensType _caseBranchPattern
      rhs' = ppCaseBranchRhs isTop _caseBranchRhs
      pipe' = ppCode <$> (_caseBranchPipe ^. unIrrelevant)
  pipe' <?+> pat' <+> rhs'

ppCase :: forall r s. (Members '[ExactPrint, Reader Options] r, SingI s) => IsTop -> Case s -> Sem r ()
ppCase isTop c = do
  let exp' = ppExpressionType (c ^. caseExpression)

  align $ ppCode (c ^. caseKw) <> oneLineOrNextBlock exp' <> ppCode (c ^. caseOfKw) <> ppBranches branches'
  where
    branches' = insertFirstPipe1 (caseBranchPipe . unIrrelevant) (c ^. caseBranches)

    ppBranches :: NonEmpty (CaseBranch s) -> Sem r ()
    ppBranches = \case
      b :| [] -> case isTop of
        Top -> oneLineOrNext (ppCaseBranch' Top b)
        NotTop -> space <> oneLineOrNextBraces (ppCaseBranch' NotTop b)
      _ -> ppPipeBranches True isTop ppCaseBranch' branches'

    ppCaseBranch' :: IsTop -> CaseBranch s -> Sem r ()
    ppCaseBranch' lastTopBranch b = ppCaseBranch lastTopBranch b

instance (SingI s) => PrettyPrint (IfBranch s 'BranchIfBool) where
  ppCode IfBranch {..} = do
    let pipe' = ppCode _ifBranchPipe
        cond' = ppExpressionType _ifBranchCondition
        e' = ppExpressionType _ifBranchExpression
    pipe' <+> cond' <+> ppCode _ifBranchAssignKw <> oneLineOrNext e'

ppIfBranchElse ::
  forall r s.
  (Members '[ExactPrint, Reader Options] r, SingI s) =>
  IsTop ->
  IfBranch s 'BranchIfElse ->
  Sem r ()
ppIfBranchElse isTop IfBranch {..} = do
  let e' = ppMaybeTopExpression isTop _ifBranchExpression
  ppCode _ifBranchPipe
    <+> ppCode _ifBranchCondition
    <+> ppCode _ifBranchAssignKw <> oneLineOrNext e'

ppIf :: forall r s. (Members '[ExactPrint, Reader Options] r, SingI s) => IsTop -> If s -> Sem r ()
ppIf isTop If {..} = do
  ppCode _ifKw
    <+> hardline
      <> indent
        ( vsepHard (ppIfBranch <$> _ifBranches)
            <> hardline
            <> ppIfBranch _ifBranchElse
        )
  where
    ppIfBranch :: forall k. (SingI k) => IfBranch s k -> Sem r ()
    ppIfBranch b = case sing :: SIfBranchKind k of
      SBranchIfBool -> ppCode b
      SBranchIfElse -> ppIfBranchElse isTop b

instance PrettyPrint Universe where
  ppCode Universe {..} = ppCode _universeKw <+?> (noLoc . pretty <$> _universeLevel)

apeHelper :: (IsApe a ApeLeaf, Members '[Reader Options, ExactPrint] r) => a -> Sem r ()
apeHelper a = do
  opts <- ask @Options
  let params :: ApeParams ApeLeaf
      params = ApeParams (runReader opts . ppCode)
  runApe params a

instance PrettyPrint ApeLeaf where
  ppCode = \case
    ApeLeafExpression e -> ppCode e
    ApeLeafFunctionParams a -> ppCode a
    ApeLeafFunctionKw r -> ppCode r
    ApeLeafPattern r -> ppCode r
    ApeLeafPatternArg r -> ppCode r
    ApeLeafAtom r -> ppAnyStage r

annDef :: forall s r. (SingI s, Members '[ExactPrint] r) => SymbolType s -> Sem r () -> Sem r ()
annDef nm = case sing :: SStage s of
  SScoped -> annSDef nm
  SParsed -> id

nameReference :: S.Name' n -> CodeReference
nameReference n@S.Name' {..} =
  CodeReference
    { _codeReferenceNameKindPretty = getNameKindPretty n,
      _codeReferenceLoc = loc
    }
  where
    loc :: CodeReferenceLoc
    loc
      | _nameTop =
          CodeReferenceLocTop
            TopCodeReference
              { _topCodeReferenceAbsModule = _nameDefinedIn,
                _topCodeReferenceVerbatimSymbol = _nameVerbatim
              }
      | otherwise =
          CodeReferenceLocLocal
            LocalCodeReference
              { _localCodeReferenceModule = _nameDefinedIn ^. absTopModulePath,
                _localCodeReferenceNameId = _nameId
              }

annSDef :: (Members '[ExactPrint] r) => S.Name' n -> Sem r () -> Sem r ()
annSDef = annotated . AnnDef . nameReference

instance (SingI s) => PrettyPrint (FunctionParameters s) where
  ppCode :: forall r. (Members '[ExactPrint, Reader Options] r) => FunctionParameters s -> Sem r ()
  ppCode FunctionParameters {..} = do
    case _paramNames of
      []
        | _paramImplicit == Explicit -> ppLeftExpression' funFixity _paramType
      _ -> do
        let paramNames' = map ppCode _paramNames
            paramType' = ppExpressionType _paramType
            delims' = over both ppCode <$> _paramDelims ^. unIrrelevant
            colon' = ppCode <$> _paramColon ^. unIrrelevant
            pre = case colon' of
              Just col -> hsep paramNames' <+> col <> space
              Nothing -> mempty
        delimIf' delims' _paramImplicit True (pre <> paramType')
    where
      ppLeftExpression' = case sing :: SStage s of
        SParsed -> ppLeftExpression
        SScoped -> ppLeftExpression

instance (SingI s) => PrettyPrint (FunctionParameter s) where
  ppCode = \case
    FunctionParameterName n -> annDef n (ppSymbolType n)
    FunctionParameterWildcard w -> ppCode w

instance (SingI s) => PrettyPrint (Function s) where
  ppCode :: forall r. (Members '[ExactPrint, Reader Options] r) => Function s -> Sem r ()
  ppCode a = case sing :: SStage s of
    SParsed -> helper a
    SScoped -> apeHelper a
    where
      helper :: Function 'Parsed -> Sem r ()
      helper Function {..} = do
        let funParameter' = ppCode _funParameters
            funReturn' = ppRightExpression' funFixity _funReturn
            funKw' = ppCode _funKw
        funParameter' <+> funKw' <+> funReturn'
        where
          ppRightExpression' = case sing :: SStage s of
            SParsed -> ppRightExpression
            SScoped -> ppRightExpression

ppRightExpression ::
  (PrettyPrint a, HasAtomicity a, Members '[Reader Options, ExactPrint] r) =>
  Fixity ->
  a ->
  Sem r ()
ppRightExpression = ppLRExpression isRightAssoc

ppLeftExpression ::
  (PrettyPrint a, HasAtomicity a, Members '[Reader Options, ExactPrint] r) =>
  Fixity ->
  a ->
  Sem r ()
ppLeftExpression = ppLRExpression isLeftAssoc

ppLRExpression ::
  (HasAtomicity a, PrettyPrint a, Members '[Reader Options, ExactPrint] r) =>
  (Fixity -> Bool) ->
  Fixity ->
  a ->
  Sem r ()
ppLRExpression associates fixlr e =
  parensIf
    (atomParens associates (atomicity e) fixlr)
    (ppCode e)

ppBlock' :: (Members '[Reader Options, ExactPrint] r, Traversable t) => t (Sem r ()) -> Sem r ()
ppBlock' items = blockIndent (vsepHard (sepEndSemicolon items))

ppBlock :: (PrettyPrint a, Members '[Reader Options, ExactPrint] r, Traversable t) => t a -> Sem r ()
ppBlock items = ppBlock' (fmap ppCode items)

ppBlockOrList' :: (Members '[Reader Options, ExactPrint] r, Traversable t) => t (Sem r ()) -> Sem r ()
ppBlockOrList' items =
  flatAlt
    (ppBlock' items)
    (hsepSemicolon items)

ppBlockOrList :: (PrettyPrint a, Members '[Reader Options, ExactPrint] r, Traversable t) => t a -> Sem r ()
ppBlockOrList items = ppBlockOrList' (fmap ppCode items)

instance (SingI s) => PrettyPrint (Lambda s) where
  ppCode Lambda {..} = do
    let lambdaKw' = ppCode _lambdaKw
        braces' = uncurry enclose (over both ppCode (_lambdaBraces ^. unIrrelevant))
        lambdaClauses' = braces' $ case insertFirstPipe1 (lambdaPipe . unIrrelevant) _lambdaClauses of
          s :| [] -> ppCode s
          clauses' -> blockIndent (vsepHard (ppCode <$> clauses'))
    lambdaKw' <> lambdaClauses'

-- | Inserts a pipe to the first element when it is not already there and the
-- list has more than one element
insertFirstPipe1 :: (HasLoc a) => Lens' a (Maybe KeywordRef) -> NonEmpty a -> NonEmpty a
insertFirstPipe1 pipekw l = case l of
  _ :| [] -> l
  a :| as ->
    let p = run (runReader (getLoc a) (Gen.kw Kw.kwPipe))
     in over pipekw (<|> Just p) a :| as

instance PrettyPrint Precedence where
  ppCode = \case
    PrecArrow -> noLoc (pretty ("-ω" :: Text))
    PrecNat n -> noLoc (pretty n)
    PrecApp -> noLoc (pretty ("ω" :: Text))
    PrecUpdate -> noLoc (pretty ("ω₁" :: Text))

ppFixityDefHeader :: (SingI s) => PrettyPrinting (FixitySyntaxDef s)
ppFixityDefHeader FixitySyntaxDef {..} = do
  let sym' = annotated (AnnKind KNameFixity) (ppSymbolType _fixitySymbol)
  ppCode _fixitySyntaxKw <+> ppCode _fixityKw <+> sym'

instance PrettyPrint Arity where
  ppCode = \case
    Unary -> noLoc Str.unary
    Binary -> noLoc Str.binary
    None -> noLoc Str.none

instance PrettyPrint BinaryAssoc where
  ppCode a = noLoc $ case a of
    AssocNone -> Str.none
    AssocLeft -> Str.left
    AssocRight -> Str.right

ppIdentifierList :: (SingI s) => PrettyPrinting [IdentifierType s]
ppIdentifierList items = do
  ppCode Kw.delimBracketL
  hsepSemicolon (map ppIdentifierType items)
  ppCode Kw.delimBracketR

instance (SingI s) => PrettyPrint (ParsedFixityInfo s) where
  ppCode ParsedFixityInfo {..} = do
    let rhs = do
          ParsedFixityFields {..} <- _fixityFields
          let assocItem = do
                a <- _fixityFieldsAssoc
                return (ppCode Kw.kwAssoc <+> ppCode Kw.kwAssign <+> ppCode a)
              sameItem = do
                a <- _fixityFieldsPrecSame
                return (ppCode Kw.kwSame <+> ppCode Kw.kwAssign <+> ppIdentifierType a)
              aboveItem = do
                a <- _fixityFieldsPrecAbove
                return (ppCode Kw.kwAbove <+> ppCode Kw.kwAssign <+> ppIdentifierList a)
              belowItem = do
                a <- _fixityFieldsPrecBelow
                return (ppCode Kw.kwBelow <+> ppCode Kw.kwAssign <+> ppIdentifierList a)
              items = ppBlockOrList' (catMaybes [assocItem, sameItem, aboveItem, belowItem])
              (l, r) = _fixityFieldsBraces ^. unIrrelevant
          return (grouped (ppCode l <> items <> ppCode r))
    ppCode _fixityParsedArity <+?> rhs

instance (SingI s) => PrettyPrint (FixitySyntaxDef s) where
  ppCode f@FixitySyntaxDef {..} = do
    let doc' = ppCode <$> _fixityDoc
        header' = ppFixityDefHeader f
        body' = ppCode _fixityInfo
    doc' ?<> header' <+> ppCode _fixityAssignKw <+> body'

instance PrettyPrint VisibilityAnn where
  ppCode = noLoc . ppCodeAnn

instance PrettyPrint ExportInfo where
  ppCode ExportInfo {..} = do
    header "Export Info"
    indent $ do
      itemize
        [ header "Symbols:"
            >> ppCode _exportSymbols,
          header "Module Symbols:"
            >> ppCode _exportModuleSymbols,
          header "Fixity Symbols:"
            >> ppCode _exportFixitySymbols
        ]
      hardline

instance (SingI s) => PrettyPrint (OperatorSyntaxDef s) where
  ppCode OperatorSyntaxDef {..} = do
    let doc' = ppCode <$> _opDoc
        opSymbol' = ppIdentifierType _opSymbol
        p = ppIdentifierType _opFixity
    doc' ?<> ppCode _opSyntaxKw <+> ppCode _opKw <+> opSymbol' <+> p

instance PrettyPrint PatternApp where
  ppCode = apeHelper

instance PrettyPrint Application where
  ppCode = apeHelper

instance PrettyPrint InfixApplication where
  ppCode = apeHelper

instance PrettyPrint PostfixApplication where
  ppCode = apeHelper

instance PrettyPrint ParsedIteratorInfo where
  ppCode :: forall r. (Members '[ExactPrint, Reader Options] r) => ParsedIteratorInfo -> Sem r ()
  ppCode ParsedIteratorInfo {..} = do
    let (l, r) = _parsedIteratorInfoBraces ^. unIrrelevant
        ppInt :: WithLoc Int -> Sem r ()
        ppInt = morphemeWithLoc . fmap (annotate AnnLiteralInteger . pretty)
        iniItem = do
          a <- _parsedIteratorInfoInitNum
          return (ppCode Kw.kwInit <+> ppCode Kw.kwAssign <+> ppInt a)
        rangeItem = do
          a <- _parsedIteratorInfoRangeNum
          return (ppCode Kw.kwRange <+> ppCode Kw.kwAssign <+> ppInt a)
        items = ppBlockOrList' (catMaybes [iniItem, rangeItem])
    grouped (ppCode l <> items <> ppCode r)

instance (SingI s) => PrettyPrint (IteratorSyntaxDef s) where
  ppCode IteratorSyntaxDef {..} = do
    let doc' = ppCode <$> _iterDoc
        iterSymbol' = ppIdentifierType _iterSymbol
    doc'
      ?<> ppCode _iterSyntaxKw
      <+> ppCode _iterIteratorKw
      <+> iterSymbol'
      <+?> fmap ppCode _iterInfo

instance PrettyPrint RecordUpdateApp where
  ppCode = apeHelper

instance PrettyPrint ParensRecordUpdate where
  ppCode = parens . ppCode . (^. parensRecordUpdate)

instance PrettyPrint Expression where
  ppCode = \case
    ExpressionIdentifier n -> ppCode n
    ExpressionHole w -> ppCode w
    ExpressionInstanceHole w -> ppCode w
    ExpressionParensIdentifier n -> parens (ppCode n)
    ExpressionBraces b -> braces (ppCode b)
    ExpressionDoubleBraces b -> ppCode b
    ExpressionApplication a -> ppCode a
    ExpressionList a -> ppCode a
    ExpressionInfixApplication a -> ppCode a
    ExpressionPostfixApplication a -> ppCode a
    ExpressionLambda l -> ppCode l
    ExpressionLet lb -> ppLet NotTop lb
    ExpressionDo d -> ppCode d
    ExpressionUniverse u -> ppCode u
    ExpressionLiteral l -> ppCode l
    ExpressionFunction f -> ppCode f
    ExpressionCase c -> ppCase NotTop c
    ExpressionIf c -> ppIf NotTop c
    ExpressionIterator i -> ppIterator NotTop i
    ExpressionNamedApplication i -> ppCode i
    ExpressionRecordUpdate i -> ppCode i
    ExpressionParensRecordUpdate i -> ppCode i

instance PrettyPrint (WithSource Pragmas) where
  ppCode pragma = do
    b <- asks (^. optPrintPragmas)
    when b $
      let txt = pretty (Str.pragmasStart <> pragma ^. withSourceText <> Str.pragmasEnd)
       in annotated AnnComment (noLoc txt) <> line

ppJudocStart :: (Members '[ExactPrint, Reader Options] r) => Sem r (Maybe ())
ppJudocStart = do
  inBlock <- asks (^. optInJudocBlock)
  if
      | inBlock -> return Nothing
      | otherwise -> ppCode Kw.delimJudocStart $> Just ()

instance (PrettyPrint a) => PrettyPrint (WithLoc a) where
  ppCode a = morphemeM (getLoc a) (ppCode (a ^. withLocParam))

instance (SingI s) => PrettyPrint (JudocAtom s) where
  ppCode :: forall r. (Members '[Reader Options, ExactPrint] r) => JudocAtom s -> Sem r ()
  ppCode = \case
    JudocExpression e -> goExpression e
    JudocText t -> annotated AnnJudoc (noLoc (pretty t))
    where
      goExpression :: ExpressionType s -> Sem r ()
      goExpression = semiDelim . ppExpressionType
      semiDelim :: Sem r () -> Sem r ()
      semiDelim = enclose1 (annotated AnnJudoc (noLoc (";" :: Doc Ann)))

instance (SingI s) => PrettyPrint (JudocLine s) where
  ppCode :: forall r. (Members '[ExactPrint, Reader Options] r) => JudocLine s -> Sem r ()
  ppCode (JudocLine deli atoms) = do
    let start' :: Maybe (Sem r ()) = ppCode <$> deli
        atoms' = mapM_ ppCode atoms
    bJudoc <- asks (^. optJudoc)
    if
        | bJudoc -> atoms'
        | otherwise -> start' <?+> atoms'

instance (SingI s) => PrettyPrint (Judoc s) where
  ppCode :: forall r. (Members '[ExactPrint, Reader Options] r) => Judoc s -> Sem r ()
  ppCode (Judoc groups) = do
    bJudoc <- asks (^. optJudoc)
    ppGroups groups <> if bJudoc then mempty else hardline
    where
      ppGroups :: NonEmpty (JudocGroup s) -> Sem r ()
      ppGroups = \case
        g :| [] -> ppCode g
        g :| b : bs -> ppCode g <> groupSep <> ppGroups (b :| bs)
          where
            groupSep :: Sem r ()
            groupSep = line <> extraLine
            extraLine :: Sem r ()
            extraLine = case (g, b) of
              (JudocGroupLines {}, JudocGroupLines {}) -> delim <> line
                where
                  delim :: Sem r ()
                  delim = do
                    bJudoc <- asks (^. optJudoc)
                    if bJudoc then mempty else ppCode Kw.delimJudocStart
              _ -> return ()

instance (SingI s) => PrettyPrint (JudocBlockParagraph s) where
  ppCode p = do
    let start' = ppCode (p ^. judocBlockParagraphStart)
        contents' = inJudocBlock (vsep2 (ppCode <$> p ^. judocBlockParagraphBlocks))
        endpar' = ppCode (p ^. judocBlockParagraphEnd)
    bJudoc <- asks (^. optJudoc)
    if
        | bJudoc -> contents'
        | otherwise -> start' <+> contents' <+> endpar'

instance (SingI s) => PrettyPrint (JudocGroup s) where
  ppCode :: forall r. (Members '[ExactPrint, Reader Options] r) => JudocGroup s -> Sem r ()
  ppCode = \case
    JudocGroupLines l -> goLines l
    JudocGroupBlock l -> ppCode l
    where
      goLines blocks = sequenceWith blockSep (fmap ppCode blocks)
        where
          blockSep :: Sem r ()
          blockSep = hardline >> ppJudocStart >> hardline

instance (SingI s) => PrettyPrint (JudocBlock s) where
  ppCode = \case
    JudocLines l -> vsep (ppCode <$> l)

instance (SingI s) => PrettyPrint (AxiomDef s) where
  ppCode :: forall r. (Members '[ExactPrint, Reader Options] r) => AxiomDef s -> Sem r ()
  ppCode AxiomDef {..} = do
    let axiomName' = annDef _axiomName (ppSymbolType _axiomName)
        builtin' :: Maybe (Sem r ()) = (<> line) . ppCode <$> _axiomBuiltin
        _axiomDoc' :: Maybe (Sem r ()) = ppCode <$> _axiomDoc
        _axiomPragmas' :: Maybe (Sem r ()) = ppCode <$> _axiomPragmas
    _axiomDoc'
      ?<> _axiomPragmas'
      ?<> builtin'
      ?<> ppCode _axiomKw
      <+> axiomName'
        <> ppCode _axiomTypeSig

instance PrettyPrint BuiltinInductive where
  ppCode i = ppCode Kw.kwBuiltin <+> keywordText (P.prettyText i)

instance PrettyPrint BuiltinFunction where
  ppCode i = ppCode Kw.kwBuiltin <+> keywordText (P.prettyText i)

instance PrettyPrint BuiltinAxiom where
  ppCode i = ppCode Kw.kwBuiltin <+> keywordText (P.prettyText i)

ppFunctionClause :: (SingI s, Members '[ExactPrint, Reader Options] r) => IsTop -> FunctionClause s -> Sem r ()
ppFunctionClause isTop FunctionClause {..} = do
  let pats' = hsep (ppPatternAtomType <$> _clausenPatterns)
      e' = ppMaybeTopExpression isTop _clausenBody
  ppCode _clausenPipeKw <+> pats' <+> ppCode _clausenAssignKw <> oneLineOrNext e'

instance (SingI s) => PrettyPrint (Argument s) where
  ppCode :: (Members '[ExactPrint, Reader Options] r) => Argument s -> Sem r ()
  ppCode = \case
    ArgumentSymbol s -> ppSymbolType s
    ArgumentWildcard w -> ppCode w

instance (SingI s) => PrettyPrint (ArgDefault s) where
  ppCode ArgDefault {..} = do
    ppCode _argDefaultAssign <+> ppExpressionType _argDefaultValue

instance (SingI s) => PrettyPrint (SigArgNames s) where
  ppCode = \case
    SigArgNamesInstance -> return ()
    SigArgNames ns -> hsep (fmap ppCode ns)

instance (SingI s) => PrettyPrint (SigArg s) where
  ppCode :: (Members '[ExactPrint, Reader Options] r) => SigArg s -> Sem r ()
  ppCode SigArg {..} = do
    let Irrelevant (l, r) = _sigArgDelims
        names' = ppCode _sigArgNames
        colon' = ppCode <$> _sigArgColon
        ty = ppExpressionType <$> _sigArgType
        arg = case _sigArgImplicit of
          ImplicitInstance
            | isNothing colon' -> mempty <>? ty
          _ -> names' <+?> colon' <+?> ty
        defaultVal = ppCode <$> _sigArgDefault
    ppCode l <> arg <+?> defaultVal <> ppCode r

instance (SingI s) => PrettyPrint (Deriving s) where
  ppCode Deriving {..} =
    (ppCode <$> _derivingPragmas)
      ?<> ppCode _derivingKw
      <+> ppCode _derivingFunLhs

instance (SingI s) => PrettyPrint (TypeSig s) where
  ppCode TypeSig {..} = do
    let margs' = fmap ppCode <$> nonEmpty _typeSigArgs
        mtype' = case _typeSigColonKw ^. unIrrelevant of
          Just col -> Just (ppCode col <+> ppExpressionType (fromJust _typeSigRetType))
          Nothing -> Nothing
        margsAndType' = case mtype' of
          Nothing -> margs'
          Just ty' -> case margs' of
            Nothing -> Just (pure ty')
            Just args' -> Just (args' <> pure ty')
    case margsAndType' of
      Nothing -> return ()
      Just argsAndType' -> oneLineOrNext (sep argsAndType')

instance (SingI s) => PrettyPrint (FunctionLhs s) where
  ppCode FunctionLhs {..} = do
    let termin' = (<> line) . ppCode <$> _funLhsTerminating
        coercion' = (<> if isJust instance' then space else line) . ppCode <$> _funLhsCoercion
        instance' = (<> line) . ppCode <$> _funLhsInstance
        builtin' = (<> line) . ppCode <$> _funLhsBuiltin
        mpat :: Maybe (PatternAtomType s) = functionSymbolPattern _funLhsName
        name' = case mpat of
          Just pat -> withFunctionSymbol id annDef _funLhsName (ppPatternAtomType pat)
          Nothing -> annDef (getFunctionSymbol _funLhsName) (ppSymbolType (getFunctionSymbol _funLhsName))
        sig' = ppCode _funLhsTypeSig
    builtin'
      ?<> termin'
      ?<> coercion'
      ?<> instance'
      ?<> (name' <> sig')

ppPipeBranches :: (Members '[ExactPrint, Reader Options] r) => Bool -> IsTop -> (IsTop -> a -> Sem r ()) -> NonEmpty a -> Sem r ()
ppPipeBranches allowSameLine isTop ppBranch = \case
  b :| [] -> case isTop of
    Top
      | allowSameLine -> oneLineOrNext (ppBranch Top b)
      | otherwise -> hardline <> indent (ppBranch Top b)
    NotTop -> space <> oneLineOrNextBraces (ppBranch NotTop b)
  branches -> case isTop of
    Top -> do
      let brs =
            vsepHard (ppBranch NotTop <$> NonEmpty.init branches)
              <> hardline
              <> ppBranch Top (NonEmpty.last branches)
      hardline <> indent brs
    NotTop -> space <> braces (blockIndent (vsepHard (ppBranch NotTop <$> branches)))

instance (SingI s) => PrettyPrint (FunctionDef s) where
  ppCode :: forall r. (Members '[ExactPrint, Reader Options] r) => FunctionDef s -> Sem r ()
  ppCode fun@FunctionDef {..} = do
    let doc' :: Maybe (Sem r ()) = ppCode <$> _functionDefDoc
        pragmas' :: Maybe (Sem r ()) = ppCode <$> _functionDefPragmas
        sig' = ppCode (fun ^. functionDefLhs)
        body' = case _functionDefBody of
          SigBodyExpression e -> space <> ppCode Kw.kwAssign <> oneLineOrNext (ppTopExpressionType e)
          SigBodyClauses k -> ppPipeBranches False Top ppFunctionClause k
    doc'
      ?<> pragmas'
      ?<> sig'
      <> body'

instance PrettyPrint Wildcard where
  ppCode w = morpheme (getLoc w) C.kwWildcard

instance (SingI s) => PrettyPrint (PatternFieldPun s) where
  ppCode = ppSymbolType . (^. fieldPunField)

instance (SingI s) => PrettyPrint (RecordPatternAssign s) where
  ppCode a = do
    ppCode (a ^. recordPatternAssignField)
      <+> ppCode (a ^. recordPatternAssignKw)
      <+> ppPatternParensType (a ^. recordPatternAssignPattern)

instance (SingI s) => PrettyPrint (RecordPatternItem s) where
  ppCode = \case
    RecordPatternItemFieldPun f -> ppCode f
    RecordPatternItemAssign f -> ppCode f

instance (SingI s) => PrettyPrint (RecordPattern s) where
  ppCode r = do
    let c = ppIdentifierType (r ^. recordPatternConstructor)
        items = ppBlockOrList (r ^. recordPatternItems)
    grouped (align (c <> noLoc C.kwAt <> braces items))

instance PrettyPrint Pattern where
  ppCode = \case
    PatternVariable v -> annDef v (ppCode v)
    PatternApplication (PatternApp l r) -> do
      let l' = ppLeftExpression appFixity l
          r' = ppRightExpression appFixity r
      l' <+> r'
    PatternWildcard w -> ppCode w
    PatternWildcardConstructor w -> ppCode w
    PatternList w -> ppCode w
    PatternEmpty {} -> parens (return ())
    PatternConstructor constr -> ppCode constr
    PatternInfixApplication i -> apeHelper i
    PatternPostfixApplication i -> apeHelper i
    PatternRecord i -> ppCode i

instance PrettyPrint PatternArg where
  ppCode PatternArg {..} = do
    let name' = ppCode <$> _patternArgName
        pat' = ppCode _patternArgPattern

    let delimCond :: Bool
        delimCond = isJust _patternArgName && not (isAtomic _patternArgPattern)

        asPatternInfo =
          ((name' <&> (<> ppCode Kw.kwAt)) ?<>)
            . if delimCond then parens else id

    case _patternArgIsImplicit of
      Explicit -> asPatternInfo pat'
      ImplicitInstance -> doubleBraces . asPatternInfo $ pat'
      Implicit -> braces . asPatternInfo $ pat'

instance PrettyPrint Text where
  ppCode = noLoc . pretty

ppUnkindedSymbol :: (Members '[Reader Options, ExactPrint] r) => WithLoc Text -> Sem r ()
ppUnkindedSymbol = region (annotate AnnUnkindedSym) . ppCode

instance (SingI s) => PrettyPrint (HidingItem s) where
  ppCode h = do
    let sym = ppSymbolType (h ^. hidingSymbol)
        kwmodule = ppCode <$> (h ^. hidingModuleKw)
    kwmodule <?+> sym

instance (SingI s) => PrettyPrint (HidingList s) where
  ppCode HidingList {..} = do
    let (openb, closeb) = _hidingBraces ^. unIrrelevant
        items' = ppBlockOrList _hidingList
    grouped (ppCode _hidingKw <+> ppCode openb <> items' <> ppCode closeb)

instance (SingI s) => PrettyPrint (UsingList s) where
  ppCode UsingList {..} = do
    let (openb, closeb) = _usingBraces ^. unIrrelevant
        items' = ppBlockOrList _usingList
    grouped (ppCode _usingKw <+> ppCode openb <> items' <> ppCode closeb)

instance (SingI s) => PrettyPrint (UsingHiding s) where
  ppCode :: forall r. (Members '[ExactPrint, Reader Options] r) => UsingHiding s -> Sem r ()
  ppCode = \case
    Using u -> ppCode u
    Hiding h -> ppCode h

instance (SingI s) => PrettyPrint (UsingItem s) where
  ppCode :: forall r. (Members '[ExactPrint, Reader Options] r) => UsingItem s -> Sem r ()
  ppCode ui = do
    let kwAs' :: Maybe (Sem r ()) = ppCode <$> ui ^. usingAsKw . unIrrelevant
        alias' = ppSymbolType <$> ui ^. usingAs
        sym' = ppSymbolType (ui ^. usingSymbol)
        kwmodule = ppCode <$> (ui ^. usingModuleKw)
    kwmodule <?+> (sym' <+?> kwAs' <+?> alias')

instance PrettyPrint PackageInfo where
  ppCode PackageInfo {..} = do
    header ("Package name: " <> (_packagePackage ^. packageLikeName))
    noLoc ("root:" P.<+> pretty _packageRoot)
    let roots = case nonEmpty _packageAvailableRoots of
          Nothing -> return ()
          Just roots1 -> hardline <> indent (itemize (fmap (noLoc . pretty) roots1))
    hardline
    noLoc ("available roots:") <> roots
    hardline
    noLoc ("package id:" P.<+> ppCodeAnn _packageInfoPackageId)
    hardline

instance PrettyPrint ImportTreeStats where
  ppCode ImportTreeStats {..} = do
    header "Import Tree Statistics:"
    header "======================="
    itemize
      [ noLoc "Total number of nodes:" <+> noLoc (pretty _importTreeStatsTotalModules),
        noLoc "Total number of edges:" <+> noLoc (pretty _importTreeStatsTotalEdges),
        noLoc "Height (longest chain of imports):" <+> noLoc (pretty _importTreeStatsHeight)
      ]
    hardline

instance PrettyPrint ImportTree where
  ppCode tree = do
    header "Import Tree:"
    header "============"
    hardline

    header ("Packages (" <> show (length importsTable) <> "):")
    header "========="
    itemize . map (noLoc . pretty) $ Map.keys importsTable
    hardline

    hardline
    forM_ (Map.toList importsTable) $ \(pkgRoot, tbl :: Map (Path Rel File) (Set ImportNode)) -> do
      annotated AnnImportant (noLoc ("* Package at " <> pretty pkgRoot))
      hardline
      let pkgNodes :: HashSet ImportNode = fromJust (nodesByRoot ^. at pkgRoot)
      header ("Nodes Relative paths (" <> show (length pkgNodes) <> ")")
      forM_ pkgNodes $ \node -> do
        noLoc (pMod (node ^. importNodeFile))
        hardline
      hardline
      header ("Nodes Absolute paths (" <> show (length pkgNodes) <> ")")
      forM_ pkgNodes $ \node -> do
        noLoc (pMod (node ^. importNodeAbsFile))
        hardline
      hardline
      let numEdges = sum (map length (toList tbl))
      header ("Edges (" <> show numEdges <> ")")
      forM_ (Map.toList tbl) $ \(fromFile, toFiles :: Set ImportNode) -> do
        let fromNode :: ImportNode =
              ImportNode
                { _importNodePackageRoot = pkgRoot,
                  _importNodeFile = fromFile
                }
        noLoc (pMod fromFile P.<+> "at" P.<+> pMod (fromNode ^. importNodeAbsFile) P.<+> annotate AnnKeyword "imports" P.<+> "(" <> pretty (length toFiles) <> "):")
        hardline
        indent . itemize . (`map` (toList toFiles)) $ \toFile -> do
          let toMod
                | pkgRoot == toFile ^. importNodePackageRoot = pMod (toFile ^. importNodeFile)
                | otherwise = pMod (toFile ^. importNodeAbsFile)
          noLoc toMod
        hardline
        unless (null toFiles) hardline
      hardline
    where
      pMod :: Path x File -> Doc Ann
      pMod = annotate (AnnKind KNameTopModule) . pretty

      allNodes :: [ImportNode]
      allNodes = HashMap.keys (tree ^. importTree)

      allRoots :: [Path Abs Dir]
      allRoots = nubSort (map (^. importNodePackageRoot) allNodes)

      nodesByRoot :: HashMap (Path Abs Dir) (HashSet ImportNode)
      nodesByRoot = importTreeNodesByPackage tree

      -- fromPackageRoot -> fromFile -> tofile
      importsTable :: Map (Path Abs Dir) (Map (Path Rel File) (Set ImportNode))
      importsTable =
        ordMap
          [ (root, rootSubTable)
            | root <- allRoots,
              let nodesInRoot = toList (nodesByRoot ^?! at root . _Just),
              let rootSubTable :: Map (Path Rel File) (Set ImportNode)
                  rootSubTable =
                    ordMap
                      [ (from ^. importNodeFile, ordSet (tree ^. importTree ^?! at from . _Just))
                        | from :: ImportNode <- nodesInRoot
                      ]
          ]

instance PrettyPrint (ImportScan' a) where
  ppCode = noLoc . ppCodeAnn

instance (SingI s) => PrettyPrint (Import s) where
  ppCode :: forall r. (Members '[ExactPrint, Reader Options] r) => Import s -> Sem r ()
  ppCode i = do
    let open' = ppCode <$> (i ^. importOpen)
        usingHiding' = ppCode <$> i ^. importUsingHiding
        public' = ppCode <$> i ^? importPublic . _Public
    ppCode (i ^. importKw)
      <+> ppModulePathType (i ^. importModulePath)
      <+?> ppAlias
      <+?> usingHiding'
      <+?> public'
      <+?> open'
    where
      ppAlias :: Maybe (Sem r ())
      ppAlias = case i ^. importAsName of
        Nothing -> Nothing
        Just as -> Just (ppCode Kw.kwAs <+> ppModulePathType as)

instance (SingI s, SingI short) => PrettyPrint (OpenModule s short) where
  ppCode :: forall r. (Members '[ExactPrint, Reader Options] r) => OpenModule s short -> Sem r ()
  ppCode OpenModule {..} = do
    let name' :: Maybe (Sem r ()) = case sing :: SIsOpenShort short of
          SOpenShort -> Nothing
          SOpenFull -> Just (ppModuleNameType _openModuleName)
        openkw = ppCode _openModuleKw
        pub = ppPublicAnn _openModulePublic
        usingHiding = ppCode <$> _openModuleUsingHiding
    openkw
      <+?> name'
      <+?> usingHiding
      <+?> pub

ppPublicAnn :: PrettyPrintingMaybe PublicAnn
ppPublicAnn = \case
  NoPublic -> Nothing
  Public k -> Just (ppCode (k ^. unIrrelevant))

ppCodeAtom :: (HasAtomicity c, PrettyPrint c) => PrettyPrinting c
ppCodeAtom c = do
  let p' = ppCode c
  if isAtomic c then p' else parens p'

ppPatternAtom :: forall s. (SingI s) => PrettyPrinting (PatternAtomType s)
ppPatternAtom = case sing :: SStage s of
  SParsed -> ppCodeAtom
  SScoped -> \pat ->
    case pat ^. patternArgPattern of
      PatternVariable s | s ^. S.nameVerbatim == "=" -> parens (ppCodeAtom pat)
      _ -> ppCodeAtom pat

instance (SingI s) => PrettyPrint (InductiveParametersRhs s) where
  ppCode InductiveParametersRhs {..} =
    ppCode _inductiveParametersColon <+> ppExpressionType _inductiveParametersType

instance (SingI s) => PrettyPrint (InductiveParameters s) where
  ppCode InductiveParameters {..} = do
    let names' = fmap (\nm -> annDef nm (ppSymbolType nm)) _inductiveParametersNames
    case _inductiveParametersRhs of
      Just rhs -> parens (hsep names' <+> ppCode rhs)
      Nothing -> hsep names'

instance (SingI s) => PrettyPrint (NonEmpty (InductiveParameters s)) where
  ppCode = hsep . fmap ppCode

instance (PrettyPrint a) => PrettyPrint (Irrelevant a) where
  ppCode (Irrelevant a) = ppCode a

instance (SingI s) => PrettyPrint (RhsGadt s) where
  ppCode RhsGadt {..} = ppCode _rhsGadtTypeSig

instance (SingI s) => PrettyPrint (RecordField s) where
  ppCode RecordField {..} = do
    let doc' = ppCode <$> _fieldDoc
        pragmas' = ppCode <$> _fieldPragmas
        builtin' = (<> line) . ppCode <$> _fieldBuiltin
        mayBraces :: forall r'. (Members '[ExactPrint] r') => Sem r' () -> Sem r' ()
        mayBraces = case _fieldIsImplicit of
          ExplicitField -> id
          ImplicitInstanceField -> doubleBraces
    doc'
      ?<> pragmas'
      ?<> builtin'
      ?<> mayBraces (ppSymbolType _fieldName)
      <> ppCode _fieldTypeSig

instance (SingI s) => PrettyPrint (RhsRecord s) where
  ppCode RhsRecord {..} = do
    let Irrelevant (_, l, r) = _rhsRecordDelim
        fields'
          | null _rhsRecordStatements = mempty
          | otherwise = ppBlock _rhsRecordStatements
    ppCode kwAt <> ppCode l <> fields' <> ppCode r

instance (SingI s) => PrettyPrint (RhsAdt s) where
  ppCode = align . sep . fmap ppExpressionAtomType . (^. rhsAdtArguments)

instance (SingI s) => PrettyPrint (ConstructorRhs s) where
  ppCode :: (Members '[ExactPrint, Reader Options] r) => ConstructorRhs s -> Sem r ()
  ppCode = \case
    ConstructorRhsGadt r -> ppCode r
    ConstructorRhsRecord r -> ppCode r
    ConstructorRhsAdt r -> ppCode r

instance (SingI s) => PrettyPrint (ConstructorDef s) where
  ppCode :: forall r. (Members '[ExactPrint, Reader Options] r) => ConstructorDef s -> Sem r ()
  ppCode ConstructorDef {..} = do
    let constructorName' = annDef _constructorName (ppSymbolType _constructorName)
        constructorRhs' = constructorRhsHelper _constructorRhs
        doc' = ppCode <$> _constructorDoc
        pragmas' = ppCode <$> _constructorPragmas
        pipe = ppCode <$> (_constructorPipe ^. unIrrelevant)

        nestCond :: Sem r () -> Sem r ()
        nestCond x = case _constructorPipe ^. unIrrelevant of
          Just p -> printCommentsUntil (getLoc p) >> nest x
          Nothing -> x
    nestCond (pipe <?+> doc' ?<> pragmas' ?<> constructorName' <> constructorRhs')
    where
      constructorRhsHelper :: ConstructorRhs s -> Sem r ()
      constructorRhsHelper r = spaceMay <> ppCode r
        where
          spaceMay = case r of
            ConstructorRhsGadt {} -> mempty
            ConstructorRhsRecord {} -> mempty
            ConstructorRhsAdt a
              | null (a ^. rhsAdtArguments) -> mempty
              | otherwise -> space

ppInductiveSignature :: (SingI s) => PrettyPrinting (InductiveDef s)
ppInductiveSignature InductiveDef {..} = do
  let builtin' = (<> line) . ppCode <$> _inductiveBuiltin
      name' = annDef _inductiveName (ppSymbolType _inductiveName)
      params' = ppCode <$> nonEmpty _inductiveParameters
      ty' = case _inductiveType of
        Nothing -> Nothing
        Just e -> Just (colon <+> ppExpressionType e)
      positive'
        | Just k <- _inductivePositive = (<> line) <$> Just (ppCode k)
        | otherwise = Nothing
      trait' = (<> line) . ppCode <$> _inductiveTrait
  builtin'
    ?<> positive'
    ?<> trait'
    ?<> ppCode _inductiveKw
    <+> name'
    <+?> params'
    <+?> ty'

instance (SingI s) => PrettyPrint (WithModule s) where
  ppCode :: (Members '[ExactPrint, Reader Options] r) => WithModule s -> Sem r ()
  ppCode WithModule {..} = do
    let moduleBody' = unless (null _withModuleBody) $ do
          indent (ppStatements _withModuleBody)
          hardline
    ppCode _withModuleWithKw
      <> hardline
      <> moduleBody'
      <> ppCode _withModuleEndKw

instance (SingI s) => PrettyPrint (InductiveDef s) where
  ppCode :: forall r. (Members '[ExactPrint, Reader Options] r) => InductiveDef s -> Sem r ()
  ppCode d@InductiveDef {..} = do
    let doc' = ppCode <$> _inductiveDoc
        pragmas' = ppCode <$> _inductivePragmas
        constrs' = ppConstructorBlock (insertFirstPipe1 (constructorPipe . unIrrelevant) _inductiveConstructors)
        sig' = ppInductiveSignature d
        withModule' = ppCode <$> _inductiveWithModule
    doc'
      ?<> pragmas'
      ?<> sig'
      <+> ppCode _inductiveAssignKw
        <> constrs'
        <>? ((line <>) <$> withModule')
    where
      ppConstructorBlock :: NonEmpty (ConstructorDef s) -> Sem r ()
      ppConstructorBlock = \case
        c :| []
          | not (has (constructorRhs . _ConstructorRhsRecord . rhsRecordStatements . each) c) -> oneLineOrNext (ppCode c)
        cs -> hardline <> indent (vsep (ppCode <$> cs))

instance (SingI s) => PrettyPrint (ProjectionDef s) where
  ppCode ProjectionDef {..} =
    do
      ppSymbolType _projectionField
      <+> noLoc ":= projection"
      <+> noLoc (pretty _projectionFieldIx)
      <+> noLoc "for"
      <+> ppSymbolType _projectionConstructor

ppReservedInductiveDefType :: forall s. (SingI s) => PrettyPrinting (ReservedInductiveDefType s)
ppReservedInductiveDefType x = case sing :: SStage s of
  SParsed -> ppCode x
  SScoped -> absurd x

instance PrettyPrint ReservedInductiveDef where
  ppCode ReservedInductiveDef {..} = do
    ppStatements [StatementInductive _reservedInductiveDef, StatementModule _reservedInductiveDefModule]

instance (SingI s) => PrettyPrint (Statement s) where
  ppCode = \case
    StatementSyntax s -> ppCode s
    StatementReservedInductive s -> ppReservedInductiveDefType s
    StatementFunctionDef f -> ppCode f
    StatementDeriving f -> ppCode f
    StatementImport i -> ppCode i
    StatementInductive i -> ppCode i
    StatementModule m -> ppCode m
    StatementOpenModule o -> ppCode o
    StatementAxiom a -> ppCode a
    StatementProjectionDef a -> ppCode a

instance PrettyPrint PreSymbolEntry where
  ppCode = \case
    PreSymbolAlias a -> ppCode a
    PreSymbolFinal a -> ppCode a

instance PrettyPrint Alias where
  ppCode a =
    noLoc
      ( kindWord
          P.<+> C.code ((pretty (a ^. aliasName . S.nameVerbatim)))
          P.<+> "defined at"
          P.<+> pretty (getLoc a)
      )
    where
      kindWord :: Doc Ann = "Alias"

instance PrettyPrint SymbolEntry where
  ppCode ent =
    noLoc
      ( kindWord
          P.<+> C.code (kindAnn (pretty (ent ^. symbolEntry . S.nameVerbatim)))
          P.<+> "defined at"
          P.<+> pretty (getLoc ent)
      )
    where
      pretty' :: Text -> Doc a
      pretty' = pretty
      (kindAnn :: Doc Ann -> Doc Ann, kindWord :: Doc Ann) =
        let k = getNameKind ent
         in (annotate (AnnKind k), pretty' (nameKindText k))

instance PrettyPrint FixitySymbolEntry where
  ppCode ent =
    noLoc
      ( kindWord
          P.<+> C.code (kindAnn (pretty (ent ^. fixityEntry . S.nameVerbatim)))
          P.<+> "defined at"
          P.<+> pretty (getLoc ent)
      )
    where
      pretty' :: Text -> Doc a
      pretty' = pretty
      (kindAnn :: Doc Ann -> Doc Ann, kindWord :: Doc Ann) =
        let k = getNameKind ent
         in (annotate (AnnKind k), pretty' (nameKindText k))

instance PrettyPrint ModuleSymbolEntry where
  ppCode ent = do
    let mname = ppCode (ent ^. moduleEntry . S.nameVerbatim)
    noLoc
      kindWord
      <+> mname
      <+> noLoc "defined at"
      <+> noLoc (pretty (getLoc ent))
    where
      kindWord :: Doc Ann =
        let k = getNameKind ent
         in (pretty (nameKindText k))

header :: (Members '[ExactPrint] r) => Text -> Sem r ()
header txt = annotated AnnImportant (noLoc (pretty (txt <> "\n")))

instance PrettyPrint ScoperState where
  ppCode :: (Members '[ExactPrint, Reader Options] r) => ScoperState -> Sem r ()
  ppCode s =
    do
      header "scoperModules"
      <> sepSemicolon (map ppCode (HashMap.toList (s ^. scoperModules)))
