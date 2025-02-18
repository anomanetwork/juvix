module Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping
  ( module Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Data.Context,
    module Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Error,
    scopeCheck,
    scopeCheckRepl,
    getModuleId,
    scopeCheckImport,
    scopeCheckOpenModule,
    scopeCheckExpression,
    scopeCheckExpressionAtoms,
    iniScoperState,
  )
where

import Control.Monad.Combinators.Expr qualified as P
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.List.NonEmpty qualified as NonEmpty
import GHC.Base (maxInt, minInt)
import Juvix.Compiler.Concrete.Data.Highlight.Builder
import Juvix.Compiler.Concrete.Data.InfoTableBuilder
import Juvix.Compiler.Concrete.Data.Name qualified as N
import Juvix.Compiler.Concrete.Data.NameSignature
import Juvix.Compiler.Concrete.Data.Scope
import Juvix.Compiler.Concrete.Data.ScopedName (nameConcrete)
import Juvix.Compiler.Concrete.Data.ScopedName qualified as S
import Juvix.Compiler.Concrete.Extra qualified as P
import Juvix.Compiler.Concrete.Gen qualified as G
import Juvix.Compiler.Concrete.Language
import Juvix.Compiler.Concrete.Pretty (ppTrace)
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Data.Context
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Error
import Juvix.Compiler.Concrete.Translation.FromSource.Data.Context qualified as Parser
import Juvix.Compiler.Store.Scoped.Language as Store
import Juvix.Data.FixityInfo qualified as FI
import Juvix.Prelude

data PatternNamesKind
  = PatternNamesKindVariables
  | PatternNamesKindFunctions

scopeCheck ::
  (Members '[HighlightBuilder, Error JuvixError, NameIdGen] r) =>
  PackageId ->
  ScopedModuleTable ->
  Parser.ParserResult ->
  Sem r ScoperResult
scopeCheck pkg importMap pr =
  mapError (JuvixError @ScoperError)
    . runReader pkg
    $ scopeCheck' importMap pr m
  where
    m :: Module 'Parsed 'ModuleTop
    m = pr ^. Parser.resultModule

iniScoperState :: InfoTable -> ScoperState
iniScoperState tab =
  ScoperState
    { _scoperModules = mempty,
      _scoperReservedModules = mempty,
      _scoperExportInfo = mempty,
      _scoperNameSignatures = tab ^. infoParsedNameSigs,
      _scoperRecordFields = tab ^. infoRecords,
      _scoperAlias = tab ^. infoScoperAlias,
      _scoperConstructorFields = tab ^. infoParsedConstructorSigs,
      _scoperScopedConstructorFields = tab ^. infoConstructorSigs
    }

scopeCheck' ::
  (Members '[HighlightBuilder, Error ScoperError, NameIdGen, Reader PackageId] r) =>
  ScopedModuleTable ->
  Parser.ParserResult ->
  Module 'Parsed 'ModuleTop ->
  Sem r ScoperResult
scopeCheck' importTab pr m = do
  fmap mkResult
    . runReader tab
    . runReader iniScopeParameters
    . runState (iniScoperState tab)
    $ checkTopModule m
  where
    tab :: InfoTable
    tab = computeCombinedInfoTable importTab

    iniScopeParameters :: ScopeParameters
    iniScopeParameters =
      ScopeParameters
        { _scopeImportedModules = importTab ^. scopedModuleTable
        }

    mkResult :: (ScoperState, (Module 'Scoped 'ModuleTop, ScopedModule, Scope)) -> ScoperResult
    mkResult (scoperSt, (md, sm, sc)) =
      let exp = createExportsTable (sm ^. scopedModuleExportInfo)
       in ScoperResult
            { _resultParserResult = pr,
              _resultModule = md,
              _resultScopedModule = sm,
              _resultExports = exp,
              _resultScoperState = scoperSt,
              _resultScope = sc
            }

scopeCheckRepl ::
  forall r a b.
  (Members '[Error JuvixError, NameIdGen, Reader PackageId, State Scope, State ScoperState] r) =>
  ( forall r'.
    (Members '[HighlightBuilder, Reader ScopeParameters, Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, Reader InfoTable, NameIdGen, Reader BindingStrategy, Reader PackageId] r') =>
    a ->
    Sem r' b
  ) ->
  ScopedModuleTable ->
  InfoTable ->
  a ->
  Sem r b
scopeCheckRepl check importTab tab a =
  mapError (JuvixError @ScoperError)
    . evalHighlightBuilder
    . evalInfoTableBuilder tab
    . runReader iniScopeParameters
    . runReader tab'
    . localBindings
    $ check a
  where
    tab' = computeCombinedInfoTable importTab

    iniScopeParameters :: ScopeParameters
    iniScopeParameters =
      ScopeParameters
        { _scopeImportedModules = importTab ^. scopedModuleTable
        }

-- TODO refactor to have less code duplication
scopeCheckExpressionAtoms ::
  forall r.
  (Members '[Error JuvixError, NameIdGen, Reader PackageId, State Scope, State ScoperState] r) =>
  ScopedModuleTable ->
  InfoTable ->
  ExpressionAtoms 'Parsed ->
  Sem r (ExpressionAtoms 'Scoped)
scopeCheckExpressionAtoms = scopeCheckRepl checkExpressionAtoms

scopeCheckExpression ::
  forall r.
  (Members '[Error JuvixError, NameIdGen, Reader PackageId, State Scope, State ScoperState] r) =>
  ScopedModuleTable ->
  InfoTable ->
  ExpressionAtoms 'Parsed ->
  Sem r Expression
scopeCheckExpression = scopeCheckRepl checkParseExpressionAtoms

scopeCheckImport ::
  forall r.
  (Members '[Error JuvixError, NameIdGen, Reader PackageId, State Scope, State ScoperState] r) =>
  ScopedModuleTable ->
  InfoTable ->
  Import 'Parsed ->
  Sem r (Import 'Scoped)
scopeCheckImport = scopeCheckRepl checkImport

scopeCheckOpenModule ::
  forall r.
  (Members '[Error JuvixError, InfoTableBuilder, Reader InfoTable, NameIdGen, State Scope, Reader ScopeParameters, State ScoperState] r) =>
  OpenModule 'Parsed 'OpenFull ->
  Sem r (OpenModule 'Scoped 'OpenFull)
scopeCheckOpenModule = mapError (JuvixError @ScoperError) . checkOpenModule

freshVariable :: (Members '[NameIdGen, State Scope, State ScoperState] r) => Symbol -> Sem r S.Symbol
freshVariable = freshSymbol KNameLocal KNameLocal False

freshSymbol ::
  forall r.
  (Members '[State Scope, State ScoperState, NameIdGen] r) =>
  NameKind ->
  NameKind ->
  Bool ->
  Symbol ->
  Sem r S.Symbol
freshSymbol _nameKind _nameKindPretty _nameTop _nameConcrete = do
  _nameId <- freshNameId
  _nameDefinedIn <- gets (^. scopePath)
  let _nameDefined = getLoc _nameConcrete
      _nameWhyInScope = S.BecauseDefined
      _nameVisibilityAnn = VisPublic
      _nameVerbatim = _nameConcrete ^. symbolText
  return
    S.Name'
      { _nameFixity = Nothing,
        _nameIterator = Nothing,
        ..
      }

reserveSymbolSignatureOfNameSpace ::
  forall r d ns.
  ( Members '[Error ScoperError, NameIdGen, State Scope, State ScoperState, Reader BindingStrategy, InfoTableBuilder, Reader InfoTable] r,
    HasNameSignature 'Parsed d,
    SingI ns
  ) =>
  SNameSpace ns ->
  NameKind ->
  NameKind ->
  d ->
  Maybe BuiltinPrim ->
  Symbol ->
  Sem r S.Symbol
reserveSymbolSignatureOfNameSpace ns kind kindPretty d builtin s = do
  sig <- mkNameSignature d
  reserveSymbolOfNameSpace ns kind kindPretty (Just sig) builtin s

reserveSymbolSignatureOf ::
  forall (k :: NameKind) r d.
  ( Members '[Error ScoperError, NameIdGen, State Scope, State ScoperState, Reader BindingStrategy, InfoTableBuilder, Reader InfoTable] r,
    HasNameSignature 'Parsed d,
    SingI (NameKindNameSpace k)
  ) =>
  Sing k ->
  d ->
  Maybe BuiltinPrim ->
  Symbol ->
  Sem r S.Symbol
reserveSymbolSignatureOf k d builtin s = do
  sig <- mkNameSignature d
  reserveSymbolOf k (Just sig) builtin s

registerNameSignature ::
  (Members '[State ScoperState, Error ScoperError, InfoTableBuilder] r, HasNameSignature 'Scoped d) =>
  S.NameId ->
  d ->
  Sem r ()
registerNameSignature uid d = do
  sig <- mkNameSignature d
  registerNameSig uid sig

registerConstructorSignature ::
  (Members '[State ScoperState, Error ScoperError, InfoTableBuilder] r) =>
  S.NameId ->
  RecordNameSignature 'Scoped ->
  Sem r ()
registerConstructorSignature uid sig = do
  modify' (set (scoperScopedConstructorFields . at uid) (Just sig))
  registerConstructorSig uid sig

registerProjectionSignature ::
  (Members '[State ScoperState, Error ScoperError, InfoTableBuilder] r) =>
  ProjectionDef 'Scoped ->
  Sem r ()
registerProjectionSignature p =
  registerNameSignature (p ^. projectionField . S.nameId) p

reserveSymbolOfNameSpace ::
  forall (ns :: NameSpace) r.
  ( Members
      '[ Error ScoperError,
         NameIdGen,
         State Scope,
         State ScoperState,
         Reader BindingStrategy,
         InfoTableBuilder,
         Reader InfoTable
       ]
      r,
    SingI ns
  ) =>
  SNameSpace ns ->
  NameKind ->
  NameKind ->
  Maybe (NameSignature 'Parsed) ->
  Maybe BuiltinPrim ->
  Symbol ->
  Sem r S.Symbol
reserveSymbolOfNameSpace ns kind kindPretty nameSig builtin s = do
  checkNotBound
  strat <- ask
  let isTop = case strat of
        BindingLocal -> False
        BindingTop -> True
  s' <- freshSymbol kind kindPretty isTop s
  whenJust builtin (`registerBuiltin` s')
  whenJust nameSig (modify' . set (scoperNameSignatures . at (s' ^. S.nameId)) . Just)
  whenJust nameSig (registerParsedNameSig (s' ^. S.nameId))
  registerName isTop s'
  modify (set (scopeReservedNameSpace sns . at s) (Just s'))
  addToScope ns kind s s'
  return s'
  where
    sns :: Sing ns = sing
    checkNotBound :: Sem r ()
    checkNotBound = do
      exists <- gets (^. scopeReservedNameSpace sns . at s)
      whenJust exists $ \d ->
        throw
          ( ErrMultipleDeclarations
              MultipleDeclarations
                { _multipleDeclSecond = s,
                  _multipleDeclFirst = getLoc d
                }
          )

addToScope ::
  forall r ns.
  ( Members
      '[ State Scope,
         Reader BindingStrategy
       ]
      r
  ) =>
  SNameSpace ns ->
  NameKind ->
  Symbol ->
  S.Symbol ->
  Sem r ()
addToScope ns kind s s' = withSingI ns $ do
  strat <- ask
  path <- gets (^. scopePath)
  let u :: S.Name = S.unqualifiedSymbol s'
      entry :: NameSpaceEntryType ns
      entry =
        let symE
              | isAlias = PreSymbolAlias (Alias u)
              | otherwise = PreSymbolFinal (SymbolEntry u)
            modE = ModuleSymbolEntry u
            fixE = FixitySymbolEntry u
         in case ns of
              SNameSpaceSymbols -> symE
              SNameSpaceModules -> modE
              SNameSpaceFixities -> fixE

      addS :: NameSpaceEntryType ns -> Maybe (SymbolInfo ns) -> SymbolInfo ns
      addS mentry m = case m of
        Nothing -> symbolInfoSingle mentry
        Just SymbolInfo {..} -> case strat of
          BindingLocal -> symbolInfoSingle mentry
          BindingTop -> SymbolInfo (HashMap.insert path mentry _symbolInfo)
  modify (over scopeNameSpaceI (HashMap.alter (Just . addS entry) s))
  where
    isAlias = case kind of
      KNameAlias -> True
      _ -> False

reserveSymbolOf ::
  forall (nameKind :: NameKind) r.
  ( Members
      '[ Error ScoperError,
         NameIdGen,
         State Scope,
         State ScoperState,
         Reader BindingStrategy,
         InfoTableBuilder,
         Reader InfoTable
       ]
      r,
    SingI (NameKindNameSpace nameKind)
  ) =>
  Sing nameKind ->
  Maybe (NameSignature 'Parsed) ->
  Maybe BuiltinPrim ->
  Symbol ->
  Sem r S.Symbol
reserveSymbolOf k =
  reserveSymbolOfNameSpace
    (sing :: Sing (NameKindNameSpace nameKind))
    (fromSing k)
    (fromSing k)

getReservedSymbol ::
  (HasCallStack, Members '[State Scope] r) =>
  NameSpace ->
  Symbol ->
  Sem r S.Symbol
getReservedSymbol dns s = withSomeSing dns $ \ns -> do
  m <- gets (^. scopeReserved . reservedNameSpace ns)
  let s' = fromMaybe err (m ^. at s)
      err = impossibleError (nameSpaceElemName dns <> " " <> ppTrace s <> " not found in the scope. Contents of scope:\n" <> ppTrace (toList m))
  getFixityAndIterator s'

getReservedFixitySymbol ::
  (HasCallStack, Members '[State Scope] r) =>
  Symbol ->
  Sem r S.Symbol
getReservedFixitySymbol = getReservedSymbol NameSpaceFixities

getReservedLocalModuleSymbol ::
  (HasCallStack, Members '[State Scope] r) =>
  Symbol ->
  Sem r S.Symbol
getReservedLocalModuleSymbol = getReservedSymbol NameSpaceModules

getReservedDefinitionSymbol ::
  forall r.
  (HasCallStack) =>
  (Members '[State Scope] r) =>
  Symbol ->
  Sem r S.Symbol
getReservedDefinitionSymbol = getReservedSymbol NameSpaceSymbols

-- | Variables are assumed to never be infix operators
bindVariableSymbol ::
  (Members '[Error ScoperError, NameIdGen, State Scope, InfoTableBuilder, Reader InfoTable, State ScoperState] r) =>
  Symbol ->
  Sem r S.Symbol
bindVariableSymbol = localBindings . reserveSymbolOf SKNameLocal Nothing Nothing

reserveInductiveSymbol ::
  (Members '[Error ScoperError, NameIdGen, State Scope, State ScoperState, Reader BindingStrategy, InfoTableBuilder, Reader InfoTable] r) =>
  InductiveDef 'Parsed ->
  Sem r S.Symbol
reserveInductiveSymbol d =
  reserveSymbolSignatureOf
    SKNameInductive
    d
    (toBuiltinPrim <$> d ^. inductiveBuiltin)
    (d ^. inductiveName)

reserveAliasSymbol ::
  (Members '[Error ScoperError, NameIdGen, State Scope, Reader BindingStrategy, InfoTableBuilder, Reader InfoTable, State ScoperState] r) =>
  AliasDef 'Parsed ->
  Sem r S.Symbol
reserveAliasSymbol a = do
  s <- reserveSymbolOf SKNameAlias Nothing Nothing (a ^. aliasDefName)
  let locAliasDef = getLoc a
  return (set S.nameDefined locAliasDef s)

reserveProjectionSymbol ::
  (Members '[Error ScoperError, NameIdGen, State Scope, Reader BindingStrategy, InfoTableBuilder, Reader InfoTable, State ScoperState] r) =>
  ProjectionDef 'Parsed ->
  Sem r S.Symbol
reserveProjectionSymbol d =
  reserveSymbolSignatureOf SKNameFunction d (toBuiltinPrim <$> d ^. projectionFieldBuiltin) (d ^. projectionField)

reserveConstructorSymbol ::
  (Members '[Error ScoperError, NameIdGen, State Scope, State ScoperState, Reader BindingStrategy, InfoTableBuilder, Reader InfoTable] r) =>
  InductiveDef 'Parsed ->
  ConstructorDef 'Parsed ->
  Maybe BuiltinConstructor ->
  Sem r S.Symbol
reserveConstructorSymbol d c b = do
  reserveSymbolSignatureOf SKNameConstructor (d, c) (toBuiltinPrim <$> b) (c ^. constructorName)

reserveFunctionSymbol ::
  (Members '[Error ScoperError, NameIdGen, State Scope, State ScoperState, Reader BindingStrategy, InfoTableBuilder, Reader InfoTable] r) =>
  FunctionLhs 'Parsed ->
  Sem r S.Symbol
reserveFunctionSymbol f =
  reserveSymbolSignatureOf SKNameFunction f (toBuiltinPrim <$> f ^. funLhsBuiltin) (f ^?! funLhsName . _FunctionDefName)

reserveAxiomSymbol ::
  (Members '[Error ScoperError, NameIdGen, State Scope, State ScoperState, Reader BindingStrategy, InfoTableBuilder, Reader InfoTable] r) =>
  AxiomDef 'Parsed ->
  Sem r S.Symbol
reserveAxiomSymbol a =
  reserveSymbolSignatureOfNameSpace SNameSpaceSymbols KNameAxiom kindPretty a (toBuiltinPrim <$> a ^. axiomBuiltin) (a ^. axiomName)
  where
    kindPretty :: NameKind
    kindPretty = maybe KNameAxiom getNameKind (a ^? axiomBuiltin . _Just . withLocParam)

reserveDerivingSymbol ::
  (Members '[Error ScoperError, NameIdGen, State Scope, State ScoperState, Reader BindingStrategy, InfoTableBuilder, Reader InfoTable] r) =>
  Deriving 'Parsed ->
  Sem r ()
reserveDerivingSymbol f = do
  let lhs = f ^. derivingFunLhs
  when (P.isLhsFunctionRecursive lhs) $
    void (reserveFunctionSymbol lhs)

reserveFunctionLikeSymbol ::
  (Members '[Error ScoperError, NameIdGen, State Scope, State ScoperState, Reader BindingStrategy, InfoTableBuilder, Reader InfoTable] r) =>
  FunctionDef 'Parsed ->
  Sem r ()
reserveFunctionLikeSymbol f =
  when (P.isFunctionRecursive f) $
    void (reserveFunctionSymbol (f ^. functionDefLhs))

reservePatternFunctionSymbols ::
  forall r.
  (Members '[Error ScoperError, NameIdGen, State Scope, State ScoperState, Reader BindingStrategy, InfoTableBuilder, Reader InfoTable] r) =>
  PatternAtomType 'Parsed ->
  Sem r ()
reservePatternFunctionSymbols = goAtom
  where
    goAtom :: PatternAtom 'Parsed -> Sem r ()
    goAtom = \case
      PatternAtomIden iden -> void (reservePatternName iden)
      PatternAtomWildcard {} -> return ()
      PatternAtomEmpty {} -> return ()
      PatternAtomList x -> goList x
      PatternAtomWildcardConstructor {} -> return ()
      PatternAtomRecord x -> goRecord x
      PatternAtomParens x -> goAtoms x
      PatternAtomBraces x -> goAtoms x
      PatternAtomDoubleBraces x -> goAtoms x
      PatternAtomAt x -> goAt x

    goList :: ListPattern 'Parsed -> Sem r ()
    goList ListPattern {..} = mapM_ goAtoms _listpItems

    goRecord :: RecordPattern 'Parsed -> Sem r ()
    goRecord RecordPattern {..} = mapM_ goRecordItem _recordPatternItems

    goRecordItem :: RecordPatternItem 'Parsed -> Sem r ()
    goRecordItem = \case
      RecordPatternItemFieldPun PatternFieldPun {..} -> do
        void (reservePatternName (NameUnqualified _fieldPunField))
      RecordPatternItemAssign RecordPatternAssign {..} -> do
        goAtoms _recordPatternAssignPattern

    goAtoms :: PatternAtoms 'Parsed -> Sem r ()
    goAtoms PatternAtoms {..} = mapM_ goAtom _patternAtoms

    goAt :: PatternBinding -> Sem r ()
    goAt PatternBinding {..} = do
      void (reservePatternName (NameUnqualified _patternBindingName))
      goAtom _patternBindingPattern

reserveImport ::
  forall r.
  ( Members
      '[ HighlightBuilder,
         Error ScoperError,
         State Scope,
         Reader ScopeParameters,
         State ScoperState,
         InfoTableBuilder,
         Reader InfoTable,
         NameIdGen,
         Reader BindingStrategy,
         Reader PackageId
       ]
      r
  ) =>
  Import 'Parsed ->
  Sem r ()
reserveImport i@Import {..} = case _importPublic of
  NoPublic -> return ()
  Public {} -> reserveImportPublic i

checkImport ::
  forall r.
  ( Members
      '[ HighlightBuilder,
         Error ScoperError,
         State Scope,
         Reader ScopeParameters,
         State ScoperState,
         InfoTableBuilder,
         Reader InfoTable,
         NameIdGen,
         Reader BindingStrategy,
         Reader PackageId
       ]
      r
  ) =>
  Import 'Parsed ->
  Sem r (Import 'Scoped)
checkImport i@Import {..} = case _importPublic of
  NoPublic -> checkImportNoPublic i
  Public {} -> checkImportPublic i

reserveImportPublic ::
  forall r.
  ( Members
      '[ Error ScoperError,
         State Scope,
         Reader ScopeParameters,
         State ScoperState,
         InfoTableBuilder,
         Reader InfoTable,
         NameIdGen,
         HighlightBuilder,
         Reader BindingStrategy,
         Reader PackageId
       ]
      r
  ) =>
  Import 'Parsed ->
  Sem r ()
reserveImportPublic i@Import {..} = do
  let locMod :: Module 'Parsed 'ModuleLocal =
        localModule (splitName outerOpenModuleName)
  reserveLocalModule locMod
  where
    gen :: forall a. Sem '[Reader Interval] a -> a
    gen = run . runReader loc

    loc :: Interval
    loc = getLoc i

    outerOpenModuleName :: Name
    outerOpenModuleName = topModulePathToName (fromMaybe _importModulePath _importAsName)

    innerOpen :: OpenModule 'Parsed 'OpenFull
    innerOpen = gen $ do
      _openModuleKw <- G.kw G.kwOpen
      let _openModuleName = topModulePathToName _importModulePath
      pubKw <- Irrelevant <$> G.kw G.kwPublic
      let
      return
        OpenModule
          { _openModuleKw,
            _openModuleUsingHiding = _importUsingHiding,
            _openModulePublic = Public pubKw,
            _openModuleName
          }

    singletonModule :: Symbol -> Statement 'Parsed -> Module 'Parsed 'ModuleLocal
    singletonModule modName stm = gen $ do
      _moduleKw <- G.kw G.kwModule
      _moduleKwEnd <- G.kw G.kwEnd
      let _moduleId = ()
          _moduleBody = [stm]
      return
        Module
          { _moduleDoc = Nothing,
            _modulePragmas = Nothing,
            _moduleOrigin = LocalModuleType,
            _moduleMarkdownInfo = Nothing,
            _modulePath = modName,
            ..
          }

    localModule :: ([Symbol], Symbol) -> Module 'Parsed 'ModuleLocal
    localModule (qualf, m) = case qualf of
      [] -> singletonModule m (StatementOpenModule innerOpen)
      n : ns -> singletonModule n (StatementModule (localModule (ns, m)))

checkImportPublic ::
  forall r.
  ( HasCallStack,
    Members
      '[ Error ScoperError,
         State Scope,
         Reader ScopeParameters,
         State ScoperState,
         InfoTableBuilder,
         Reader InfoTable,
         NameIdGen,
         HighlightBuilder,
         Reader BindingStrategy,
         Reader PackageId
       ]
      r
  ) =>
  Import 'Parsed ->
  Sem r (Import 'Scoped)
checkImportPublic i@Import {..} = do
  let outerImport =
        Import
          { _importKw,
            _importModulePath,
            _importAsName = Nothing,
            _importUsingHiding = Nothing,
            _importPublic = NoPublic,
            _importOpen = Nothing
          }
  outerImport' <- checkImportNoPublic outerImport
  let locMod :: Module 'Parsed 'ModuleLocal =
        localModule (splitName outerOpenModuleName)
  local' <- checkLocalModule locMod
  let (innerModuleName', usingHiding') = extract local'
  mouterOpen' :: Maybe (OpenModule 'Scoped 'OpenShort) <-
    fmap (set openModuleName ()) <$> mapM checkOpenModule outerOpen
  let asName' :: Maybe S.TopModulePath = do
        topModule :: TopModulePath <- _importAsName
        return (set nameConcrete topModule innerModuleName')
  return
    Import
      { _importKw,
        _importPublic,
        _importModulePath = outerImport' ^. importModulePath,
        _importAsName = asName',
        _importOpen = mouterOpen',
        _importUsingHiding = usingHiding'
      }
  where
    gen :: forall a. Sem '[Reader Interval] a -> a
    gen = run . runReader loc

    loc :: Interval
    loc = getLoc i

    -- Extracts the name of the innermost module as well as the scoped UsingHiding of the inner open
    extract :: Module 'Scoped 'ModuleLocal -> (S.Symbol, Maybe (UsingHiding 'Scoped))
    extract m = case m ^. moduleBody of
      [StatementOpenModule OpenModule {..}] -> (m ^. modulePath, _openModuleUsingHiding)
      [StatementModule inner] -> extract inner
      _ -> error "impossible. When generating this module we always put a single statement"

    outerOpenModuleName :: Name
    outerOpenModuleName = topModulePathToName (fromMaybe _importModulePath _importAsName)

    outerOpen :: Maybe (OpenModule 'Parsed 'OpenFull)
    outerOpen = do
      OpenModule {..} <- _importOpen
      return
        OpenModule
          { _openModuleKw,
            _openModulePublic,
            _openModuleName = outerOpenModuleName,
            _openModuleUsingHiding
          }

    innerOpen :: OpenModule 'Parsed 'OpenFull
    innerOpen = gen $ do
      _openModuleKw <- G.kw G.kwOpen
      let _openModuleName = topModulePathToName _importModulePath
      pubKw <- Irrelevant <$> G.kw G.kwPublic
      let
      return
        OpenModule
          { _openModuleKw,
            _openModuleUsingHiding = _importUsingHiding,
            _openModulePublic = Public pubKw,
            _openModuleName
          }

    singletonModule :: Symbol -> Statement 'Parsed -> Module 'Parsed 'ModuleLocal
    singletonModule modName stm = gen $ do
      _moduleKw <- G.kw G.kwModule
      _moduleKwEnd <- G.kw G.kwEnd
      let _moduleId = ()
          _moduleBody = [stm]
      return
        Module
          { _moduleDoc = Nothing,
            _modulePragmas = Nothing,
            _moduleOrigin = LocalModuleType,
            _moduleMarkdownInfo = Nothing,
            _modulePath = modName,
            ..
          }

    localModule :: ([Symbol], Symbol) -> Module 'Parsed 'ModuleLocal
    localModule (qualf, m) = case qualf of
      [] -> singletonModule m (StatementOpenModule innerOpen)
      n : ns -> singletonModule n (StatementModule (localModule (ns, m)))

checkImportNoPublic ::
  forall r.
  (Members '[Error ScoperError, State Scope, Reader ScopeParameters, State ScoperState, InfoTableBuilder, Reader InfoTable, NameIdGen] r) =>
  Import 'Parsed ->
  Sem r (Import 'Scoped)
checkImportNoPublic import_@Import {..} = do
  massert (_importPublic == NoPublic)
  smodule <- readScopeModule import_
  let sname :: S.TopModulePath = set nameConcrete _importModulePath (smodule ^. scopedModulePath)
      sname' :: S.Name = set S.nameConcrete (topModulePathToName _importModulePath) sname
      scopedModule :: ScopedModule = set scopedModuleName sname' smodule
      exportInfoOriginal = scopedModule ^. scopedModuleExportInfo
      importName :: S.TopModulePath = set S.nameConcrete _importModulePath sname
      synonymName :: Maybe S.TopModulePath = do
        synonym <- _importAsName
        return (set S.nameConcrete synonym sname)
      qual' :: Maybe S.TopModulePath
      qual' = do
        asName <- _importAsName
        return (set S.nameConcrete asName sname')
  registerName False importName
  whenJust synonymName (registerName False)
  registerScoperModules scopedModule
  importOpen' <- mapM (checkOpenModuleShort (scopedModuleToModuleExportInfo scopedModule)) _importOpen
  usingHiding' <- mapM (checkUsingHiding sname' exportInfoOriginal) _importUsingHiding
  let exportInfoFiltered :: ExportInfo = filterExportInfo _importPublic usingHiding' exportInfoOriginal
      filteredScopedModule = set scopedModuleExportInfo exportInfoFiltered scopedModule
  addModuleToScope filteredScopedModule
  return
    Import
      { _importModulePath = sname,
        _importAsName = qual',
        _importOpen = importOpen',
        _importUsingHiding = usingHiding',
        _importKw,
        _importPublic
      }
  where
    addModuleToScope :: ScopedModule -> Sem r ()
    addModuleToScope smod = do
      let mpath :: TopModulePathKey = modulePathTypeKey (fromMaybe _importModulePath _importAsName)
          uid :: S.NameId = smod ^. scopedModuleName . S.nameId
          singTbl = HashMap.singleton uid smod
      modify (over (scopeImports . at mpath) (Just . maybe singTbl (HashMap.insert uid smod)))

    registerScoperModules :: ScopedModule -> Sem r ()
    registerScoperModules m = do
      modify (set (scoperModules . at (m ^. scopedModulePath . S.nameId)) (Just m))
      modify (set (scoperExportInfo . at (m ^. scopedModulePath . S.nameId)) (Just (scopedModuleToModuleExportInfo m)))
      forM_ (m ^. scopedModuleLocalModules) registerScoperModules

getTopModulePath :: Module 'Parsed 'ModuleTop -> AbsModulePath
getTopModulePath Module {..} =
  AbsModulePath
    { _absTopModulePath = _modulePath,
      _absLocalPath = mempty
    }

getModuleExportInfo :: forall r. (HasCallStack, Members '[State ScoperState] r) => ModuleSymbolEntry -> Sem r ModuleExportInfo
getModuleExportInfo m =
  fromMaybeM err $
    gets (^. scoperExportInfo . at (m ^. moduleEntry . S.nameId))
  where
    err :: Sem r a
    err = do
      ms <- toList <$> gets (^. scoperReservedModules)
      impossibleError
        ( "Could not find "
            <> ppTrace m
            <> "\nExport Infos in the state: "
            <> ppTrace ms
        )

getReservedLocalModule :: forall r. (HasCallStack, Members '[State ScoperState] r) => ModuleSymbolEntry -> Sem r ReservedModule
getReservedLocalModule m = fromMaybeM err (gets (^. scoperReservedModules . at (m ^. moduleEntry . S.nameId)))
  where
    err :: Sem r a
    err = do
      ms <- toList <$> gets (^. scoperReservedModules)
      impossibleError
        ( "Could not find "
            <> ppTrace m
            <> "\nModules in the state: "
            <> ppTrace ms
        )

lookupLocalSymbolAux ::
  forall r.
  (HasCallStack, Members '[State ScoperState, State Scope, Output ModuleSymbolEntry, Output PreSymbolEntry, Output FixitySymbolEntry] r) =>
  (S.WhyInScope -> Bool) ->
  [Symbol] ->
  Symbol ->
  Sem r ()
lookupLocalSymbolAux whyInScope modules final =
  case modules of
    [] ->
      lookHere
    p : ps -> do
      entries <- gets (^.. scopeModuleSymbols . at p . _Just . symbolInfo . each)
      let entries' = filter (whyInScope . (^. moduleEntry . S.nameWhyInScope)) entries
      mapM_ (getModuleExportInfo >=> lookInExport final ps) entries'
  where
    lookHere :: Sem r ()
    lookHere = do
      let helper ::
            forall ns r'.
            (SingI ns, Members '[Output (NameSpaceEntryType ns), State Scope] r') =>
            Proxy ns ->
            Sem r' ()
          helper Proxy = do
            entries <- gets (^.. scopeNameSpaceI @ns . at final . _Just . symbolInfo . each)
            let entries' = filter (whyInScope . (^. entryName . S.nameWhyInScope)) entries
            mapM_ output entries'
      helper (Proxy @'NameSpaceSymbols)
      helper (Proxy @'NameSpaceModules)
      helper (Proxy @'NameSpaceFixities)

-- | Do not call directly. Looks for a symbol in (possibly) nested local modules
lookupSymbolAux ::
  forall r.
  (HasCallStack, Members '[State ScoperState, State Scope, Output ModuleSymbolEntry, Output PreSymbolEntry, Output FixitySymbolEntry] r) =>
  [Symbol] ->
  Symbol ->
  Sem r ()
lookupSymbolAux modules final = do
  hereOrInLocalModule
  importedTopModule
  where
    hereOrInLocalModule :: Sem r ()
    hereOrInLocalModule = do
      path0 <- gets (^. scopePath)
      let topPath = path0 ^. absTopModulePath
          path1 = topPath ^. modulePathDir ++ [topPath ^. modulePathName]
          path2 = path0 ^. absLocalPath
          pref = commonPrefix path2 modules
      when (path1 `isPrefixOf` modules) $ do
        let modules' = drop (length path1) modules
            pref' = commonPrefix path2 modules'
        lookPrefix pref' path2 modules'
      when (notNull pref) $
        lookPrefix pref path2 modules
      lookupLocalSymbolAux (const True) modules final

    lookPrefix :: [Symbol] -> [Symbol] -> [Symbol] -> Sem r ()
    lookPrefix pref path modules' = do
      let prefLen = length pref
          inheritDepth = length path - prefLen
          modules'' = drop prefLen modules'
      lookupLocalSymbolAux
        (== iterate S.BecauseInherited S.BecauseDefined !! inheritDepth)
        modules''
        final

    importedTopModule :: Sem r ()
    importedTopModule = do
      tbl <- gets (^. scopeImports)
      mapM_ output (tbl ^.. at path . _Just . each . to mkModuleEntry)
      where
        path = topModulePathKey (TopModulePath modules final)

mkModuleEntry :: ScopedModule -> ModuleSymbolEntry
mkModuleEntry m = ModuleSymbolEntry (m ^. scopedModuleName)

lookInExport ::
  forall r.
  (Members '[State ScoperState, Output PreSymbolEntry, Output ModuleSymbolEntry, Output FixitySymbolEntry] r) =>
  Symbol ->
  [Symbol] ->
  ModuleExportInfo ->
  Sem r ()
lookInExport sym remaining modExport = case remaining of
  [] -> do
    whenJust (e ^. exportSymbols . at sym) output
    whenJust (e ^. exportModuleSymbols . at sym) output
    whenJust (e ^. exportFixitySymbols . at sym) output
  s : ss -> whenJustM (mayModule e s) (lookInExport sym ss)
  where
    e :: ExportInfo = modExport ^. moduleExportInfo
    mayModule :: ExportInfo -> Symbol -> Sem r (Maybe ModuleExportInfo)
    mayModule ExportInfo {..} s =
      mapM getModuleExportInfo (_exportModuleSymbols ^. at s)

-- | We return a list of entries because qualified names can point to different
-- modules due to nesting.
lookupQualifiedSymbol ::
  forall r.
  (Members '[State Scope, State ScoperState] r) =>
  ([Symbol], Symbol) ->
  Sem r (HashSet PreSymbolEntry, HashSet ModuleSymbolEntry, HashSet FixitySymbolEntry)
lookupQualifiedSymbol sms = do
  (es, (ms, fs)) <-
    runOutputHashSet
      . runOutputHashSet
      . execOutputHashSet
      $ go sms
  return (es, ms, fs)
  where
    go ::
      forall r'.
      (Members '[State ScoperState, State Scope, Output PreSymbolEntry, Output ModuleSymbolEntry, Output FixitySymbolEntry] r') =>
      ([Symbol], Symbol) ->
      Sem r' ()
    go (path, sym) = do
      here
      there
      where
        -- Current module.
        here :: Sem r' ()
        here = lookupSymbolAux path sym
        -- Looks for top level modules
        there :: Sem r' ()
        there = mapM_ (uncurry lookInTopModule) allTopPaths
          where
            allTopPaths :: [(TopModulePathKey, [Symbol])]
            allTopPaths = map (first nonEmptyToTopPath) raw
              where
                lpath = toList path
                raw :: [(NonEmpty Symbol, [Symbol])]
                raw =
                  [ (l, r) | i <- [1 .. length path], (Just l, r) <- [first nonEmpty (splitAt i lpath)]
                  ]
                nonEmptyToTopPath :: NonEmpty Symbol -> TopModulePathKey
                nonEmptyToTopPath lsym = TopModulePathKey (NonEmpty.init l) (NonEmpty.last l)
                  where
                    l = (^. symbolText) <$> lsym

            lookInTopModule :: TopModulePathKey -> [Symbol] -> Sem r' ()
            lookInTopModule topPath remaining = do
              tbl <- gets (^. scopeImports)
              sequence_
                [ lookInExport sym remaining (scopedModuleToModuleExportInfo ref)
                  | Just t <- [tbl ^. at topPath],
                    ref <- toList t
                ]

-- | This assumes that alias do not have cycles.
normalizePreSymbolEntry :: (Members '[State ScoperState] r) => PreSymbolEntry -> Sem r S.Name
normalizePreSymbolEntry = \case
  PreSymbolFinal a -> return (a ^. symbolEntry)
  PreSymbolAlias a -> fromMaybe err <$> gets (^? scoperAlias . at (a ^. aliasName . S.nameId) . _Just . scopedIdenFinal)
    where
      err :: forall a. a
      err = impossibleError ("The alias " <> ppTrace (a ^. aliasName) <> " was not found in the ScoperState ")

checkQualifiedName ::
  (Members '[Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, Reader InfoTable] r) =>
  QualifiedName ->
  Sem r PreSymbolEntry
checkQualifiedName q@(QualifiedName (SymbolPath p) sym) = do
  es <- fst3 <$> lookupQualifiedSymbol (toList p, sym)
  case toList es of
    [] -> notInScope
    [e] -> return e
    _ -> throw (ErrAmbiguousSym (AmbiguousSym q' (toList es)))
  where
    q' = NameQualified q
    notInScope = throw (ErrQualSymNotInScope (QualSymNotInScope q))

entryToScopedIden ::
  forall ns r.
  (SingI ns, Members '[State Scope, InfoTableBuilder, State ScoperState] r) =>
  Name ->
  NameSpaceEntryType ns ->
  Sem r ScopedIden
entryToScopedIden name e = do
  let setConcrete :: S.Name' x -> S.Name
      setConcrete = set S.nameConcrete name

      scopedName :: S.Name
      scopedName = setConcrete (e ^. entryName)
  si0 <-
    let noAlias =
          ScopedIden
            { _scopedIdenFinal = scopedName,
              _scopedIdenAlias = Nothing
            }
     in case sing :: SNameSpace ns of
          SNameSpaceModules -> return noAlias
          SNameSpaceFixities -> return noAlias
          SNameSpaceSymbols ->
            case e of
              PreSymbolFinal {} -> return noAlias
              PreSymbolAlias {} -> do
                e' <- normalizePreSymbolEntry e
                let scopedName' = set S.nameKind (getNameKind e') scopedName
                return
                  ScopedIden
                    { _scopedIdenAlias = Just scopedName',
                      _scopedIdenFinal = setConcrete e'
                    }
  si <- scopedIdenSrcName getFixityAndIterator si0
  registerScopedIden False si
  return si

getFixityAndIterator ::
  forall c r.
  (Members '[State Scope] r) =>
  S.Name' c ->
  Sem r (S.Name' c)
getFixityAndIterator n = execState n $ do
  let uid = n ^. S.nameId
  whenJustM (gets (^. scopeFixities . at uid)) (modify @(S.Name' c) . set S.nameFixity . Just)
  whenJustM (gets (^. scopeIterators . at uid)) (modify @(S.Name' c) . set S.nameIterator . Just)

-- | We gather all symbols which have been defined or marked to be public in the given scope.
genExportInfo ::
  forall r.
  (Members '[State Scope, Error ScoperError] r) =>
  Sem r ExportInfo
genExportInfo = do
  scope <- get
  let mkHashMap ::
        forall ns.
        (SingI ns) =>
        (Lens' Scope (HashMap Symbol (SymbolInfo ns))) ->
        Sem r (HashMap Symbol (NameSpaceEntryType ns))
      mkHashMap l = hashMap <$> mapMaybeM (mkentry (scope ^. scopePath)) (HashMap.toList (scope ^. l))
  _exportSymbols :: HashMap Symbol PreSymbolEntry <- mkHashMap scopeSymbols >>= (traversed . preSymbolName) getFixityAndIterator
  _exportModuleSymbols <- mkHashMap scopeModuleSymbols
  _exportFixitySymbols <- mkHashMap scopeFixitySymbols
  return ExportInfo {..}
  where
    mkentry ::
      forall ns.
      (SingI ns) =>
      AbsModulePath ->
      (Symbol, SymbolInfo ns) ->
      Sem r (Maybe (Symbol, NameSpaceEntryType ns))
    mkentry _scopePath (s, SymbolInfo {..}) =
      case filter shouldExport (toList _symbolInfo) of
        [] -> return Nothing
        [e] -> return (Just (s, e))
        e : es -> err (e :| es)
      where
        err :: NonEmpty (NameSpaceEntryType ns) -> Sem r a
        err es =
          throw $
            ErrMultipleExport
              MultipleExportConflict
                { _multipleExportModule = _scopePath,
                  _multipleExportSymbol = s,
                  _multipleExportNameSpace = fromSing (sing :: SNameSpace ns),
                  _multipleExportEntries = case sing :: SNameSpace ns of
                    SNameSpaceSymbols -> ExportEntriesSymbols es
                    SNameSpaceModules -> ExportEntriesModules es
                    SNameSpaceFixities -> ExportEntriesFixities es
                }

getScopedLocalModules :: (Member (State ScoperState) r) => ExportInfo -> Sem r (HashMap S.NameId ScopedModule)
getScopedLocalModules ExportInfo {..} = do
  mds <- gets (^. scoperModules)
  return . hashMap . mapMaybe (fetch mds) $ toList _exportModuleSymbols
  where
    fetch :: HashMap NameId ScopedModule -> ModuleSymbolEntry -> Maybe (NameId, ScopedModule)
    fetch mds ModuleSymbolEntry {..} = do
      m <- mds ^. at n
      return (n, m)
      where
        n = _moduleEntry ^. S.nameId

readScopeModule ::
  forall r.
  (Members '[Error ScoperError, Reader ScopeParameters, NameIdGen, State ScoperState, InfoTableBuilder, Reader InfoTable] r) =>
  Import 'Parsed ->
  Sem r ScopedModule
readScopeModule import_ = do
  mods <- ask
  let err :: a
      err = do
        error
          ( "unexpected error in "
              <> ppTrace (getLoc import_)
              <> "\n"
              <> "could not find module "
              <> ppTrace (import_ ^. importModulePath)
              <> "\nAvailable modules:\n "
              <> show (HashMap.keys (mods ^. scopeImportedModules))
          )
  let path = modulePathTypeKey (import_ ^. importModulePath)
  return (fromMaybe err (mods ^. scopeImportedModules . at path))

checkFixityInfo ::
  forall r.
  (Members '[Error ScoperError, State Scope, State ScoperState, NameIdGen, InfoTableBuilder, Reader InfoTable] r) =>
  ParsedFixityInfo 'Parsed ->
  Sem r (ParsedFixityInfo 'Scoped)
checkFixityInfo ParsedFixityInfo {..} = do
  fields' <- mapM checkFixityFields _fixityFields
  return
    ParsedFixityInfo
      { _fixityFields = fields',
        ..
      }

checkFixityFields ::
  (Members '[Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, Reader InfoTable] r) =>
  ParsedFixityFields 'Parsed ->
  Sem r (ParsedFixityFields 'Scoped)
checkFixityFields ParsedFixityFields {..} = do
  same' <- mapM checkFixitySymbol _fixityFieldsPrecSame
  below' <- mapM (mapM checkFixitySymbol) _fixityFieldsPrecBelow
  above' <- mapM (mapM checkFixitySymbol) _fixityFieldsPrecAbove
  return
    ParsedFixityFields
      { _fixityFieldsPrecSame = same',
        _fixityFieldsPrecAbove = above',
        _fixityFieldsPrecBelow = below',
        _fixityFieldsAssoc,
        _fixityFieldsBraces
      }

getModuleId :: forall r. (Member (Reader PackageId) r) => TopModulePathKey -> Sem r ModuleId
getModuleId path = do
  pkgId <- ask
  return
    ModuleId
      { _moduleIdPath = path,
        _moduleIdPackageId = pkgId
      }

checkFixitySyntaxDef ::
  forall r.
  (Members '[Error ScoperError, Reader ScopeParameters, State Scope, State ScoperState, NameIdGen, InfoTableBuilder, Reader InfoTable, Reader PackageId] r) =>
  FixitySyntaxDef 'Parsed ->
  Sem r (FixitySyntaxDef 'Scoped)
checkFixitySyntaxDef def@FixitySyntaxDef {..} = topBindings $ do
  resolveFixitySyntaxDef def
  sym <- getReservedFixitySymbol _fixitySymbol
  doc <- mapM checkJudoc _fixityDoc
  info' <- checkFixityInfo _fixityInfo
  registerHighlightDoc (sym ^. S.nameId) doc
  return
    FixitySyntaxDef
      { _fixitySymbol = sym,
        _fixityDoc = doc,
        _fixityInfo = info',
        _fixityAssignKw,
        _fixitySyntaxKw,
        _fixityKw
      }

reserveFixitySyntaxDef ::
  forall r.
  (Members '[Reader BindingStrategy, Error ScoperError, Reader ScopeParameters, State Scope, State ScoperState, NameIdGen, InfoTableBuilder, Reader InfoTable] r) =>
  FixitySyntaxDef 'Parsed ->
  Sem r ()
reserveFixitySyntaxDef FixitySyntaxDef {..} =
  void (reserveSymbolOf SKNameFixity Nothing Nothing _fixitySymbol)

resolveFixitySyntaxDef ::
  forall r.
  (Members '[Error ScoperError, Reader ScopeParameters, State Scope, State ScoperState, NameIdGen, InfoTableBuilder, Reader InfoTable] r) =>
  FixitySyntaxDef 'Parsed ->
  Sem r ()
resolveFixitySyntaxDef fdef@FixitySyntaxDef {..} = topBindings $ do
  sym <- getReservedFixitySymbol _fixitySymbol
  let fi :: ParsedFixityInfo 'Parsed = _fixityInfo
  fi' :: ParsedFixityInfo 'Scoped <- checkFixityInfo _fixityInfo
  let same = fi' ^. fixityPrecSame
      below = fromMaybe [] (fi' ^. fixityPrecBelow)
      above = fromMaybe [] (fi' ^. fixityPrecAbove)
  fid <- maybe freshNameId getFixityId same
  below' <- mapM getFixityId below
  above' <- mapM getFixityId above
  forM_ above' (`registerPrecedence` fid)
  forM_ below' (registerPrecedence fid)
  samePrec <- maybe (return Nothing) (fmap Just . getPrec) same
  belowPrecs <- mapM getPrec below
  abovePrecs <- mapM getPrec above
  let belowPrec :: Integer
      belowPrec = fromIntegral $ maximum (minInt + 1 : abovePrecs)
      abovePrec :: Integer
      abovePrec = fromIntegral $ minimum (maxInt - 1 : belowPrecs)
  when (belowPrec + 1 >= abovePrec) $
    throw (ErrPrecedenceInconsistency (PrecedenceInconsistencyError fdef))
  when (isJust same && not (null below && null above)) $
    throw (ErrPrecedenceInconsistency (PrecedenceInconsistencyError fdef))
  -- we need Integer to avoid overflow when computing prec
  let prec = fromMaybe (fromInteger $ (abovePrec + belowPrec) `div` 2) samePrec
      fx =
        Fixity
          { _fixityId = Just fid,
            _fixityPrecedence = PrecNat prec,
            _fixityArity =
              case fi ^. fixityParsedArity . withLocParam of
                FI.Unary -> OpUnary AssocPostfix
                FI.Binary -> case fi ^. fixityAssoc of
                  Nothing -> OpBinary AssocNone
                  Just FI.AssocLeft -> OpBinary AssocLeft
                  Just FI.AssocRight -> OpBinary AssocRight
                  Just FI.AssocNone -> OpBinary AssocNone
                FI.None -> OpNone
          }
  registerFixityDef
    @$> FixityDef
      { _fixityDefSymbol = sym,
        _fixityDefFixity = fx,
        _fixityDefPrec = prec
      }
  return ()
  where
    getFixityDef :: (Members '[InfoTableBuilder, Reader InfoTable] r') => ScopedIden -> Sem r' FixityDef
    getFixityDef = lookupFixity . (^. scopedIdenFinal . S.nameId)

    getPrec :: (Members '[InfoTableBuilder, Reader InfoTable] r') => ScopedIden -> Sem r' Int
    getPrec = return . (^. fixityDefPrec) <=< getFixityDef

    getFixityId :: (Members '[InfoTableBuilder, Reader InfoTable] r') => ScopedIden -> Sem r' S.NameId
    getFixityId = return . fromJust . (^. fixityDefFixity . fixityId) <=< getFixityDef

checkOperatorSyntaxDef ::
  forall r.
  (Members '[Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, Reader InfoTable, Reader ScopeParameters, Reader PackageId, NameIdGen] r) =>
  OperatorSyntaxDef 'Parsed ->
  Sem r (OperatorSyntaxDef 'Scoped)
checkOperatorSyntaxDef OperatorSyntaxDef {..} = do
  fixityIden :: ScopedIden <- checkFixitySymbol _opFixity
  fxDef <- lookupFixity (fixityIden ^. scopedIdenFinal . S.nameId)
  let fx = fxDef ^. fixityDefFixity
  opIden :: ScopedIden <- checkScopedIden _opSymbol
  let uid = opIden ^. scopedIdenSrcName . S.nameId
  modify (set (scopeFixities . at uid) (Just fx))
  mdef <- mapM checkJudoc _opDoc
  return
    OperatorSyntaxDef
      { _opSymbol = opIden,
        _opDoc = mdef,
        _opFixity = fixityIden,
        _opSyntaxKw = _opSyntaxKw,
        _opKw = _opKw
      }

checkIteratorSyntaxDef ::
  forall r.
  (Members '[Reader ScopeParameters, Reader InfoTable, Reader PackageId, InfoTableBuilder, NameIdGen, Error ScoperError, State Scope, State ScoperState] r) =>
  IteratorSyntaxDef 'Parsed ->
  Sem r (IteratorSyntaxDef 'Scoped)
checkIteratorSyntaxDef s@IteratorSyntaxDef {..} = do
  checkAtLeastOneRange
  itername :: ScopedIden <- checkScopedIden _iterSymbol
  let uid = itername ^. scopedIdenSrcName . S.nameId
      iterNfo = fromParsedIteratorInfo _iterInfo
  modify (set (scopeIterators . at uid) (Just iterNfo))
  doc <- mapM checkJudoc _iterDoc
  return
    IteratorSyntaxDef
      { _iterSymbol = itername,
        _iterDoc = doc,
        _iterInfo = _iterInfo,
        _iterIteratorKw,
        _iterSyntaxKw
      }
  where
    checkAtLeastOneRange :: Sem r ()
    checkAtLeastOneRange = unless (maybe True (> 0) num) (throw (ErrInvalidRangeNumber (InvalidRangeNumber s)))
      where
        num :: Maybe Int
        num = s ^. iterInfo . to fromParsedIteratorInfo . iteratorInfoRangeNum

-- | Only used as syntactical convenience for registerX functions
(@$>) :: (Functor m) => (a -> m ()) -> a -> m a
(@$>) f a = f a $> a

checkDeriving ::
  ( Members
      '[ State Scope,
         Error ScoperError,
         State ScoperState,
         Reader ScopeParameters,
         Reader InfoTable,
         Reader PackageId,
         HighlightBuilder,
         InfoTableBuilder,
         NameIdGen,
         Reader BindingStrategy
       ]
      r
  ) =>
  Deriving 'Parsed ->
  Sem r (Deriving 'Scoped)
checkDeriving Deriving {..} = do
  let lhs@FunctionLhs {..} = _derivingFunLhs
  massert (isJust (_funLhsName ^? _FunctionDefName))
  let name = case _funLhsName of
        FunctionDefName n -> n
        FunctionDefNamePattern {} -> impossible
  typeSig' <- withLocalScope (checkTypeSig _funLhsTypeSig)
  name' <-
    if
        | P.isLhsFunctionRecursive lhs -> getReservedDefinitionSymbol name
        | otherwise -> reserveFunctionSymbol lhs
  let defname' =
        FunctionDefNameScoped
          { _functionDefNameScoped = name',
            _functionDefNamePattern = Nothing
          }
  let lhs' =
        FunctionLhs
          { _funLhsName = defname',
            _funLhsTypeSig = typeSig',
            ..
          }
  return
    Deriving
      { _derivingFunLhs = lhs',
        _derivingKw,
        _derivingPragmas
      }

checkTypeSig ::
  forall r.
  (Members '[HighlightBuilder, Reader ScopeParameters, Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, Reader InfoTable, NameIdGen, Reader PackageId, Reader BindingStrategy] r) =>
  TypeSig 'Parsed ->
  Sem r (TypeSig 'Scoped)
checkTypeSig TypeSig {..} = do
  a' <- mapM checkSigArg _typeSigArgs
  t' <- mapM checkParseExpressionAtoms _typeSigRetType
  return TypeSig {_typeSigArgs = a', _typeSigRetType = t', ..}
  where
    checkSigArg ::
      SigArg 'Parsed ->
      Sem r (SigArg 'Scoped)
    checkSigArg arg@SigArg {..} = do
      default' <- case _sigArgDefault of
        Nothing -> return Nothing
        Just ArgDefault {..} ->
          let err = throw (ErrWrongDefaultValue WrongDefaultValue {_wrongDefaultValue = arg})
           in case _sigArgImplicit of
                Explicit -> err
                ImplicitInstance -> err
                Implicit -> do
                  val' <- checkParseExpressionAtoms _argDefaultValue
                  return (Just ArgDefault {_argDefaultValue = val', ..})
      ty' <- mapM checkParseExpressionAtoms _sigArgType
      names' <- checkSigArgNames _sigArgNames
      return
        SigArg
          { _sigArgNames = names',
            _sigArgType = ty',
            _sigArgDefault = default',
            ..
          }

    checkSigArgNames :: SigArgNames 'Parsed -> Sem r (SigArgNames 'Scoped)
    checkSigArgNames = \case
      SigArgNamesInstance -> return SigArgNamesInstance
      SigArgNames ns -> fmap SigArgNames . forM ns $ \case
        ArgumentSymbol s -> ArgumentSymbol <$> bindVariableSymbol s
        ArgumentWildcard w -> return (ArgumentWildcard w)

checkFunctionDef ::
  forall r.
  (Members '[HighlightBuilder, Reader ScopeParameters, Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, Reader InfoTable, NameIdGen, Reader PackageId, Reader BindingStrategy] r) =>
  FunctionDef 'Parsed ->
  Sem r (FunctionDef 'Scoped)
checkFunctionDef fdef@FunctionDef {..} = do
  let FunctionLhs {..} = _functionDefLhs
  sigDoc' <- mapM checkJudoc _functionDefDoc
  (sig', sigBody') <- withLocalScope $ do
    a' <- checkTypeSig _funLhsTypeSig
    b' <- checkBody
    return (a', b')
  whenJust (functionSymbolPattern _funLhsName) reservePatternFunctionSymbols
  sigName' <- case _funLhsName of
    FunctionDefName name -> do
      name' <-
        if
            | P.isFunctionRecursive fdef -> getReservedDefinitionSymbol name
            | otherwise -> reserveFunctionSymbol (fdef ^. functionDefLhs)
      return
        FunctionDefNameScoped
          { _functionDefNameScoped = name',
            _functionDefNamePattern = Nothing
          }
    FunctionDefNamePattern p -> do
      name' <- freshSymbol KNameFunction KNameFunction False (WithLoc (getLoc p) "__pattern__")
      p' <- runReader PatternNamesKindFunctions (checkParsePatternAtom p)
      return
        FunctionDefNameScoped
          { _functionDefNameScoped = name',
            _functionDefNamePattern = Just p'
          }
  let lhs' =
        FunctionLhs
          { _funLhsName = sigName',
            _funLhsTypeSig = sig',
            ..
          }
      def =
        FunctionDef
          { _functionDefLhs = lhs',
            _functionDefDoc = sigDoc',
            _functionDefBody = sigBody',
            ..
          }
  registerNameSignature (sigName' ^. functionDefNameScoped . S.nameId) def
  registerFunctionDef @$> def
  where
    checkBody :: Sem r (FunctionDefBody 'Scoped)
    checkBody = case _functionDefBody of
      SigBodyExpression e -> SigBodyExpression <$> checkParseExpressionAtoms e
      SigBodyClauses cls -> SigBodyClauses <$> mapM checkClause cls

    checkClause :: FunctionClause 'Parsed -> Sem r (FunctionClause 'Scoped)
    checkClause FunctionClause {..} = do
      (patterns', body') <- withLocalScope $ do
        p <- mapM checkParsePatternAtom' _clausenPatterns
        b <- checkParseExpressionAtoms _clausenBody
        return (p, b)
      return
        FunctionClause
          { _clausenBody = body',
            _clausenPatterns = patterns',
            _clausenPipeKw,
            _clausenAssignKw
          }

checkInductiveParameters ::
  forall r.
  (Members '[HighlightBuilder, Reader ScopeParameters, Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, Reader InfoTable, NameIdGen, Reader PackageId] r) =>
  InductiveParameters 'Parsed ->
  Sem r (InductiveParameters 'Scoped)
checkInductiveParameters params = do
  _inductiveParametersRhs <- mapM checkRhs (params ^. inductiveParametersRhs)
  _inductiveParametersNames <- mapM bindVariableSymbol (params ^. inductiveParametersNames)
  return InductiveParameters {..}
  where
    checkRhs :: InductiveParametersRhs 'Parsed -> Sem r (InductiveParametersRhs 'Scoped)
    checkRhs rhs = do
      let _inductiveParametersColon = rhs ^. inductiveParametersColon
      _inductiveParametersType <- checkParseExpressionAtoms (rhs ^. inductiveParametersType)
      return InductiveParametersRhs {..}

checkInductiveDef ::
  forall r.
  ( Members
      '[ HighlightBuilder,
         Reader ScopeParameters,
         Error ScoperError,
         State Scope,
         State ScoperState,
         InfoTableBuilder,
         Reader InfoTable,
         NameIdGen,
         Reader PackageId,
         Reader BindingStrategy
       ]
      r,
    HasCallStack
  ) =>
  NonEmpty S.Symbol ->
  InductiveDef 'Parsed ->
  Sem r (InductiveDef 'Scoped)
checkInductiveDef constructorNames' InductiveDef {..} = do
  inductiveName' <- topBindings $ do
    i <- getReservedDefinitionSymbol _inductiveName
    return i
  (inductiveParameters', inductiveType', inductiveTypeApplied', inductiveDoc', inductiveConstructors') <- withLocalScope $ do
    inductiveParameters' <- mapM checkInductiveParameters _inductiveParameters
    inductiveType' <- mapM checkParseExpressionAtoms _inductiveType
    inductiveTypeApplied' <- checkParseExpressionAtoms _inductiveTypeApplied
    inductiveDoc' <- mapM checkJudoc _inductiveDoc
    inductiveConstructors' <-
      nonEmpty'
        <$> sequence
          [ checkConstructorDef inductiveName' cname cdef
            | (cname, cdef) <- zipExact (toList constructorNames') (toList _inductiveConstructors)
          ]
    return (inductiveParameters', inductiveType', inductiveTypeApplied', inductiveDoc', inductiveConstructors')
  let withModule' = case _inductiveWithModule of
        Just w -> Just (set (withModuleBody) [] w)
        Nothing -> Nothing
  let indDef =
        InductiveDef
          { _inductiveName = inductiveName',
            _inductiveDoc = inductiveDoc',
            _inductivePragmas = _inductivePragmas,
            _inductiveParameters = inductiveParameters',
            _inductiveType = inductiveType',
            _inductiveTypeApplied = inductiveTypeApplied',
            _inductiveConstructors = inductiveConstructors',
            _inductiveWithModule = withModule',
            _inductiveBuiltin,
            _inductivePositive,
            _inductiveTrait,
            _inductiveAssignKw,
            _inductiveKw
          }
  registerNameSignature (inductiveName' ^. S.nameId) indDef
  forM_ inductiveConstructors' $ \c -> do
    registerNameSignature (c ^. constructorName . S.nameId) (indDef, c)
  registerInductive @$> indDef
  where
    -- note that the constructor name is not bound here
    checkConstructorDef :: S.Symbol -> S.Symbol -> ConstructorDef 'Parsed -> Sem r (ConstructorDef 'Scoped)
    checkConstructorDef inductiveName' constructorName' ConstructorDef {..} = do
      doc' <- mapM checkJudoc _constructorDoc
      rhs' <- checkRhs _constructorRhs
      registerConstructor
        @$> ConstructorDef
          { _constructorName = constructorName',
            _constructorInductiveName = inductiveName',
            _constructorRhs = rhs',
            _constructorDoc = doc',
            _constructorPragmas = _constructorPragmas,
            _constructorPipe
          }
      where
        checkRhs :: ConstructorRhs 'Parsed -> Sem r (ConstructorRhs 'Scoped)
        checkRhs = \case
          ConstructorRhsGadt r -> ConstructorRhsGadt <$> checkGadt r
          ConstructorRhsRecord r -> ConstructorRhsRecord <$> checkRecord r
          ConstructorRhsAdt r -> ConstructorRhsAdt <$> checkAdt r

        checkRecord :: RhsRecord 'Parsed -> Sem r (RhsRecord 'Scoped)
        checkRecord RhsRecord {..} = do
          fields' <- checkRecordStatements _rhsRecordStatements
          let rhs' =
                RhsRecord
                  { _rhsRecordStatements = fields',
                    _rhsRecordDelim
                  }
          registerConstructorSignature (constructorName' ^. S.nameId) (mkRecordNameSignature rhs')
          return rhs'
          where
            checkRecordStatements :: [RecordStatement 'Parsed] -> Sem r [RecordStatement 'Scoped]
            checkRecordStatements ss = do
              -- The field names need to be only in scope for the syntax statements
              scopeSyntax <- withLocalScope $ do
                mapM_ reserveFieldName (ss ^.. each . _RecordStatementField)
                gets (^. scopeReserved)
              mapM (checkRecordStatement scopeSyntax) ss

            reserveFieldName :: RecordField 'Parsed -> Sem r ()
            reserveFieldName RecordField {..} = void (bindVariableSymbol _fieldName)

            checkRecordSyntaxDef :: RecordSyntaxDef 'Parsed -> Sem r (RecordSyntaxDef 'Scoped)
            checkRecordSyntaxDef = \case
              RecordSyntaxOperator d -> RecordSyntaxOperator <$> checkOperatorSyntaxDef d
              RecordSyntaxIterator d -> RecordSyntaxIterator <$> checkIteratorSyntaxDef d

            checkRecordStatement :: Reserved -> RecordStatement 'Parsed -> Sem r (RecordStatement 'Scoped)
            checkRecordStatement scopeSyntax = \case
              RecordStatementField d -> RecordStatementField <$> checkField d
              RecordStatementSyntax s -> RecordStatementSyntax <$> (withLocalScope (putReservedInScope scopeSyntax >> (checkRecordSyntaxDef s)))

            checkField :: RecordField 'Parsed -> Sem r (RecordField 'Scoped)
            checkField RecordField {..} = do
              doc' <- maybe (return Nothing) (checkJudoc >=> return . Just) _fieldDoc
              -- Since we don't allow dependent types in constructor types, each
              -- field is checked with a local scope
              withLocalScope $ do
                type' <- checkParseExpressionAtoms _fieldType
                typeSig' <- checkTypeSig _fieldTypeSig
                name' <- bindVariableSymbol _fieldName
                return
                  RecordField
                    { _fieldTypeSig = typeSig',
                      _fieldType = type',
                      _fieldName = name',
                      _fieldDoc = doc',
                      ..
                    }

        checkAdt :: RhsAdt 'Parsed -> Sem r (RhsAdt 'Scoped)
        checkAdt RhsAdt {..} = do
          args' <- mapM checkParseExpressionAtoms _rhsAdtArguments
          return
            RhsAdt
              { _rhsAdtArguments = args'
              }

        checkGadt :: RhsGadt 'Parsed -> Sem r (RhsGadt 'Scoped)
        checkGadt RhsGadt {..} = do
          constructorTypeSig' <- withLocalScope (checkTypeSig _rhsGadtTypeSig)
          return
            RhsGadt
              { _rhsGadtTypeSig = constructorTypeSig'
              }

checkProjectionDef ::
  forall r.
  (Members '[HighlightBuilder, Error ScoperError, InfoTableBuilder, Reader PackageId, Reader ScopeParameters, Reader InfoTable, Reader BindingStrategy, State Scope, State ScoperState, NameIdGen] r) =>
  ProjectionDef 'Parsed ->
  Sem r (ProjectionDef 'Scoped)
checkProjectionDef p = do
  _projectionConstructor <- getReservedDefinitionSymbol (p ^. projectionConstructor)
  _projectionField <- getReservedDefinitionSymbol (p ^. projectionField)
  _projectionType <- checkParseExpressionAtoms (p ^. projectionType)
  _projectionDoc <- maybe (return Nothing) (checkJudoc >=> return . Just) (p ^. projectionDoc)
  let p' =
        ProjectionDef
          { _projectionFieldIx = p ^. projectionFieldIx,
            _projectionFieldBuiltin = p ^. projectionFieldBuiltin,
            _projectionPragmas = p ^. projectionPragmas,
            _projectionKind = p ^. projectionKind,
            _projectionConstructor,
            _projectionField,
            _projectionType,
            _projectionDoc
          }
  registerProjectionSignature p'
  return p'

topBindings :: Sem (Reader BindingStrategy ': r) a -> Sem r a
topBindings = runReader BindingTop

localBindings :: Sem (Reader BindingStrategy ': r) a -> Sem r a
localBindings = runReader BindingLocal

checkTopModule ::
  forall r.
  (Members '[HighlightBuilder, Error ScoperError, Reader ScopeParameters, State ScoperState, Reader InfoTable, NameIdGen, Reader PackageId] r) =>
  Module 'Parsed 'ModuleTop ->
  Sem r (Module 'Scoped 'ModuleTop, ScopedModule, Scope)
checkTopModule m@Module {..} = checkedModule
  where
    freshTopModulePath ::
      forall s.
      (Members '[State ScoperState, NameIdGen, InfoTableBuilder, Reader InfoTable] s) =>
      Sem s S.TopModulePath
    freshTopModulePath = do
      _nameId <- freshNameId
      let _nameDefinedIn = topModulePathToAbsPath _modulePath
          _nameConcrete = _modulePath
          _nameDefined = getLoc (_modulePath ^. modulePathName)
          _nameKind = KNameTopModule
          _nameKindPretty = _nameKind
          _nameFixity :: Maybe Fixity
          _nameFixity = Nothing
          -- This visibility annotation is not relevant
          _nameVisibilityAnn = VisPublic
          _nameWhyInScope = S.BecauseDefined
          _nameVerbatim = N.topModulePathToDottedPath _modulePath
          _nameIterator :: Maybe IteratorInfo
          _nameIterator = Nothing
          _nameTop = True
          moduleName = S.Name' {..}
      registerName True moduleName
      return moduleName

    checkedModule :: Sem r (Module 'Scoped 'ModuleTop, ScopedModule, Scope)
    checkedModule = do
      ( tab :: InfoTable,
        ( sc :: Scope,
          ( e :: ExportInfo,
            body' :: [Statement 'Scoped],
            path' :: S.TopModulePath,
            doc' :: Maybe (Judoc 'Scoped)
            )
          )
        ) <-
        runInfoTableBuilder mempty $ do
          path' <- freshTopModulePath
          let iniScope :: Scope = emptyScopeTop (path' ^. S.nameId) (getTopModulePath m)
          runState iniScope $
            do
              withTopScope $ do
                body' <- topBindings (checkTopModuleBody _moduleBody)
                e <- genExportInfo
                doc' <- mapM checkJudoc _moduleDoc
                registerModuleDoc (path' ^. S.nameId) doc'
                return (e, body', path', doc')
      localModules <- getScopedLocalModules e
      _moduleId <- getModuleId (topModulePathKey (path' ^. S.nameConcrete))
      doctbl <- getDocTable _moduleId
      let md =
            Module
              { _modulePath = path',
                _moduleBody = body',
                _moduleDoc = doc',
                _modulePragmas = _modulePragmas,
                _moduleKw,
                _moduleOrigin,
                _moduleKwEnd,
                _moduleId,
                _moduleMarkdownInfo
              }
          smd =
            ScopedModule
              { _scopedModulePath = path',
                _scopedModuleName = S.topModulePathName path',
                _scopedModuleFilePath = P.getModuleFilePath m,
                _scopedModuleExportInfo = e,
                _scopedModuleLocalModules = localModules,
                _scopedModuleInfoTable = tab,
                _scopedModuleDocTable = doctbl
              }
      return (md, smd, sc)

withTopScope :: (Members '[State Scope] r) => Sem r a -> Sem r a
withTopScope ma = do
  before <- get @Scope
  let scope' = set scopeReserved emptyReserved before
  put scope'
  ma

withLocalScope :: (Members '[State Scope] r) => Sem r a -> Sem r a
withLocalScope ma = do
  before <- get @Scope
  let scope' = set scopeReserved emptyReserved before
  put scope'
  x <- ma
  put before
  return x

checkLocalModuleBody ::
  forall r.
  (Members '[HighlightBuilder, InfoTableBuilder, Reader InfoTable, Error ScoperError, State Scope, Reader ScopeParameters, State ScoperState, NameIdGen, Reader PackageId, Reader BindingStrategy] r) =>
  ReservedModule ->
  Sem r [Statement 'Scoped]
checkLocalModuleBody res = do
  let body = res ^. reservedModuleStatements
  checkReservedStatements body

checkTopModuleBody ::
  forall r.
  (Members '[HighlightBuilder, InfoTableBuilder, Reader InfoTable, Error ScoperError, State Scope, Reader ScopeParameters, State ScoperState, NameIdGen, Reader PackageId, Reader BindingStrategy] r) =>
  [Statement 'Parsed] ->
  Sem r [Statement 'Scoped]
checkTopModuleBody =
  reserveStatements >=> checkReservedStatements

reserveStatements ::
  forall r.
  ( Members
      '[ HighlightBuilder,
         Error ScoperError,
         Reader ScopeParameters,
         State Scope,
         State ScoperState,
         InfoTableBuilder,
         Reader InfoTable,
         NameIdGen,
         Reader PackageId
       ]
      r
  ) =>
  [Statement 'Parsed] ->
  Sem r [Statement 'Parsed]
reserveStatements = topBindings . mapM reserveDefinition
  where
    reserveDefinition ::
      ( Members
          '[ Error ScoperError,
             State Scope,
             State ScoperState,
             Reader PackageId,
             Reader ScopeParameters,
             InfoTableBuilder,
             NameIdGen,
             HighlightBuilder,
             Reader BindingStrategy,
             Reader InfoTable
           ]
          r'
      ) =>
      Statement 'Parsed ->
      Sem r' (Statement 'Parsed)
    reserveDefinition def = case def of
      StatementSyntax s -> reserveSyntaxDef s $> def
      StatementFunctionDef d -> reserveFunctionLikeSymbol d $> def
      StatementDeriving d -> reserveDerivingSymbol d $> def
      StatementAxiom d -> void (reserveAxiomSymbol d) $> def
      StatementModule d -> void (reserveLocalModule d) $> def
      StatementProjectionDef d -> void (reserveProjectionSymbol d) $> def
      StatementImport i -> reserveImport i $> def
      StatementOpenModule {} -> return def
      StatementReservedInductive {} -> impossible
      StatementInductive d -> StatementReservedInductive <$> reserveInductive d

checkReservedStatements ::
  forall r.
  (Members '[HighlightBuilder, Error ScoperError, Reader ScopeParameters, State Scope, State ScoperState, InfoTableBuilder, Reader InfoTable, NameIdGen, Reader PackageId] r) =>
  [Statement 'Parsed] ->
  Sem r [Statement 'Scoped]
checkReservedStatements = topBindings . concatMapM (fmap toList . scopeDefinition)
  where
    scopeDefinition ::
      ( Members
          '[ Error ScoperError,
             State Scope,
             State ScoperState,
             Reader PackageId,
             Reader ScopeParameters,
             InfoTableBuilder,
             NameIdGen,
             Reader BindingStrategy,
             HighlightBuilder,
             Reader InfoTable
           ]
          r'
      ) =>
      Statement 'Parsed ->
      Sem r' (NonEmpty (Statement 'Scoped))
    scopeDefinition = \case
      StatementSyntax s -> pure . StatementSyntax <$> checkSyntaxDef s
      StatementFunctionDef d -> pure . StatementFunctionDef <$> checkFunctionDef d
      StatementDeriving d -> pure . StatementDeriving <$> checkDeriving d
      StatementAxiom d -> pure . StatementAxiom <$> checkAxiomDef d
      StatementModule d -> pure . StatementModule <$> checkLocalModule d
      StatementImport i -> pure . StatementImport <$> checkImport i
      StatementOpenModule i -> pure . StatementOpenModule <$> checkOpenModule i
      StatementInductive {} -> impossible
      StatementReservedInductive d -> checkReservedInductive d
      StatementProjectionDef d -> pure . StatementProjectionDef <$> checkProjectionDef d

checkReservedInductive ::
  ( HasCallStack,
    Members
      '[ Error ScoperError,
         State Scope,
         State ScoperState,
         Reader PackageId,
         Reader ScopeParameters,
         InfoTableBuilder,
         NameIdGen,
         Reader BindingStrategy,
         HighlightBuilder,
         Reader InfoTable
       ]
      r
  ) =>
  ReservedInductiveDef ->
  Sem r (NonEmpty (Statement 'Scoped))
checkReservedInductive r = do
  tyDef0 <- checkInductiveDef (r ^. reservedInductiveConstructors) (r ^. reservedInductiveDef)
  modDef <- checkLocalModule (r ^. reservedInductiveDefModule)
  let withDefs :: [Statement 'Scoped] =
        getDefs (modDef ^. moduleBody)
      tyDef = set (inductiveWithModule . _Just . withModuleBody) withDefs tyDef0
  return (StatementInductive tyDef :| [StatementModule modDef])
  where
    getDefs :: [Statement 'Scoped] -> [Statement 'Scoped]
    getDefs = dropWhile (has _StatementProjectionDef)

defineInductiveModule :: forall r. (Members '[Reader PackageId] r) => InductiveDef 'Parsed -> Sem r (Module 'Parsed 'ModuleLocal)
defineInductiveModule i =
  runReader (getLoc (i ^. inductiveName)) genModule
  where
    genModule :: forall s'. (Members '[Reader Interval, Reader PackageId] s') => Sem s' (Module 'Parsed 'ModuleLocal)
    genModule = do
      _moduleKw <- G.kw G.kwModule
      _moduleKwEnd <- G.kw G.kwEnd
      let _modulePath = i ^. inductiveName
          _moduleId = ()
      _moduleBody <- genBody
      return
        Module
          { _moduleDoc = Nothing,
            _modulePragmas = Nothing,
            _moduleOrigin = LocalModuleType,
            _moduleMarkdownInfo = Nothing,
            ..
          }
      where
        genBody :: Sem s' [Statement 'Parsed]
        genBody = do
          return (projections ++ i ^.. inductiveWithModule . _Just . withModuleBody . each)
          where
            constructorAndFields :: Maybe (Symbol, [RecordStatement 'Parsed])
            constructorAndFields = case i ^. inductiveConstructors of
              c :| []
                | ConstructorRhsRecord r <- c ^. constructorRhs -> Just (c ^. constructorName, r ^. rhsRecordStatements)
              _ -> Nothing

            projections :: [Statement 'Parsed]
            projections = case constructorAndFields of
              Nothing -> []
              Just (constr, fields) -> run . evalState 0 $ mapM goRecordStatement fields
                where
                  goRecordStatement :: RecordStatement 'Parsed -> Sem '[State Int] (Statement 'Parsed)
                  goRecordStatement = \case
                    RecordStatementSyntax f -> StatementSyntax <$> goSyntax f
                    RecordStatementField f -> goField f
                    where
                      goSyntax :: RecordSyntaxDef 'Parsed -> Sem s (SyntaxDef 'Parsed)
                      goSyntax = \case
                        RecordSyntaxOperator d -> return (SyntaxOperator d)
                        RecordSyntaxIterator d -> return (SyntaxIterator d)

                      goField :: RecordField 'Parsed -> Sem '[State Int] (Statement 'Parsed)
                      goField f = do
                        idx <- popFieldIx
                        let s = mkProjection (Indexed idx f)
                        return (StatementProjectionDef s)
                        where
                          popFieldIx :: Sem '[State Int] Int
                          popFieldIx = get <* modify' @Int succ

                  mkProjection ::
                    Indexed (RecordField 'Parsed) ->
                    ProjectionDef 'Parsed
                  mkProjection (Indexed idx field) =
                    ProjectionDef
                      { _projectionConstructor = constr,
                        _projectionField = field ^. fieldName,
                        _projectionType = G.mkProjectionType i (G.mkTypeSigType' (G.mkWildcardParsed (getLoc (i ^. inductiveName))) (field ^. fieldTypeSig)),
                        _projectionFieldIx = idx,
                        _projectionKind = kind,
                        _projectionFieldBuiltin = field ^. fieldBuiltin,
                        _projectionDoc = field ^. fieldDoc,
                        _projectionPragmas = combinePragmas (i ^. inductivePragmas) (field ^. fieldPragmas)
                      }
                    where
                      kind :: ProjectionKind
                      kind = case field ^. fieldIsImplicit of
                        ExplicitField -> ProjectionExplicit
                        ImplicitInstanceField -> ProjectionCoercion

                      combinePragmas :: Maybe ParsedPragmas -> Maybe ParsedPragmas -> Maybe ParsedPragmas
                      combinePragmas p1 p2 = case (p1, p2) of
                        (Nothing, Nothing) -> Nothing
                        (Just p, Nothing) -> Just p
                        (Nothing, Just p) -> Just p
                        (Just p1', Just p2') ->
                          Just
                            ( over
                                (withLocParam . withSourceValue . pragmasIsabelleIgnore)
                                (\i2 -> i2 <|> (p1' ^. withLocParam . withSourceValue . pragmasIsabelleIgnore))
                                p2'
                            )

-- | Returns the module generated for the inductive definition
reserveInductive ::
  forall r.
  ( Members
      '[ Reader PackageId,
         Error ScoperError,
         Reader BindingStrategy,
         Reader InfoTable,
         State Scope,
         NameIdGen,
         State ScoperState,
         Reader ScopeParameters,
         HighlightBuilder,
         InfoTableBuilder
       ]
      r
  ) =>
  InductiveDef 'Parsed ->
  Sem r ReservedInductiveDef
reserveInductive d = do
  i <- reserveInductiveSymbol d
  let builtinConstrs :: NonEmpty (Maybe BuiltinConstructor)
      builtinConstrs =
        NonEmpty.prependList
          (d ^.. inductiveBuiltin . _Just . withLocParam . to builtinConstructors . each . to Just)
          (NonEmpty.repeat Nothing)
  let regConstrs :: Sem r (NonEmpty S.Symbol) = do
        constrs <- mapM (uncurry reserveConstructor) (mzip builtinConstrs (d ^. inductiveConstructors))
        ignoreFail (registerRecordType (head constrs) i)
        return constrs
  m <- defineInductiveModule d
  constrs <- reserveTypeLocalModule regConstrs m
  let r =
        ReservedInductiveDef
          { _reservedInductiveDef = d,
            _reservedInductiveConstructors = constrs,
            _reservedInductiveDefModule = m
          }
  return r
  where
    reserveConstructor :: Maybe BuiltinConstructor -> ConstructorDef 'Parsed -> Sem r S.Symbol
    reserveConstructor b c = do
      c' <- reserveConstructorSymbol d c b
      let storeSig :: RecordNameSignature 'Parsed -> Sem r ()
          storeSig sig = modify' (set (scoperConstructorFields . at (c' ^. S.nameId)) (Just sig))
          mrecord :: Maybe (RhsRecord 'Parsed) = c ^? constructorRhs . _ConstructorRhsRecord
      whenJust mrecord $ \r -> do
        let sig = mkRecordNameSignature r
        storeSig sig
        registerParsedConstructorSig (c' ^. S.nameId) sig
      return c'

    registerRecordType :: S.Symbol -> S.Symbol -> Sem (Fail ': r) ()
    registerRecordType mconstr ind =
      case d ^. inductiveConstructors of
        mkRec :| cs
          | notNull cs -> fail
          | otherwise -> do
              fs <-
                failMaybe $
                  mkRec
                    ^? ( constructorRhs
                           . _ConstructorRhsRecord
                           . to mkRecordNameSignature
                       )
              let info =
                    RecordInfo
                      { _recordInfoSignature = fs,
                        _recordInfoConstructor = mconstr
                      }
              modify' (set (scoperRecordFields . at (ind ^. S.nameId)) (Just info))
              registerRecordInfo (ind ^. S.nameId) info

mkLetSections :: [LetStatement 'Parsed] -> [Statement 'Parsed]
mkLetSections = map toTopStatement
  where
    toTopStatement :: LetStatement 'Parsed -> Statement 'Parsed
    toTopStatement = \case
      LetFunctionDef f -> StatementFunctionDef f
      LetAliasDef f -> StatementSyntax (SyntaxAlias f)
      LetOpen o -> StatementOpenModule o

reserveLocalModuleSymbol ::
  (Members '[Error ScoperError, State Scope, Reader ScopeParameters, State ScoperState, InfoTableBuilder, Reader InfoTable, NameIdGen, Reader PackageId, Reader BindingStrategy] r) =>
  Symbol ->
  Sem r S.Symbol
reserveLocalModuleSymbol =
  reserveSymbolOf SKNameLocalModule Nothing Nothing

reserveLocalModule ::
  forall r.
  ( Members
      '[ HighlightBuilder,
         Error ScoperError,
         State Scope,
         Reader ScopeParameters,
         State ScoperState,
         InfoTableBuilder,
         Reader InfoTable,
         NameIdGen,
         Reader BindingStrategy,
         Reader PackageId
       ]
      r
  ) =>
  Module 'Parsed 'ModuleLocal ->
  Sem r ()
reserveLocalModule = reserveTypeLocalModule (return ())

reserveTypeLocalModule ::
  forall r constrs.
  ( Members
      '[ HighlightBuilder,
         Error ScoperError,
         State Scope,
         Reader ScopeParameters,
         State ScoperState,
         InfoTableBuilder,
         Reader InfoTable,
         NameIdGen,
         Reader BindingStrategy,
         Reader PackageId
       ]
      r
  ) =>
  -- | An action that reserves the constructors at the beginning. Only used for
  -- type modules
  (Sem r constrs) ->
  Module 'Parsed 'ModuleLocal ->
  Sem r constrs
reserveTypeLocalModule mreserveConstructors Module {..} = do
  _modulePath' :: S.Symbol <- reserveLocalModuleSymbol _modulePath
  (cs :: constrs, resModule :: ReservedModule, minfo :: ModuleExportInfo) <- withLocalScope $ do
    inheritScope _modulePath
    cs <- mreserveConstructors
    b <- reserveStatements _moduleBody
    export <- genExportInfo
    reserved <- gets (^. scopeReserved)
    let mname = S.unqualifiedSymbol _modulePath'
        resMod =
          ReservedModule
            { _reservedModuleName = mname,
              _reservedModuleReserved = reserved,
              _reservedModuleStatements = b
            }
        nfo =
          ModuleExportInfo
            { _moduleExportInfoModuleName = mname,
              _moduleExportInfo = export
            }
    return (cs, resMod, nfo)
  modify (set (scoperReservedModules . at (_modulePath' ^. S.nameId)) (Just resModule))
  modify (set (scoperExportInfo . at (_modulePath' ^. S.nameId)) (Just minfo))
  registerName True _modulePath'
  return cs

inheritScope :: (Members '[State Scope] r') => Symbol -> Sem r' ()
inheritScope _modulePath = do
  absPath <- (`appendModulePath` _modulePath) <$> gets (^. scopePath)
  modify (set scopePath absPath)
  modify (over scopeSymbols (fmap inheritSymbol))
  modify (over scopeModuleSymbols (fmap inheritSymbol))
  modify (over scopeFixitySymbols (fmap inheritSymbol))
  where
    inheritSymbol :: forall ns. (SingI ns) => SymbolInfo ns -> SymbolInfo ns
    inheritSymbol (SymbolInfo s) = SymbolInfo (inheritEntry <$> s)
      where
        inheritEntry :: NameSpaceEntryType ns -> NameSpaceEntryType ns
        inheritEntry =
          over (nsEntry . S.nameWhyInScope) S.BecauseInherited
            . set (nsEntry . S.nameVisibilityAnn) VisPrivate

checkLocalModule ::
  forall r.
  ( HasCallStack,
    Members
      '[ HighlightBuilder,
         Error ScoperError,
         State Scope,
         Reader ScopeParameters,
         State ScoperState,
         InfoTableBuilder,
         Reader InfoTable,
         NameIdGen,
         Reader BindingStrategy,
         Reader PackageId
       ]
      r
  ) =>
  Module 'Parsed 'ModuleLocal ->
  Sem r (Module 'Scoped 'ModuleLocal)
checkLocalModule md@Module {..} = do
  tab1 <- ask @InfoTable
  tab2 <- getBuilderInfoTable
  _modulePath' :: S.Symbol <- getReservedLocalModuleSymbol _modulePath
  let modEntry = ModuleSymbolEntry (S.unqualifiedSymbol _modulePath')
      mid = _modulePath' ^. S.nameId
  reservedModule <- getReservedLocalModule modEntry
  let reserved = reservedModule ^. reservedModuleReserved
  (tab, (exportInfo, moduleBody', moduleDoc')) <-
    withLocalScope
      . runReader (tab1 <> tab2)
      . runInfoTableBuilder mempty
      $ do
        inheritScope _modulePath
        modify (set scopeReserved reserved)
        putReservedInScope reserved
        modify (set scopeModuleId mid)
        b <- checkLocalModuleBody reservedModule
        doc' <- mapM checkJudoc _moduleDoc
        e <- genExportInfo
        return (e, b, doc')
  localModules <- getScopedLocalModules exportInfo
  modify (set (scoperExportInfo . at mid . _Just . moduleExportInfo) exportInfo)
  let moduleName = S.unqualifiedSymbol _modulePath'
      m =
        Module
          { _modulePath = _modulePath',
            _moduleBody = moduleBody',
            _moduleDoc = moduleDoc',
            _modulePragmas = _modulePragmas,
            _moduleMarkdownInfo = Nothing,
            _moduleId = (),
            _moduleKw,
            _moduleOrigin,
            _moduleKwEnd
          }
      smod =
        ScopedModule
          { _scopedModulePath = set nameConcrete (moduleNameToTopModulePath (NameUnqualified _modulePath)) moduleName,
            _scopedModuleName = moduleName,
            _scopedModuleDocTable = mempty,
            _scopedModuleFilePath = P.getModuleFilePath md,
            _scopedModuleExportInfo = exportInfo,
            _scopedModuleLocalModules = localModules,
            _scopedModuleInfoTable = tab
          }
  modify (over scoperModules (HashMap.insert mid smod))
  registerLocalModule smod
  registerName True _modulePath'
  return m

putReservedInScope ::
  ( Members
      '[ Reader BindingStrategy,
         State Scope
       ]
      r'
  ) =>
  Reserved ->
  Sem r' ()
putReservedInScope reserved = forEachNameSpace $ \ns ->
  forM_ (HashMap.toList (reserved ^. reservedNameSpace ns)) $ \(s, s') -> do
    let kind = getNameKind s'
    addToScope ns kind s s'

symbolInfoSingle :: (SingI ns) => NameSpaceEntryType ns -> SymbolInfo ns
symbolInfoSingle p = SymbolInfo $ HashMap.singleton (p ^. nsEntry . S.nameDefinedIn) p

getModule ::
  (Members '[State ScoperState] r) =>
  ModuleSymbolEntry ->
  Name ->
  Sem r ModuleExportInfo
getModule e n =
  set (moduleExportInfoModuleName . S.nameConcrete) n
    <$> getModuleExportInfo e

lookupModuleSymbol ::
  (Members '[Error ScoperError, State Scope, State ScoperState] r) =>
  Name ->
  Sem r ModuleExportInfo
lookupModuleSymbol n = do
  es <- snd3 <$> lookupQualifiedSymbol (path, sym)
  case nonEmpty (resolveShadowing (toList es)) of
    Nothing -> notInScope
    Just (x :| []) -> getModule x n
    Just more -> throw (ErrAmbiguousModuleSym (AmbiguousModuleSym n more))
  where
    notInScope = throw (ErrModuleNotInScope (ModuleNotInScope n))
    (path, sym) = case n of
      NameUnqualified s -> ([], s)
      NameQualified (QualifiedName (SymbolPath p) s) -> (toList p, s)

checkOpenModuleShort ::
  forall r.
  (Members '[Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, Reader InfoTable, NameIdGen] r) =>
  ModuleExportInfo ->
  OpenModule 'Parsed 'OpenShort ->
  Sem r (OpenModule 'Scoped 'OpenShort)
checkOpenModuleShort = checkOpenModuleHelper

checkOpenModule ::
  forall r.
  (Members '[Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, Reader InfoTable, NameIdGen] r) =>
  OpenModule 'Parsed 'OpenFull ->
  Sem r (OpenModule 'Scoped 'OpenFull)
checkOpenModule open@OpenModule {..} = do
  m <- lookupModuleSymbol _openModuleName
  checkOpenModuleHelper m open

checkUsingHiding ::
  forall r.
  (Members '[Error ScoperError, InfoTableBuilder] r) =>
  S.Name ->
  ExportInfo ->
  UsingHiding 'Parsed ->
  Sem r (UsingHiding 'Scoped)
checkUsingHiding moduleName exportInfo = \case
  Hiding h -> Hiding <$> checkHidingList h
  Using uh -> Using <$> checkUsingList uh
  where
    scopeSymbol :: forall (ns :: NameSpace). (SingI ns) => Sing ns -> Symbol -> Sem r S.Symbol
    scopeSymbol _ s = do
      let mentry :: Maybe (NameSpaceEntryType ns)
          mentry = exportInfo ^. exportNameSpace . at s
          err =
            throw
              . ErrModuleDoesNotExportSymbol
              $ ModuleDoesNotExportSymbol
                { _moduleDoesNotExportSymbol = s,
                  _moduleDoesNotExportModule = moduleName
                }
      entry <- maybe err return mentry
      let scopedSym = entryToSymbol entry s
      registerName False scopedSym
      return scopedSym

    checkHidingList :: HidingList 'Parsed -> Sem r (HidingList 'Scoped)
    checkHidingList l = do
      items' <- mapM checkHidingItem (l ^. hidingList)
      return
        HidingList
          { _hidingKw = l ^. hidingKw,
            _hidingBraces = l ^. hidingBraces,
            _hidingList = items'
          }

    checkUsingList :: UsingList 'Parsed -> Sem r (UsingList 'Scoped)
    checkUsingList l = do
      items' <- mapM checkUsingItem (l ^. usingList)
      return
        UsingList
          { _usingKw = l ^. usingKw,
            _usingBraces = l ^. usingBraces,
            _usingList = items'
          }

    checkHidingItem :: HidingItem 'Parsed -> Sem r (HidingItem 'Scoped)
    checkHidingItem h = do
      let s = h ^. hidingSymbol
      scopedSym <-
        if
            | isJust (h ^. hidingModuleKw) -> scopeSymbol SNameSpaceModules s
            | otherwise -> scopeSymbol SNameSpaceSymbols s
      return
        HidingItem
          { _hidingSymbol = scopedSym,
            _hidingModuleKw = h ^. hidingModuleKw
          }

    checkUsingItem :: UsingItem 'Parsed -> Sem r (UsingItem 'Scoped)
    checkUsingItem i = do
      let s = i ^. usingSymbol
      scopedSym <-
        if
            | isJust (i ^. usingModuleKw) -> scopeSymbol SNameSpaceModules s
            | otherwise -> scopeSymbol SNameSpaceSymbols s
      let scopedAs = do
            c <- i ^. usingAs
            return (set S.nameConcrete c scopedSym)
      mapM_ (registerName False) scopedAs
      return
        UsingItem
          { _usingSymbol = scopedSym,
            _usingAs = scopedAs,
            _usingAsKw = i ^. usingAsKw,
            _usingModuleKw = i ^. usingModuleKw
          }

checkOpenModuleHelper ::
  forall r short.
  (Members '[Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, Reader InfoTable, NameIdGen] r, SingI short) =>
  ModuleExportInfo ->
  OpenModule 'Parsed short ->
  Sem r (OpenModule 'Scoped short)
checkOpenModuleHelper modInfo OpenModule {..} = do
  let exportInfo = modInfo ^. moduleExportInfo
      modName = modInfo ^. moduleExportInfoModuleName
  registerName False modName
  usingHiding' <- mapM (checkUsingHiding modName exportInfo) _openModuleUsingHiding
  mergeScope (filterExportInfo _openModulePublic usingHiding' exportInfo)
  let openName :: OpenModuleNameType 'Scoped short = case sing :: SIsOpenShort short of
        SOpenFull -> modName
        SOpenShort -> ()
  return
    OpenModule
      { _openModuleName = openName,
        _openModuleUsingHiding = usingHiding',
        _openModulePublic,
        ..
      }
  where
    mergeScope :: ExportInfo -> Sem r ()
    mergeScope ei = do
      mapM_ mergeSymbol (HashMap.toList (ei ^. exportSymbols))
      mapM_ mergeSymbol (HashMap.toList (ei ^. exportModuleSymbols))
      mapM_ mergeSymbol (HashMap.toList (ei ^. exportFixitySymbols))
      where
        mergeSymbol :: forall ns. (SingI ns) => (Symbol, NameSpaceEntryType ns) -> Sem r ()
        mergeSymbol (s, entry) =
          modify
            (over scopeNameSpaceI (HashMap.insertWith (<>) s (symbolInfoSingle entry)))

filterExportInfo :: PublicAnn -> Maybe (UsingHiding 'Scoped) -> ExportInfo -> ExportInfo
filterExportInfo pub openModif = alterEntries . filterScope
  where
    alterEntries :: ExportInfo -> ExportInfo
    alterEntries nfo =
      ExportInfo
        { _exportSymbols = alterEntry <$> nfo ^. exportSymbols,
          _exportModuleSymbols = alterEntry <$> nfo ^. exportModuleSymbols,
          _exportFixitySymbols = alterEntry <$> nfo ^. exportFixitySymbols
        }

    alterEntry :: (SingI ns) => NameSpaceEntryType ns -> NameSpaceEntryType ns
    alterEntry =
      over
        nsEntry
        ( set S.nameWhyInScope S.BecauseImportedOpened
            . set S.nameVisibilityAnn (publicAnnToVis pub)
        )

    publicAnnToVis :: PublicAnn -> VisibilityAnn
    publicAnnToVis = \case
      Public {} -> VisPublic
      NoPublic -> VisPrivate

    filterScope :: ExportInfo -> ExportInfo
    filterScope = case openModif of
      Just (Using l) ->
        over exportSymbols (HashMap.fromList . mapMaybe inUsing . HashMap.toList)
          . over exportModuleSymbols (HashMap.fromList . mapMaybe inUsing . HashMap.toList)
          . over exportFixitySymbols (HashMap.fromList . mapMaybe inUsing . HashMap.toList)
        where
          inUsing ::
            forall (ns :: NameSpace).
            (SingI ns) =>
            (Symbol, NameSpaceEntryType ns) ->
            Maybe (Symbol, NameSpaceEntryType ns)
          inUsing (sym, e) = do
            mayAs' <- u ^. at (e ^. nsEntry . S.nameId)
            return (fromMaybe sym mayAs', e)
          u :: HashMap NameId (Maybe Symbol)
          u =
            hashMap
              [ (i ^. usingSymbol . S.nameId, i ^? usingAs . _Just . S.nameConcrete)
                | i <- toList (l ^. usingList)
              ]
      Just (Hiding l) ->
        over exportSymbols (HashMap.filter (not . inHiding))
          . over exportModuleSymbols (HashMap.filter (not . inHiding))
          . over exportFixitySymbols (HashMap.filter (not . inHiding))
        where
          inHiding :: forall ns. (SingI ns) => NameSpaceEntryType ns -> Bool
          inHiding e = HashSet.member (e ^. nsEntry . S.nameId) u
          u :: HashSet NameId
          u = HashSet.fromList (map (^. hidingSymbol . S.nameId) (toList (l ^. hidingList)))
      Nothing -> id

checkAxiomDef ::
  (Members '[HighlightBuilder, Reader ScopeParameters, InfoTableBuilder, Reader InfoTable, Error ScoperError, State Scope, State ScoperState, NameIdGen, Reader BindingStrategy, Reader PackageId] r) =>
  AxiomDef 'Parsed ->
  Sem r (AxiomDef 'Scoped)
checkAxiomDef AxiomDef {..} = do
  axiomName' <- getReservedDefinitionSymbol _axiomName
  axiomDoc' <- withLocalScope (mapM checkJudoc _axiomDoc)
  axiomSig' <- withLocalScope (checkTypeSig _axiomTypeSig)
  let a =
        AxiomDef
          { _axiomName = axiomName',
            _axiomTypeSig = axiomSig',
            _axiomDoc = axiomDoc',
            ..
          }
  registerNameSignature (a ^. axiomName . S.nameId) a
  registerAxiom @$> a

entryToSymbol :: forall (ns :: NameSpace). (SingI ns) => NameSpaceEntryType ns -> Symbol -> S.Symbol
entryToSymbol sentry csym = set S.nameConcrete csym (sentry ^. nsEntry)

checkFunction ::
  forall r.
  (Members '[HighlightBuilder, Reader ScopeParameters, Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, Reader InfoTable, NameIdGen, Reader PackageId] r) =>
  Function 'Parsed ->
  Sem r (Function 'Scoped)
checkFunction f = do
  _paramType <- checkParseExpressionAtoms (f ^. funParameters . paramType)
  withLocalScope $ do
    _paramNames <- forM (f ^. funParameters . paramNames) $ \case
      FunctionParameterWildcard w -> return (FunctionParameterWildcard w)
      FunctionParameterName p -> FunctionParameterName <$> bindVariableSymbol p
    _funReturn <- checkParseExpressionAtoms (f ^. funReturn)
    let _paramImplicit = f ^. funParameters . paramImplicit
        _paramColon = f ^. funParameters . paramColon
        _paramDelims = f ^. funParameters . paramDelims
        _funParameters = FunctionParameters {..}
        _funKw = f ^. funKw
    return Function {..}

-- | for now functions defined in let clauses cannot be infix operators
checkLetStatements ::
  (Members '[HighlightBuilder, Reader ScopeParameters, Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, Reader InfoTable, NameIdGen, Reader PackageId] r) =>
  NonEmpty (LetStatement 'Parsed) ->
  Sem r (NonEmpty (LetStatement 'Scoped))
checkLetStatements =
  fmap fromSections
    . (reserveStatements >=> checkReservedStatements)
    . mkLetSections
    . toList
  where
    fromSections :: [Statement s] -> NonEmpty (LetStatement s)
    fromSections l = fromDef <$> nonEmpty' l
      where
        fromSyn :: SyntaxDef s -> LetStatement s
        fromSyn = \case
          SyntaxAlias a -> LetAliasDef a
          SyntaxFixity {} -> impossible
          SyntaxOperator {} -> impossible
          SyntaxIterator {} -> impossible

        fromDef :: Statement s -> LetStatement s
        fromDef = \case
          StatementFunctionDef d -> LetFunctionDef d
          StatementSyntax syn -> fromSyn syn
          StatementOpenModule o -> LetOpen o
          StatementInductive {} -> impossible
          StatementReservedInductive {} -> impossible
          StatementModule {} -> impossible
          StatementDeriving {} -> impossible
          StatementProjectionDef {} -> impossible
          StatementAxiom {} -> impossible
          StatementImport {} -> impossible

checkRecordPattern ::
  forall r.
  (Members '[Reader PatternNamesKind, Error ScoperError, State Scope, State ScoperState, Reader BindingStrategy, InfoTableBuilder, Reader InfoTable, NameIdGen] r) =>
  RecordPattern 'Parsed ->
  Sem r (RecordPattern 'Scoped)
checkRecordPattern r = do
  c' <- getNameOfKind KNameConstructor (r ^. recordPatternConstructor)
  fields <- fromMaybeM (return (RecordNameSignature mempty)) (gets (^. scoperConstructorFields . at (c' ^. scopedIdenFinal . S.nameId)))
  l' <-
    if
        | null (r ^. recordPatternItems) -> return []
        | otherwise -> do
            when (null (fields ^. recordNames)) (throw (noFields c'))
            runReader fields (mapM checkItem (r ^. recordPatternItems))
  return
    RecordPattern
      { _recordPatternConstructor = c',
        _recordPatternItems = l'
      }
  where
    noFields :: ScopedIden -> ScoperError
    noFields = ErrConstructorNotARecord . ConstructorNotARecord

    checkItem ::
      forall r'.
      (Members '[Reader (RecordNameSignature 'Parsed), Reader PatternNamesKind, Error ScoperError, State Scope, State ScoperState, Reader BindingStrategy, InfoTableBuilder, Reader InfoTable, NameIdGen] r') =>
      RecordPatternItem 'Parsed ->
      Sem r' (RecordPatternItem 'Scoped)
    checkItem = \case
      RecordPatternItemAssign a -> RecordPatternItemAssign <$> checkAssign a
      RecordPatternItemFieldPun a -> RecordPatternItemFieldPun <$> checkPun a
      where
        checkAssign :: RecordPatternAssign 'Parsed -> Sem r' (RecordPatternAssign 'Scoped)
        checkAssign RecordPatternAssign {..} = do
          idx' <- findRecordFieldIdx _recordPatternAssignField
          pat' <- checkParsePatternAtoms _recordPatternAssignPattern
          return
            RecordPatternAssign
              { _recordPatternAssignFieldIx = idx',
                _recordPatternAssignPattern = pat',
                ..
              }

        checkPun :: PatternFieldPun 'Parsed -> Sem r' (PatternFieldPun 'Scoped)
        checkPun f = do
          idx' <- findRecordFieldIdx (f ^. fieldPunField)
          pk <- ask
          f' <- case pk of
            PatternNamesKindVariables ->
              bindVariableSymbol (f ^. fieldPunField)
            PatternNamesKindFunctions -> do
              getReservedDefinitionSymbol (f ^. fieldPunField)
          return
            PatternFieldPun
              { _fieldPunIx = idx',
                _fieldPunField = f'
              }

findRecordFieldIdx ::
  forall r.
  (Members '[Reader (RecordNameSignature 'Parsed), Error ScoperError] r) =>
  Symbol ->
  Sem r Int
findRecordFieldIdx f =
  fromMaybeM (throw err) $
    asks @(RecordNameSignature 'Parsed) (^? recordNames . at f . _Just . nameItemIndex)
  where
    err :: ScoperError
    err = ErrUnexpectedField (UnexpectedField f)

checkListPattern ::
  forall r.
  (Members '[Reader PatternNamesKind, Error ScoperError, State Scope, State ScoperState, Reader BindingStrategy, InfoTableBuilder, Reader InfoTable, NameIdGen] r) =>
  ListPattern 'Parsed ->
  Sem r (ListPattern 'Scoped)
checkListPattern l = do
  let _listpBracketL = l ^. listpBracketL
      _listpBracketR = l ^. listpBracketR
  _listpItems <- mapM checkParsePatternAtoms (l ^. listpItems)
  return ListPattern {..}

checkList ::
  forall r.
  (Members '[HighlightBuilder, Reader ScopeParameters, Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, Reader InfoTable, NameIdGen, Reader PackageId] r) =>
  List 'Parsed ->
  Sem r (List 'Scoped)
checkList l = do
  let _listBracketL = l ^. listBracketL
      _listBracketR = l ^. listBracketR
  _listItems <- mapM checkParseExpressionAtoms (l ^. listItems)
  return List {..}

checkLet ::
  forall r.
  (Members '[HighlightBuilder, Reader ScopeParameters, Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, Reader InfoTable, NameIdGen, Reader PackageId] r) =>
  Let 'Parsed ->
  Sem r (Let 'Scoped)
checkLet Let {..} =
  withLocalScope $ do
    letFunDefs' <- checkLetStatements _letFunDefs
    letExpression' <- checkParseExpressionAtoms _letExpression
    return
      Let
        { _letFunDefs = letFunDefs',
          _letExpression = letExpression',
          _letKw,
          _letInKw
        }

checkRhsExpression ::
  forall r.
  ( Members
      '[ HighlightBuilder,
         Reader ScopeParameters,
         Error ScoperError,
         State Scope,
         State ScoperState,
         InfoTableBuilder,
         Reader InfoTable,
         NameIdGen,
         Reader PackageId
       ]
      r
  ) =>
  RhsExpression 'Parsed ->
  Sem r (RhsExpression 'Scoped)
checkRhsExpression RhsExpression {..} = do
  expr' <- checkParseExpressionAtoms _rhsExpression
  return
    RhsExpression
      { _rhsExpression = expr',
        _rhsExpressionAssignKw
      }

checkSideIfBranch ::
  forall r (k :: IfBranchKind).
  ( SingI k,
    Members
      '[ HighlightBuilder,
         Reader ScopeParameters,
         Error ScoperError,
         State Scope,
         State ScoperState,
         InfoTableBuilder,
         Reader InfoTable,
         NameIdGen,
         Reader PackageId
       ]
      r
  ) =>
  SideIfBranch 'Parsed k ->
  Sem r (SideIfBranch 'Scoped k)
checkSideIfBranch SideIfBranch {..} = do
  cond' <- case sing :: SIfBranchKind k of
    SBranchIfBool -> checkParseExpressionAtoms _sideIfBranchCondition
    SBranchIfElse -> return _sideIfBranchCondition
  body' <- checkParseExpressionAtoms _sideIfBranchBody
  return
    SideIfBranch
      { _sideIfBranchBody = body',
        _sideIfBranchCondition = cond',
        _sideIfBranchPipe,
        _sideIfBranchKw,
        _sideIfBranchAssignKw
      }

checkSideIfs ::
  forall r.
  ( Members
      '[ HighlightBuilder,
         Reader ScopeParameters,
         Error ScoperError,
         State Scope,
         State ScoperState,
         InfoTableBuilder,
         Reader InfoTable,
         NameIdGen,
         Reader PackageId
       ]
      r
  ) =>
  SideIfs 'Parsed ->
  Sem r (SideIfs 'Scoped)
checkSideIfs SideIfs {..} = do
  branches' <- mapM checkSideIfBranch _sideIfBranches
  else' <- mapM checkSideIfBranch _sideIfElse
  return
    SideIfs
      { _sideIfBranches = branches',
        _sideIfElse = else'
      }

checkCaseBranchRhs ::
  forall r.
  (Members '[HighlightBuilder, Reader ScopeParameters, Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, Reader InfoTable, NameIdGen, Reader PackageId] r) =>
  CaseBranchRhs 'Parsed ->
  Sem r (CaseBranchRhs 'Scoped)
checkCaseBranchRhs = \case
  CaseBranchRhsExpression r -> CaseBranchRhsExpression <$> checkRhsExpression r
  CaseBranchRhsIf r -> CaseBranchRhsIf <$> checkSideIfs r

checkCaseBranch ::
  forall r.
  (Members '[HighlightBuilder, Reader ScopeParameters, Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, Reader InfoTable, NameIdGen, Reader PackageId] r) =>
  CaseBranch 'Parsed ->
  Sem r (CaseBranch 'Scoped)
checkCaseBranch CaseBranch {..} = withLocalScope $ do
  pattern' <- checkParsePatternAtoms' _caseBranchPattern
  rhs' <- checkCaseBranchRhs _caseBranchRhs
  return $
    CaseBranch
      { _caseBranchPattern = pattern',
        _caseBranchRhs = rhs',
        ..
      }

checkDoBind ::
  (Members '[HighlightBuilder, Reader ScopeParameters, Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, Reader InfoTable, NameIdGen, Reader PackageId] r) =>
  DoBind 'Parsed ->
  Sem r (DoBind 'Scoped)
checkDoBind DoBind {..} = do
  expr' <- checkParseExpressionAtoms _doBindExpression
  pat' <- checkParsePatternAtoms' _doBindPattern
  unless (Explicit == pat' ^. patternArgIsImplicit) $
    throw (ErrDoBindImplicitPattern (DoBindImplicitPattern pat'))
  return
    DoBind
      { _doBindArrowKw,
        _doBindPattern = pat',
        _doBindExpression = expr'
      }

checkDoLet ::
  (Members '[HighlightBuilder, Reader ScopeParameters, Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, Reader InfoTable, NameIdGen, Reader PackageId] r) =>
  DoLet 'Parsed ->
  Sem r (DoLet 'Scoped)
checkDoLet DoLet {..} = do
  defs' <- checkLetStatements _doLetStatements
  return
    DoLet
      { _doLetKw,
        _doLetInKw,
        _doLetStatements = defs'
      }

checkDoStatement ::
  (Members '[HighlightBuilder, Reader ScopeParameters, Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, Reader InfoTable, NameIdGen, Reader PackageId] r) =>
  DoStatement 'Parsed ->
  Sem r (DoStatement 'Scoped)
checkDoStatement = \case
  DoStatementExpression e -> DoStatementExpression <$> checkParseExpressionAtoms e
  DoStatementBind b -> DoStatementBind <$> checkDoBind b
  DoStatementLet b -> DoStatementLet <$> checkDoLet b

checkDo ::
  (Members '[HighlightBuilder, Reader ScopeParameters, Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, Reader InfoTable, NameIdGen, Reader PackageId] r) =>
  Do 'Parsed ->
  Sem r (Do 'Scoped)
checkDo Do {..} = do
  stmts' <- mapM checkDoStatement _doStatements
  return
    Do
      { _doStatements = stmts',
        _doKeyword,
        _doDelims
      }

checkCase ::
  (Members '[HighlightBuilder, Reader ScopeParameters, Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, Reader InfoTable, NameIdGen, Reader PackageId] r) =>
  Case 'Parsed ->
  Sem r (Case 'Scoped)
checkCase Case {..} = do
  caseBranches' <- mapM checkCaseBranch _caseBranches
  caseExpression' <- checkParseExpressionAtoms _caseExpression
  return $
    Case
      { _caseExpression = caseExpression',
        _caseBranches = caseBranches',
        _caseKw,
        _caseOfKw
      }

checkIfBranch ::
  forall r k.
  (SingI k, Members '[HighlightBuilder, Reader ScopeParameters, Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, Reader InfoTable, NameIdGen, Reader PackageId] r) =>
  IfBranch 'Parsed k ->
  Sem r (IfBranch 'Scoped k)
checkIfBranch IfBranch {..} = withLocalScope $ do
  cond' <- case sing :: SIfBranchKind k of
    SBranchIfBool -> checkParseExpressionAtoms _ifBranchCondition
    SBranchIfElse -> return _ifBranchCondition
  expression' <- checkParseExpressionAtoms _ifBranchExpression
  return $
    IfBranch
      { _ifBranchCondition = cond',
        _ifBranchExpression = expression',
        ..
      }

checkIf ::
  (Members '[HighlightBuilder, Reader ScopeParameters, Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, Reader InfoTable, NameIdGen, Reader PackageId] r) =>
  If 'Parsed ->
  Sem r (If 'Scoped)
checkIf If {..} = do
  ifBranches' <- mapM checkIfBranch _ifBranches
  ifBranchElse' <- checkIfBranch _ifBranchElse
  return $
    If
      { _ifBranchElse = ifBranchElse',
        _ifBranches = ifBranches',
        _ifKw
      }

checkLambda ::
  (Members '[HighlightBuilder, Reader ScopeParameters, Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, Reader InfoTable, NameIdGen, Reader PackageId] r) =>
  Lambda 'Parsed ->
  Sem r (Lambda 'Scoped)
checkLambda Lambda {..} = do
  clauses' <- mapM checkLambdaClause _lambdaClauses
  return
    Lambda
      { _lambdaClauses = clauses',
        _lambdaKw,
        _lambdaBraces
      }

checkLambdaClause ::
  (Members '[HighlightBuilder, Reader ScopeParameters, Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, Reader InfoTable, NameIdGen, Reader PackageId] r) =>
  LambdaClause 'Parsed ->
  Sem r (LambdaClause 'Scoped)
checkLambdaClause LambdaClause {..} = withLocalScope $ do
  lambdaParameters' <- mapM checkParsePatternAtom' _lambdaParameters
  lambdaBody' <- checkParseExpressionAtoms _lambdaBody
  return
    LambdaClause
      { _lambdaParameters = lambdaParameters',
        _lambdaBody = lambdaBody',
        _lambdaPipe,
        _lambdaAssignKw
      }

checkUnqualifiedName ::
  (Members '[Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, Reader InfoTable] r) =>
  Symbol ->
  Sem r PreSymbolEntry
checkUnqualifiedName s = do
  scope <- get
  -- Lookup at the global scope
  entries <- fst3 <$> lookupQualifiedSymbol ([], s)
  case resolveShadowing (toList entries) of
    [] -> throw (ErrSymNotInScope (NotInScope s scope))
    [x] -> return x
    es -> throw (ErrAmbiguousSym (AmbiguousSym n es))
  where
    n = NameUnqualified s

checkFixitySymbol ::
  (Members '[Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, Reader InfoTable] r) =>
  Name ->
  Sem r ScopedIden
checkFixitySymbol s = do
  -- Lookup at the global scope
  entries <- thd3 <$> lookupQualifiedSymbol (splitName s)
  case resolveShadowing (toList entries) of
    [] -> nameNotInScope s
    [entry :: FixitySymbolEntry] -> do
      res <- entryToScopedIden s entry
      registerScopedIden False res
      return res
    es -> throw (ErrAmbiguousSym (AmbiguousSym s (map (PreSymbolFinal . SymbolEntry . (^. fixityEntry)) es)))

-- | Remove the symbol entries associated with a single symbol according to the
-- shadowing rules for modules. For example, a symbol defined in the outer
-- module with the same name as a symbol defined in the inner module will be
-- removed.
resolveShadowing :: forall ns. (SingI ns) => [NameSpaceEntryType ns] -> [NameSpaceEntryType ns]
resolveShadowing es = go [(e, e ^. nsEntry . S.nameWhyInScope) | e <- es]
  where
    go :: [(NameSpaceEntryType ns, S.WhyInScope)] -> [NameSpaceEntryType ns]
    go itms
      | any (((== S.BecauseImportedOpened) .||. (== S.BecauseDefined)) . snd) itms =
          [e | (e, w) <- itms, not (isInherited w)]
      | (notNull .&&. all (isInherited . snd)) itms = go [(e, peelInherited w) | (e, w) <- itms]
      | otherwise = map fst itms
      where
        peelInherited :: S.WhyInScope -> S.WhyInScope
        peelInherited = \case
          S.BecauseInherited w -> w
          _ -> impossible

        isInherited :: S.WhyInScope -> Bool
        isInherited = \case
          S.BecauseInherited {} -> True
          _ -> False

checkPatternName' ::
  forall r.
  (Members '[Reader PatternNamesKind, Error ScoperError, State Scope, NameIdGen, State ScoperState, Reader BindingStrategy, InfoTableBuilder, Reader InfoTable] r) =>
  (Symbol -> Sem r S.Symbol) ->
  Name ->
  Sem r PatternScopedIden
checkPatternName' bindFun n = do
  c <- getConstructorRef
  case c of
    Just constr -> return (PatternScopedConstructor constr)
    Nothing -> case n of
      NameUnqualified {} -> do
        pk <- ask
        PatternScopedVar
          <$> case pk of
            PatternNamesKindVariables ->
              bindVariableSymbol sym
            PatternNamesKindFunctions -> do
              bindFun sym
      NameQualified {} -> nameNotInScope n
  where
    sym = snd (splitName n)
    getConstructorRef :: Sem r (Maybe ScopedIden)
    getConstructorRef = lookupNameOfKind KNameConstructor n

checkPatternName ::
  forall r.
  (Members '[Reader PatternNamesKind, Error ScoperError, State Scope, NameIdGen, State ScoperState, Reader BindingStrategy, InfoTableBuilder, Reader InfoTable] r) =>
  Name ->
  Sem r PatternScopedIden
checkPatternName = checkPatternName' getReservedDefinitionSymbol

reservePatternName ::
  forall r.
  (Members '[Error ScoperError, State Scope, NameIdGen, State ScoperState, Reader BindingStrategy, InfoTableBuilder, Reader InfoTable] r) =>
  Name ->
  Sem r PatternScopedIden
reservePatternName =
  runReader PatternNamesKindFunctions
    . checkPatternName' (reserveSymbolOf SKNameFunction Nothing Nothing)

nameNotInScope :: forall r a. (Members '[Error ScoperError, State Scope] r) => Name -> Sem r a
nameNotInScope n = err >>= throw
  where
    err :: Sem r ScoperError
    err = case n of
      NameQualified q -> return (ErrQualSymNotInScope (QualSymNotInScope q))
      NameUnqualified s -> ErrSymNotInScope . NotInScope s <$> get

getNameOfKind ::
  forall r.
  (Members '[Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, Reader InfoTable] r) =>
  NameKind ->
  Name ->
  Sem r ScopedIden
getNameOfKind nameKind n = fromMaybeM (nameNotInScope n) (lookupNameOfKind nameKind n)

lookupNameOfKind ::
  forall r.
  (Members '[Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, Reader InfoTable] r) =>
  NameKind ->
  Name ->
  Sem r (Maybe ScopedIden)
lookupNameOfKind nameKind n = do
  entries <- lookupQualifiedSymbol (path, sym) >>= mapMaybeM filterEntry . toList . fst3
  case entries of
    [] -> return Nothing
    [(_, s)] -> return (Just s) -- There is one constructor with such a name
    es -> throw (ErrAmbiguousSym (AmbiguousSym n (map fst es)))
  where
    (path, sym) = splitName n

    filterEntry :: PreSymbolEntry -> Sem r (Maybe (PreSymbolEntry, ScopedIden))
    filterEntry e = do
      e' <- entryToScopedIden n e
      return $ do
        guard (nameKind == getNameKind e')
        return (e, e')

checkPatternBinding ::
  (Members '[Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, Reader InfoTable, NameIdGen] r) =>
  PatternBinding ->
  Sem r PatternArg
checkPatternBinding PatternBinding {..} = do
  p' <- checkParsePatternAtom' _patternBindingPattern
  n' <- bindVariableSymbol _patternBindingName
  if
      | isJust (p' ^. patternArgName) -> throw (ErrDoubleBinderPattern (DoubleBinderPattern n' p'))
      | otherwise -> return (set patternArgName (Just n') p')

checkPatternAtoms ::
  (Members '[Reader PatternNamesKind, Error ScoperError, State Scope, State ScoperState, Reader BindingStrategy, InfoTableBuilder, Reader InfoTable, NameIdGen] r) =>
  PatternAtoms 'Parsed ->
  Sem r (PatternAtoms 'Scoped)
checkPatternAtoms (PatternAtoms s i) = (`PatternAtoms` i) <$> mapM checkPatternAtom s

checkParsePatternAtoms ::
  (Members '[Reader PatternNamesKind, Error ScoperError, State Scope, State ScoperState, Reader BindingStrategy, InfoTableBuilder, Reader InfoTable, NameIdGen] r) =>
  PatternAtoms 'Parsed ->
  Sem r PatternArg
checkParsePatternAtoms = checkPatternAtoms >=> parsePatternAtoms

checkParsePatternAtoms' ::
  (Members '[Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, Reader InfoTable, NameIdGen] r) =>
  PatternAtoms 'Parsed ->
  Sem r PatternArg
checkParsePatternAtoms' = localBindings . runReader PatternNamesKindVariables . checkParsePatternAtoms

checkPatternAtom ::
  (Members '[Reader PatternNamesKind, Error ScoperError, State Scope, State ScoperState, Reader BindingStrategy, InfoTableBuilder, Reader InfoTable, NameIdGen] r) =>
  PatternAtom 'Parsed ->
  Sem r (PatternAtom 'Scoped)
checkPatternAtom = \case
  PatternAtomIden n -> PatternAtomIden <$> checkPatternName n
  PatternAtomWildcard i -> return (PatternAtomWildcard i)
  PatternAtomEmpty i -> return (PatternAtomEmpty i)
  PatternAtomParens e -> PatternAtomParens <$> checkParsePatternAtoms e
  PatternAtomBraces e -> PatternAtomBraces <$> checkParsePatternAtoms e
  PatternAtomDoubleBraces e -> PatternAtomDoubleBraces <$> checkParsePatternAtoms e
  PatternAtomAt p -> PatternAtomAt <$> checkPatternBinding p
  PatternAtomList l -> PatternAtomList <$> checkListPattern l
  PatternAtomRecord l -> PatternAtomRecord <$> checkRecordPattern l
  PatternAtomWildcardConstructor l -> PatternAtomWildcardConstructor <$> checkWildcardConstructor l

checkWildcardConstructor :: (Members '[InfoTableBuilder, Reader InfoTable, State ScoperState, Error ScoperError, State Scope] r) => WildcardConstructor 'Parsed -> Sem r (WildcardConstructor 'Scoped)
checkWildcardConstructor WildcardConstructor {..} = do
  let err = nameNotInScope _wildcardConstructor
  c' <- fromMaybeM err (lookupNameOfKind KNameConstructor _wildcardConstructor)
  return
    WildcardConstructor
      { _wildcardConstructor = c',
        ..
      }

checkName ::
  (Members '[Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, Reader InfoTable] r) =>
  Name ->
  Sem r PreSymbolEntry
checkName n = case n of
  NameQualified q -> checkQualifiedName q
  NameUnqualified s -> checkUnqualifiedName s

checkScopedIden ::
  (Members '[Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, Reader InfoTable] r) =>
  Name ->
  Sem r ScopedIden
checkScopedIden n = checkName n >>= entryToScopedIden n

checkExpressionAtom ::
  (Members '[HighlightBuilder, Reader ScopeParameters, Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, Reader InfoTable, NameIdGen, Reader PackageId] r) =>
  ExpressionAtom 'Parsed ->
  Sem r (NonEmpty (ExpressionAtom 'Scoped))
checkExpressionAtom e = case e of
  AtomIdentifier n -> pure . AtomIdentifier <$> checkScopedIden n
  AtomLambda lam -> pure . AtomLambda <$> checkLambda lam
  AtomDo a -> pure . AtomDo <$> checkDo a
  AtomCase c -> pure . AtomCase <$> checkCase c
  AtomIf c -> pure . AtomIf <$> checkIf c
  AtomLet letBlock -> pure . AtomLet <$> checkLet letBlock
  AtomUniverse uni -> return (pure (AtomUniverse uni))
  AtomFunction fun -> pure . AtomFunction <$> checkFunction fun
  AtomParens par -> pure . AtomParens <$> checkParens par
  AtomDoubleBraces br -> pure . AtomDoubleBraces <$> traverseOf doubleBracesExpression checkParseExpressionAtoms br
  AtomBraces br -> pure . AtomBraces <$> traverseOf withLocParam checkParseExpressionAtoms br
  AtomFunArrow a -> return (pure (AtomFunArrow a))
  AtomHole h -> pure . AtomHole <$> checkHole h
  AtomInstanceHole h -> pure . AtomInstanceHole <$> checkHole h
  AtomLiteral l -> return (pure (AtomLiteral l))
  AtomList l -> pure . AtomList <$> checkList l
  AtomIterator i -> pure . AtomIterator <$> checkIterator i
  AtomNamedApplication i -> pure . AtomNamedApplication <$> checkNamedApplication i
  AtomRecordUpdate i -> pure . AtomRecordUpdate <$> checkRecordUpdate i

reserveNamedArgumentName :: (Members '[Error ScoperError, NameIdGen, State Scope, State ScoperState, Reader BindingStrategy, InfoTableBuilder, Reader InfoTable] r) => NamedArgument 'Parsed -> Sem r ()
reserveNamedArgumentName a = case a of
  NamedArgumentFunction f -> reserveFunctionLikeSymbol (f ^. namedArgumentFunctionDef)
  NamedArgumentItemPun {} -> return ()

checkNamedApplication ::
  forall r.
  (Members '[HighlightBuilder, Error ScoperError, State Scope, State ScoperState, Reader ScopeParameters, InfoTableBuilder, Reader InfoTable, NameIdGen, Reader PackageId] r) =>
  NamedApplication 'Parsed ->
  Sem r (NamedApplication 'Scoped)
checkNamedApplication napp = do
  let nargs = napp ^. namedApplicationArguments
  aname <- checkScopedIden (napp ^. namedApplicationName)
  sig :: NameSignature 'Parsed <-
    if
        | null nargs -> return (NameSignature [])
        | otherwise -> getNameSignatureParsed aname
  let namesInSignature =
        hashSet $
          sig
            ^.. nameSignatureArgs
              . each
              . nameBlockSymbols
  forM_ nargs (checkNameInSignature namesInSignature . (^. namedArgumentSymbol))
  puns <- scopePuns (napp ^.. namedApplicationArguments . each . _NamedArgumentItemPun)
  args' <- withLocalScope . localBindings $ do
    mapM_ reserveNamedArgumentName nargs
    mapM (checkNamedArgument puns) nargs
  let signatureExplicitNames =
        hashSet
          . concatMap (^.. nameBlockSymbols)
          . filter (not . isImplicitOrInstance . (^. nameBlockImplicit))
          $ sig ^. nameSignatureArgs
      givenNames :: [Symbol] = map (^. namedArgumentSymbol) nargs
  checkRepeated givenNames
  let missingArgs = HashSet.difference signatureExplicitNames (hashSet givenNames)
  unless (null missingArgs) $
    throw (ErrMissingArgs (MissingArgs (aname ^. scopedIdenFinal . nameConcrete) missingArgs))
  return
    NamedApplication
      { _namedApplicationName = aname,
        _namedApplicationArguments = args',
        _namedApplicationAtKw = napp ^. namedApplicationAtKw
      }
  where
    checkRepeated :: [Symbol] -> Sem r ()
    checkRepeated syms = whenJust (findRepeatedOn id syms ^? _head . _1) $
      \(x, y :| _) ->
        throw $
          ErrMultipleDeclarations
            MultipleDeclarations
              { _multipleDeclFirst = getLoc x,
                _multipleDeclSecond = y
              }

    checkNameInSignature :: HashSet Symbol -> Symbol -> Sem r ()
    checkNameInSignature namesInSig fname =
      unless (HashSet.member fname namesInSig) $
        throw (ErrUnexpectedArgument (UnexpectedArgument fname))

    scopePuns :: [NamedArgumentPun s] -> Sem r (HashMap Symbol ScopedIden)
    scopePuns puns = hashMap <$> mapWithM scopePun (puns ^.. each . namedArgumentPunSymbol)
      where
        scopePun :: Symbol -> Sem r ScopedIden
        scopePun = checkScopedIden . NameUnqualified

checkNamedArgument ::
  (Members '[HighlightBuilder, Error ScoperError, State Scope, State ScoperState, Reader ScopeParameters, InfoTableBuilder, Reader InfoTable, NameIdGen, Reader PackageId] r) =>
  HashMap Symbol ScopedIden ->
  NamedArgument 'Parsed ->
  Sem r (NamedArgument 'Scoped)
checkNamedArgument puns = \case
  NamedArgumentFunction f -> NamedArgumentFunction <$> checkNamedArgumentFunctionDef f
  NamedArgumentItemPun f -> return (NamedArgumentItemPun (checkNamedArgumentItemPun puns f))

checkNamedArgumentItemPun ::
  HashMap Symbol ScopedIden ->
  NamedArgumentPun 'Parsed ->
  (NamedArgumentPun 'Scoped)
checkNamedArgumentItemPun puns NamedArgumentPun {..} =
  NamedArgumentPun
    { _namedArgumentPunSymbol = _namedArgumentPunSymbol,
      _namedArgumentReferencedSymbol = fromJust (puns ^. at _namedArgumentPunSymbol)
    }

checkNamedArgumentFunctionDef ::
  (Members '[HighlightBuilder, Error ScoperError, State Scope, State ScoperState, Reader ScopeParameters, InfoTableBuilder, Reader InfoTable, NameIdGen, Reader PackageId] r) =>
  NamedArgumentFunctionDef 'Parsed ->
  Sem r (NamedArgumentFunctionDef 'Scoped)
checkNamedArgumentFunctionDef NamedArgumentFunctionDef {..} = do
  def <- localBindings $ checkFunctionDef _namedArgumentFunctionDef
  return
    NamedArgumentFunctionDef
      { _namedArgumentFunctionDef = def
      }

checkRecordUpdate :: forall r. (Members '[HighlightBuilder, Error ScoperError, State Scope, State ScoperState, Reader ScopeParameters, InfoTableBuilder, Reader InfoTable, NameIdGen, Reader PackageId] r) => RecordUpdate 'Parsed -> Sem r (RecordUpdate 'Scoped)
checkRecordUpdate RecordUpdate {..} = do
  tyName' <- getNameOfKind KNameInductive _recordUpdateTypeName
  info <- getRecordInfo tyName'
  let sig = info ^. recordInfoSignature
  (vars' :: IntMap (IsImplicit, S.Symbol), fields') <- withLocalScope $ do
    vs <- mapM bindRecordUpdateVariable (P.recordNameSignatureByIndex sig)
    fs <- runReader sig (mapM checkUpdateField _recordUpdateFields)
    return (vs, fs)
  let extra' =
        RecordUpdateExtra
          { _recordUpdateExtraVars = vars',
            _recordUpdateExtraConstructor = info ^. recordInfoConstructor
          }
  return
    RecordUpdate
      { _recordUpdateTypeName = tyName',
        _recordUpdateFields = fields',
        _recordUpdateExtra = Irrelevant extra',
        _recordUpdateAtKw,
        _recordUpdateDelims
      }
  where
    bindRecordUpdateVariable :: NameItem 'Parsed -> Sem r (IsImplicit, S.Symbol)
    bindRecordUpdateVariable NameItem {..} = do
      -- all fields have names so it is safe to use fromJust
      v <- freshVariable (fromJust _nameItemSymbol)
      return (_nameItemImplicit, v)

checkUpdateField ::
  forall r.
  (Members '[HighlightBuilder, Error ScoperError, State Scope, State ScoperState, Reader ScopeParameters, InfoTableBuilder, Reader InfoTable, NameIdGen, Reader PackageId, Reader (RecordNameSignature 'Parsed)] r) =>
  RecordUpdateField 'Parsed ->
  Sem r (RecordUpdateField 'Scoped)
checkUpdateField = \case
  RecordUpdateFieldAssign a -> RecordUpdateFieldAssign <$> checkUpdateFieldAssign a
  RecordUpdateFieldPun a -> RecordUpdateFieldPun <$> checkRecordPun a
  where
    checkRecordPun :: RecordUpdatePun 'Parsed -> Sem r (RecordUpdatePun 'Scoped)
    checkRecordPun RecordUpdatePun {..} = do
      idx <- findRecordFieldIdx _recordUpdatePunSymbol
      s <- checkScopedIden (NameUnqualified _recordUpdatePunSymbol)
      return
        RecordUpdatePun
          { _recordUpdatePunSymbol,
            _recordUpdatePunReferencedSymbol = s,
            _recordUpdatePunFieldIndex = idx
          }

getUpdateFieldIdx ::
  (Member (Error ScoperError) r) =>
  RecordNameSignature s2 ->
  RecordUpdateFieldItemAssign s ->
  Sem r Int
getUpdateFieldIdx sig f =
  maybe (throw unexpectedField) return (sig ^? recordNames . at (f ^. fieldUpdateName) . _Just . nameItemIndex)
  where
    unexpectedField :: ScoperError
    unexpectedField = ErrUnexpectedField (UnexpectedField (f ^. fieldUpdateName))

checkUpdateFieldAssign ::
  (Members '[Reader (RecordNameSignature 'Parsed), HighlightBuilder, Error ScoperError, State Scope, State ScoperState, Reader ScopeParameters, InfoTableBuilder, Reader InfoTable, NameIdGen, Reader PackageId] r) =>
  RecordUpdateFieldItemAssign 'Parsed ->
  Sem r (RecordUpdateFieldItemAssign 'Scoped)
checkUpdateFieldAssign f = do
  sig <- ask @(RecordNameSignature 'Parsed)
  value' <- checkParseExpressionAtoms (f ^. fieldUpdateValue)
  idx' <- getUpdateFieldIdx sig f
  return
    RecordUpdateFieldItemAssign
      { _fieldUpdateName = f ^. fieldUpdateName,
        _fieldUpdateArgIx = idx',
        _fieldUpdateAssignKw = f ^. fieldUpdateAssignKw,
        _fieldUpdateValue = value'
      }

getRecordInfo ::
  forall r.
  (Members '[State ScoperState, Error ScoperError] r) =>
  ScopedIden ->
  Sem r RecordInfo
getRecordInfo indTy = getRecordInfo' (getLoc indTy) (indTy ^. scopedIdenFinal . nameConcrete) (indTy ^. scopedIdenFinal . S.nameId)

getRecordInfo' ::
  forall r.
  (Members '[State ScoperState, Error ScoperError] r) =>
  Interval ->
  Name ->
  NameId ->
  Sem r RecordInfo
getRecordInfo' loc name nameId =
  fromMaybeM err (gets (^. scoperRecordFields . at nameId))
  where
    err :: Sem r a
    err = throw (ErrNotARecord (NotARecord name loc))

getNameSignatureParsed :: (Members '[State ScoperState, Error ScoperError] r) => ScopedIden -> Sem r (NameSignature 'Parsed)
getNameSignatureParsed s = do
  sig <- maybeM (throw err) return (lookupNameSignature (s ^. scopedIdenFinal . S.nameId))
  when (null (sig ^. nameSignatureArgs)) (throw err)
  return sig
  where
    err = ErrNoNameSignature (NoNameSignature s)

    lookupNameSignature :: (Members '[State ScoperState] r) => S.NameId -> Sem r (Maybe (NameSignature 'Parsed))
    lookupNameSignature s' = gets (^. scoperNameSignatures . at s')

checkIterator ::
  (Members '[HighlightBuilder, Reader ScopeParameters, Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, Reader InfoTable, NameIdGen, Reader PackageId] r) =>
  Iterator 'Parsed ->
  Sem r (Iterator 'Scoped)
checkIterator iter = do
  _iteratorName <- checkScopedIden (iter ^. iteratorName)
  case _iteratorName ^. scopedIdenSrcName . S.nameIterator of
    Just IteratorInfo {..} -> do
      case _iteratorInfoInitNum of
        Just n
          | n /= length (iter ^. iteratorInitializers) ->
              throw
                ( ErrIteratorInitializer
                    IteratorInitializer {_iteratorInitializerIterator = iter}
                )
        _ -> return ()
      case _iteratorInfoRangeNum of
        Just n
          | n /= length (iter ^. iteratorRanges) ->
              throw
                ( ErrIteratorRange
                    IteratorRange {_iteratorRangeIterator = iter}
                )
        _ -> return ()
    Nothing ->
      throw
        ( ErrIteratorUndefined
            IteratorUndefined {_iteratorUndefinedIterator = iter}
        )
  inivals' <- mapM (checkParseExpressionAtoms . (^. initializerExpression)) (iter ^. iteratorInitializers)
  rngvals' <- mapM (checkParseExpressionAtoms . (^. rangeExpression)) (iter ^. iteratorRanges)
  let initAssignKws = iter ^.. iteratorInitializers . each . initializerAssignKw
      rangesInKws = iter ^.. iteratorRanges . each . rangeInKw
  withLocalScope $ do
    inipats' <- mapM (checkParsePatternAtoms' . (^. initializerPattern)) (iter ^. iteratorInitializers)
    rngpats' <- mapM (checkParsePatternAtoms' . (^. rangePattern)) (iter ^. iteratorRanges)
    let _iteratorInitializers = [Initializer p k v | ((p, k), v) <- zipExact (zipExact inipats' initAssignKws) inivals']
        _iteratorRanges = [Range p k v | ((p, k), v) <- zipExact (zipExact rngpats' rangesInKws) rngvals']
        _iteratorParens = iter ^. iteratorParens
        _iteratorBodyBraces = iter ^. iteratorBodyBraces
    _iteratorBody <- checkParseExpressionAtoms (iter ^. iteratorBody)
    return Iterator {..}

checkHole ::
  (Members '[NameIdGen] r) =>
  HoleType 'Parsed ->
  Sem r Hole
checkHole h = do
  i <- freshNameId
  return
    Hole
      { _holeId = i,
        _holeKw = h
      }

checkParens ::
  (Members '[HighlightBuilder, Reader ScopeParameters, Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, Reader InfoTable, NameIdGen, Reader PackageId] r) =>
  ExpressionAtoms 'Parsed ->
  Sem r Expression
checkParens e@(ExpressionAtoms as _) = case as of
  p :| [] -> case p of
    AtomIdentifier s -> do
      scopedId <- checkScopedIden s
      let scopedIdenNoFix = over scopedIdenSrcName (set S.nameFixity Nothing) scopedId
      return (ExpressionParensIdentifier scopedIdenNoFix)
    AtomIterator i -> ExpressionIterator . set iteratorParens True <$> checkIterator i
    AtomRecordUpdate u -> ExpressionParensRecordUpdate . ParensRecordUpdate <$> checkRecordUpdate u
    _ -> checkParseExpressionAtoms e
  _ -> checkParseExpressionAtoms e

checkExpressionAtoms ::
  forall r.
  (Members '[HighlightBuilder, Reader ScopeParameters, Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, Reader InfoTable, NameIdGen, Reader PackageId] r) =>
  ExpressionAtoms 'Parsed ->
  Sem r (ExpressionAtoms 'Scoped)
checkExpressionAtoms (ExpressionAtoms l i) = (`ExpressionAtoms` i) <$> sconcatMap checkExpressionAtom l

checkJudoc ::
  (Members '[Reader ScopeParameters, Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, Reader InfoTable, NameIdGen, Reader PackageId] r) =>
  Judoc 'Parsed ->
  Sem r (Judoc 'Scoped)
checkJudoc (Judoc groups) =
  evalHighlightBuilder
    . ignoreInfoTableBuilder
    $ Judoc <$> mapM checkJudocGroup groups

checkJudocGroup ::
  (Members '[HighlightBuilder, Reader ScopeParameters, Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, Reader InfoTable, NameIdGen, Reader PackageId] r) =>
  JudocGroup 'Parsed ->
  Sem r (JudocGroup 'Scoped)
checkJudocGroup = \case
  JudocGroupBlock b -> JudocGroupBlock <$> checkJudocBlockParagraph b
  JudocGroupLines l -> JudocGroupLines <$> mapM checkJudocBlock l

checkJudocBlock ::
  (Members '[HighlightBuilder, Reader ScopeParameters, Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, Reader InfoTable, NameIdGen, Reader PackageId] r) =>
  JudocBlock 'Parsed ->
  Sem r (JudocBlock 'Scoped)
checkJudocBlock = \case
  JudocLines l -> JudocLines <$> mapM checkJudocLine l

checkJudocBlockParagraph ::
  (Members '[HighlightBuilder, Reader ScopeParameters, Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, Reader InfoTable, NameIdGen, Reader PackageId] r) =>
  JudocBlockParagraph 'Parsed ->
  Sem r (JudocBlockParagraph 'Scoped)
checkJudocBlockParagraph = traverseOf judocBlockParagraphBlocks (mapM checkJudocBlock)

checkJudocLine ::
  (Members '[HighlightBuilder, Reader ScopeParameters, Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, Reader InfoTable, NameIdGen, Reader PackageId] r) =>
  JudocLine 'Parsed ->
  Sem r (JudocLine 'Scoped)
checkJudocLine (JudocLine delim atoms) = JudocLine delim <$> mapM (mapM checkJudocAtom) atoms

checkJudocAtom ::
  (Members '[HighlightBuilder, Reader ScopeParameters, Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, Reader InfoTable, NameIdGen, Reader PackageId] r) =>
  JudocAtom 'Parsed ->
  Sem r (JudocAtom 'Scoped)
checkJudocAtom = \case
  JudocText t -> return (JudocText t)
  JudocExpression e -> JudocExpression <$> checkParseExpressionAtoms e

checkParseExpressionAtoms ::
  forall r.
  (Members '[HighlightBuilder, Reader ScopeParameters, Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, Reader InfoTable, NameIdGen, Reader PackageId] r) =>
  ExpressionAtoms 'Parsed ->
  Sem r Expression
checkParseExpressionAtoms = checkExpressionAtoms >=> parseExpressionAtoms

checkParsePatternAtom ::
  (Members '[Reader PatternNamesKind, Error ScoperError, State Scope, State ScoperState, Reader BindingStrategy, InfoTableBuilder, Reader InfoTable, NameIdGen] r) =>
  PatternAtom 'Parsed ->
  Sem r PatternArg
checkParsePatternAtom = checkPatternAtom >=> parsePatternAtom

checkParsePatternAtom' ::
  (Members '[Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, Reader InfoTable, NameIdGen] r) =>
  PatternAtom 'Parsed ->
  Sem r PatternArg
checkParsePatternAtom' = localBindings . runReader PatternNamesKindVariables . checkParsePatternAtom

checkSyntaxDef ::
  (Members '[Reader BindingStrategy, Error ScoperError, Reader ScopeParameters, State Scope, State ScoperState, InfoTableBuilder, Reader InfoTable, NameIdGen, Reader PackageId] r) =>
  SyntaxDef 'Parsed ->
  Sem r (SyntaxDef 'Scoped)
checkSyntaxDef = \case
  SyntaxFixity fixDef -> SyntaxFixity <$> checkFixitySyntaxDef fixDef
  SyntaxAlias a -> SyntaxAlias <$> checkAliasDef a
  SyntaxOperator opDef -> SyntaxOperator <$> checkOperatorSyntaxDef opDef
  SyntaxIterator iterDef -> SyntaxIterator <$> checkIteratorSyntaxDef iterDef

checkAliasDef ::
  forall r.
  (Members '[Reader BindingStrategy, Reader PackageId, Reader ScopeParameters, Reader InfoTable, InfoTableBuilder, NameIdGen, Error ScoperError, Reader ScopeParameters, State Scope, State ScoperState, InfoTableBuilder, Reader InfoTable, NameIdGen] r) =>
  AliasDef 'Parsed ->
  Sem r (AliasDef 'Scoped)
checkAliasDef def@AliasDef {..} = do
  asName :: ScopedIden <- checkScopedIden _aliasDefAsName
  aliasName' <-
    set S.nameKindPretty (getNameKindPretty asName)
      . set S.nameKind (getNameKind asName)
      <$> reserveAliasDef def
  registerName True aliasName'
  let aliasId = aliasName' ^. S.nameId
  modify' (set (scoperAlias . at aliasId) (Just asName))
  inheritFixityAndIterator aliasId asName
  registerAlias aliasId asName
  doc' <- maybe (return Nothing) (return . Just <=< checkJudoc) _aliasDefDoc
  return
    AliasDef
      { _aliasDefName = aliasName',
        _aliasDefAsName = asName,
        _aliasDefDoc = doc',
        ..
      }
  where
    inheritFixityAndIterator :: (Members '[State Scope] r') => NameId -> ScopedIden -> Sem r' ()
    inheritFixityAndIterator aliasId asName = do
      let targetId = asName ^. scopedIdenSrcName . S.nameId
      whenJustM (gets (^. scopeFixities . at targetId)) $ \fx ->
        (modify (set (scopeFixities . at aliasId) (Just fx)))

      whenJustM (gets (^. scopeIterators . at targetId)) $ \iter ->
        (modify (set (scopeIterators . at aliasId) (Just iter)))

reserveAliasDef ::
  (Members '[Error ScoperError, Reader ScopeParameters, State Scope, State ScoperState, InfoTableBuilder, Reader InfoTable, NameIdGen, Reader BindingStrategy] r) =>
  AliasDef 'Parsed ->
  Sem r S.Symbol
reserveAliasDef = reserveAliasSymbol

reserveSyntaxDef ::
  (Members '[Reader PackageId, Reader ScopeParameters, Reader InfoTable, InfoTableBuilder, NameIdGen, Error ScoperError, Reader ScopeParameters, State Scope, State ScoperState, InfoTableBuilder, Reader InfoTable, NameIdGen, Reader BindingStrategy] r) =>
  SyntaxDef 'Parsed ->
  Sem r ()
reserveSyntaxDef = \case
  SyntaxFixity fixDef -> reserveFixitySyntaxDef fixDef
  SyntaxOperator {} -> return ()
  SyntaxIterator {} -> return ()
  -- NOTE we don't reserve alias because we don't allow alias to be forward
  -- referenced. This avoids alias cycles.
  SyntaxAlias {} -> return ()

-------------------------------------------------------------------------------
-- Check precedences are comparable
-------------------------------------------------------------------------------

checkPrecedences ::
  forall r.
  (Members '[Error ScoperError, InfoTableBuilder, Reader InfoTable] r) =>
  [S.Name] ->
  Sem r ()
checkPrecedences opers = do
  graph <- getPrecedenceGraph
  let fids = mapMaybe (^. fixityId) $ mapMaybe (^. S.nameFixity) opers
      deps = createDependencyInfo graph mempty
  mapM_ (uncurry (checkPath deps)) $
    [(fid1, fid2) | fid1 <- fids, fid2 <- fids, fid1 /= fid2]
  where
    checkPath :: DependencyInfo S.NameId -> S.NameId -> S.NameId -> Sem r ()
    checkPath deps fid1 fid2 =
      unless (isPath deps fid1 fid2 || isPath deps fid2 fid1) $
        throw (ErrIncomparablePrecedences (IncomaprablePrecedences (findOper fid1) (findOper fid2)))

    findOper :: S.NameId -> S.Name
    findOper fid =
      fromJust $
        find
          (maybe False (\fx -> Just fid == (fx ^. fixityId)) . (^. S.nameFixity))
          opers

checkExpressionPrecedences :: (Members '[Error ScoperError, InfoTableBuilder, Reader InfoTable] r) => ExpressionAtoms 'Scoped -> Sem r ()
checkExpressionPrecedences (ExpressionAtoms atoms _) =
  checkPrecedences opers
  where
    opers :: [S.Name]
    opers = mapMaybe P.getExpressionAtomIden (toList atoms)

checkPatternPrecedences :: (Members '[Error ScoperError, InfoTableBuilder, Reader InfoTable] r) => PatternAtoms 'Scoped -> Sem r ()
checkPatternPrecedences (PatternAtoms atoms _) =
  checkPrecedences opers
  where
    opers :: [S.Name]
    opers = mapMaybe P.getPatternAtomIden (toList atoms)

-------------------------------------------------------------------------------
-- Infix Expression
-------------------------------------------------------------------------------

makeExpressionTable ::
  ExpressionAtoms 'Scoped -> [[P.Operator Parse Expression]]
makeExpressionTable (ExpressionAtoms atoms _) = [recordUpdate] : [appOpExplicit] : operators ++ [[functionOp]]
  where
    operators = mkSymbolTable idens

    idens :: [ScopedIden]
    idens = mapMaybe getIden (toList atoms)
      where
        getIden :: ExpressionAtom 'Scoped -> Maybe ScopedIden
        getIden a = case a of
          AtomIdentifier nm -> Just nm
          _ -> Nothing

    mkSymbolTable :: [ScopedIden] -> [[P.Operator Parse Expression]]
    mkSymbolTable = reverse . map (map snd) . groupSortOn' fst . mapMaybe mkOperator
      where
        mkOperator :: ScopedIden -> Maybe (Precedence, P.Operator Parse Expression)
        mkOperator iden
          | Just Fixity {..} <- _nameFixity =
              case _fixityArity of
                OpUnary u -> Just (_fixityPrecedence, P.Postfix (unaryApp <$> parseSymbolId _nameId))
                  where
                    unaryApp :: ScopedIden -> Expression -> Expression
                    unaryApp funName arg = case u of
                      AssocPostfix -> ExpressionPostfixApplication (PostfixApplication arg funName)
                OpBinary b -> Just (_fixityPrecedence, infixLRN (binaryApp <$> parseSymbolId _nameId))
                  where
                    binaryApp :: ScopedIden -> Expression -> Expression -> Expression
                    binaryApp _infixAppOperator _infixAppLeft _infixAppRight =
                      ExpressionInfixApplication InfixApplication {..}
                    infixLRN :: Parse (Expression -> Expression -> Expression) -> P.Operator Parse Expression
                    infixLRN = case b of
                      AssocLeft -> P.InfixL
                      AssocRight -> P.InfixR
                      AssocNone -> P.InfixN
                OpNone -> Nothing
          | otherwise = Nothing
          where
            S.Name' {..} = iden ^. scopedIdenSrcName

        parseSymbolId :: S.NameId -> Parse ScopedIden
        parseSymbolId uid = P.token getIdentifierWithId mempty
          where
            getIdentifierWithId :: ExpressionAtom 'Scoped -> Maybe ScopedIden
            getIdentifierWithId s = case s of
              AtomIdentifier iden
                | uid == iden ^. scopedIdenSrcName . S.nameId -> Just iden
              _ -> Nothing

    recordUpdate :: P.Operator Parse Expression
    recordUpdate = P.Postfix (mkRecUpdate <$> P.token getRecUpdate mempty)
      where
        getRecUpdate :: ExpressionAtom 'Scoped -> (Maybe (RecordUpdate 'Scoped))
        getRecUpdate = \case
          AtomRecordUpdate u -> Just u
          _ -> Nothing
        mkRecUpdate :: RecordUpdate 'Scoped -> Expression -> Expression
        mkRecUpdate u = ExpressionRecordUpdate . RecordUpdateApp u

    -- Application by juxtaposition.
    appOpExplicit :: P.Operator Parse Expression
    appOpExplicit = P.InfixL (return app)
      where
        app :: Expression -> Expression -> Expression
        app f x =
          ExpressionApplication $
            Application
              { _applicationFunction = f,
                _applicationParameter = x
              }

    -- Non-dependent function type: A → B
    functionOp :: P.Operator Parse Expression
    functionOp = P.InfixR (nonDepFun <$> P.token getArrow mempty)
      where
        getArrow :: ExpressionAtom 'Scoped -> Maybe KeywordRef
        getArrow = \case
          AtomFunArrow r -> return r
          _ -> Nothing

        nonDepFun :: KeywordRef -> Expression -> Expression -> Expression
        nonDepFun _funKw l r =
          ExpressionFunction
            Function
              { _funParameters = params,
                _funReturn = r,
                _funKw
              }
          where
            params =
              let (l', explicitOrInstance, delims') = case l of
                    ExpressionDoubleBraces i ->
                      ( i ^. doubleBracesExpression,
                        ImplicitInstance,
                        Just (i ^. doubleBracesDelims . unIrrelevant)
                      )
                    _ -> (l, Explicit, Nothing)
               in FunctionParameters
                    { _paramNames = [],
                      _paramDelims = Irrelevant delims',
                      _paramColon = Irrelevant Nothing,
                      _paramImplicit = explicitOrInstance,
                      _paramType = l'
                    }

parseExpressionAtoms ::
  forall r.
  (Members '[Error ScoperError, State Scope, InfoTableBuilder, Reader InfoTable] r) =>
  ExpressionAtoms 'Scoped ->
  Sem r Expression
parseExpressionAtoms a@(ExpressionAtoms atoms _) = do
  checkExpressionPrecedences a
  case res of
    Left {} ->
      throw
        ( ErrInfixParser
            InfixError {_infixErrorAtoms = a}
        )
    Right r -> return r
  where
    parser :: Parse Expression
    parser = mkExpressionParser tbl <* P.eof
    res = P.parse parser filePath (toList atoms)
    tbl = makeExpressionTable a
    filePath :: FilePath
    filePath = ""

-- | Monad for parsing expression sections.
type Parse = P.Parsec Void [ExpressionAtom 'Scoped]

mkExpressionParser ::
  [[P.Operator Parse Expression]] ->
  Parse Expression
mkExpressionParser = P.makeExprParser parseTerm

parseTerm :: Parse Expression
parseTerm =
  parseUniverse
    <|> parseNoInfixIdentifier
    <|> parseParens
    <|> parseHole
    <|> parseInstanceHole
    <|> parseFunction
    <|> parseLambda
    <|> parseCase
    <|> parseIf
    <|> parseList
    <|> parseLiteral
    <|> parseLet
    <|> parseDo
    <|> parseIterator
    <|> parseDoubleBraces
    <|> parseBraces
    <|> parseNamedApplicationNew
  where
    parseHole :: Parse Expression
    parseHole = ExpressionHole <$> P.token lit mempty
      where
        lit :: ExpressionAtom 'Scoped -> Maybe Hole
        lit s = case s of
          AtomHole l -> Just l
          _ -> Nothing

    parseInstanceHole :: Parse Expression
    parseInstanceHole = ExpressionInstanceHole <$> P.token lit mempty
      where
        lit :: ExpressionAtom 'Scoped -> Maybe Hole
        lit s = case s of
          AtomInstanceHole l -> Just l
          _ -> Nothing

    parseLiteral :: Parse Expression
    parseLiteral = ExpressionLiteral <$> P.token lit mempty
      where
        lit :: ExpressionAtom 'Scoped -> Maybe LiteralLoc
        lit s = case s of
          AtomLiteral l -> Just l
          _ -> Nothing

    parseLambda :: Parse Expression
    parseLambda = ExpressionLambda <$> P.token lambda mempty
      where
        lambda :: ExpressionAtom 'Scoped -> Maybe (Lambda 'Scoped)
        lambda s = case s of
          AtomLambda l -> Just l
          _ -> Nothing

    parseCase :: Parse Expression
    parseCase = ExpressionCase <$> P.token case_ mempty
      where
        case_ :: ExpressionAtom 'Scoped -> Maybe (Case 'Scoped)
        case_ s = case s of
          AtomCase l -> Just l
          _ -> Nothing

    parseIf :: Parse Expression
    parseIf = ExpressionIf <$> P.token if_ mempty
      where
        if_ :: ExpressionAtom 'Scoped -> Maybe (If 'Scoped)
        if_ s = case s of
          AtomIf l -> Just l
          _ -> Nothing

    parseList :: Parse Expression
    parseList = ExpressionList <$> P.token list_ mempty
      where
        list_ :: ExpressionAtom 'Scoped -> Maybe (List 'Scoped)
        list_ s = case s of
          AtomList l -> Just l
          _ -> Nothing

    parseUniverse :: Parse Expression
    parseUniverse = ExpressionUniverse <$> P.token universe' mempty
      where
        universe' :: ExpressionAtom 'Scoped -> Maybe Universe
        universe' s = case s of
          AtomUniverse u -> Just u
          _ -> Nothing

    parseFunction :: Parse Expression
    parseFunction = ExpressionFunction <$> P.token function mempty
      where
        function :: ExpressionAtom 'Scoped -> Maybe (Function 'Scoped)
        function s = case s of
          AtomFunction u -> Just u
          _ -> Nothing

    parseNamedApplicationNew :: Parse Expression
    parseNamedApplicationNew = ExpressionNamedApplication <$> P.token namedApp mempty
      where
        namedApp :: ExpressionAtom 'Scoped -> Maybe (NamedApplication 'Scoped)
        namedApp s = case s of
          AtomNamedApplication u -> Just u
          _ -> Nothing

    parseLet :: Parse Expression
    parseLet = ExpressionLet <$> P.token letBlock mempty
      where
        letBlock :: ExpressionAtom 'Scoped -> Maybe (Let 'Scoped)
        letBlock s = case s of
          AtomLet u -> Just u
          _ -> Nothing

    parseDo :: Parse Expression
    parseDo = ExpressionDo <$> P.token letBlock mempty
      where
        letBlock :: ExpressionAtom 'Scoped -> Maybe (Do 'Scoped)
        letBlock s = case s of
          AtomDo u -> Just u
          _ -> Nothing

    parseIterator :: Parse Expression
    parseIterator = ExpressionIterator <$> P.token iterator mempty
      where
        iterator :: ExpressionAtom 'Scoped -> Maybe (Iterator 'Scoped)
        iterator s = case s of
          AtomIterator u -> Just u
          _ -> Nothing

    parseNoInfixIdentifier :: Parse Expression
    parseNoInfixIdentifier = ExpressionIdentifier <$> P.token identifierNoFixity mempty
      where
        identifierNoFixity :: ExpressionAtom 'Scoped -> Maybe ScopedIden
        identifierNoFixity s = case s of
          AtomIdentifier iden
            | not (S.hasFixity (iden ^. scopedIdenSrcName)) -> Just iden
          _ -> Nothing

    parseDoubleBraces :: Parse Expression
    parseDoubleBraces = ExpressionDoubleBraces <$> P.token bracedExpr mempty
      where
        bracedExpr :: ExpressionAtom 'Scoped -> Maybe (DoubleBracesExpression 'Scoped)
        bracedExpr = \case
          AtomDoubleBraces l -> Just l
          _ -> Nothing

    parseBraces :: Parse Expression
    parseBraces = ExpressionBraces <$> P.token bracedExpr mempty
      where
        bracedExpr :: ExpressionAtom 'Scoped -> Maybe (WithLoc Expression)
        bracedExpr = \case
          AtomBraces l -> Just l
          _ -> Nothing

    parseParens :: Parse Expression
    parseParens = P.token parenExpr mempty
      where
        parenExpr :: ExpressionAtom 'Scoped -> Maybe Expression
        parenExpr s = case s of
          AtomParens e -> Just e
          _ -> Nothing

-------------------------------------------------------------------------------
-- Infix Patterns
-------------------------------------------------------------------------------

type ParsePat = P.ParsecT Void [PatternAtom 'Scoped] (Sem '[Error ScoperError])

makePatternTable ::
  PatternAtoms 'Scoped -> [[P.Operator ParsePat PatternArg]]
makePatternTable (PatternAtoms latoms _) = [appOp] : operators
  where
    getConstructorRef :: PatternAtom 'Scoped -> Maybe ScopedIden
    getConstructorRef = \case
      PatternAtomIden i -> case i of
        PatternScopedConstructor c -> Just c
        _ -> Nothing
      _ -> Nothing
    operators = mkSymbolTable (mapMaybe getConstructorRef (toList latoms))
    mkSymbolTable :: [ScopedIden] -> [[P.Operator ParsePat PatternArg]]
    mkSymbolTable = reverse . map (map snd) . groupSortOn' fst . mapMaybe unqualifiedSymbolOp
      where
        unqualifiedSymbolOp :: ScopedIden -> Maybe (Precedence, P.Operator ParsePat PatternArg)
        unqualifiedSymbolOp constr = run . runFail $ do
          Fixity {..} <- failMaybe (constr ^. scopedIdenSrcName . S.nameFixity)
          let _nameId = constr ^. scopedIdenSrcName . S.nameId
          case _fixityArity of
            OpUnary u -> return (_fixityPrecedence, P.Postfix (unaryApp <$> parseSymbolId _nameId))
              where
                unaryApp :: ScopedIden -> PatternArg -> PatternArg
                unaryApp constrName = case u of
                  AssocPostfix -> explicitP . PatternPostfixApplication . (`PatternPostfixApp` constrName)
            OpBinary b -> return (_fixityPrecedence, infixLRN (binaryInfixApp <$> parseSymbolId _nameId))
              where
                binaryInfixApp :: ScopedIden -> PatternArg -> PatternArg -> PatternArg
                binaryInfixApp name argLeft = explicitP . PatternInfixApplication . PatternInfixApp argLeft name
                infixLRN :: ParsePat (PatternArg -> PatternArg -> PatternArg) -> P.Operator ParsePat PatternArg
                infixLRN = case b of
                  AssocLeft -> P.InfixL
                  AssocRight -> P.InfixR
                  AssocNone -> P.InfixN
            OpNone -> fail
        parseSymbolId :: S.NameId -> ParsePat ScopedIden
        parseSymbolId uid = P.token getConstructorRefWithId mempty
          where
            getConstructorRefWithId :: PatternAtom 'Scoped -> Maybe ScopedIden
            getConstructorRefWithId s = do
              ref <- getConstructorRef s
              guard (ref ^. scopedIdenSrcName . S.nameId == uid)
              return ref

    -- Application by juxtaposition.
    appOp :: P.Operator ParsePat PatternArg
    appOp = P.InfixL (return app)
      where
        app :: PatternArg -> PatternArg -> PatternArg
        app l r =
          explicitP
            ( PatternApplication
                ( PatternApp
                    { _patAppLeft = l,
                      _patAppRight = r
                    }
                )
            )

explicitP :: Pattern -> PatternArg
explicitP _patternArgPattern =
  PatternArg
    { _patternArgIsImplicit = Explicit,
      _patternArgName = Nothing,
      _patternArgPattern
    }

parsePatternTerm :: ParsePat PatternArg
parsePatternTerm = do
  parseNoInfixConstructor
    <|> parseVariable
    <|> parseDoubleBraces
    <|> parseParens
    <|> parseBraces
    <|> parseWildcard
    <|> parseWildcardConstructor
    <|> parseEmpty
    <|> parseAt
    <|> parseList
    <|> parseRecord
  where
    parseNoInfixConstructor :: ParsePat PatternArg
    parseNoInfixConstructor =
      explicitP
        . PatternConstructor
        <$> P.token constructorNoFixity mempty
      where
        constructorNoFixity :: PatternAtom 'Scoped -> Maybe ScopedIden
        constructorNoFixity s = case s of
          PatternAtomIden (PatternScopedConstructor ref)
            | not (S.hasFixity (n ^. scopedIdenSrcName)) -> Just ref
            where
              n = ref
          _ -> Nothing

    parseWildcard :: ParsePat PatternArg
    parseWildcard = explicitP . PatternWildcard <$> P.token isWildcard mempty
      where
        isWildcard :: PatternAtom 'Scoped -> Maybe Wildcard
        isWildcard s = case s of
          PatternAtomWildcard i -> Just i
          _ -> Nothing

    parseList :: ParsePat PatternArg
    parseList = explicitP . PatternList <$> P.token isList mempty
      where
        isList :: PatternAtom 'Scoped -> Maybe (ListPattern 'Scoped)
        isList s = case s of
          PatternAtomList i -> Just i
          _ -> Nothing

    parseRecord :: ParsePat PatternArg
    parseRecord = explicitP . PatternRecord <$> P.token isRecord mempty
      where
        isRecord :: PatternAtom 'Scoped -> Maybe (RecordPattern 'Scoped)
        isRecord s = case s of
          PatternAtomRecord i -> Just i
          _ -> Nothing

    parseEmpty :: ParsePat PatternArg
    parseEmpty = explicitP . PatternEmpty <$> P.token isEmpty mempty
      where
        isEmpty :: PatternAtom 'Scoped -> Maybe Interval
        isEmpty s = case s of
          PatternAtomEmpty i -> Just i
          _ -> Nothing

    parseAt :: ParsePat PatternArg
    parseAt = do
      res <- P.token isAt mempty
      case res of
        Left e -> P.lift (throw e)
        Right a -> return a
      where
        isAt :: PatternAtom 'Scoped -> Maybe (Either ScoperError PatternArg)
        isAt = \case
          PatternAtomAt p -> Just $ case p ^. patternArgPattern of
            PatternVariable _ -> Left (ErrAliasBinderPattern (AliasBinderPattern p))
            _ -> Right p
          _ -> Nothing

    parseVariable :: ParsePat PatternArg
    parseVariable = explicitP . PatternVariable <$> P.token var mempty
      where
        var :: PatternAtom 'Scoped -> Maybe S.Symbol
        var s = case s of
          PatternAtomIden (PatternScopedVar sym) -> Just sym
          _ -> Nothing

    parseDoubleBraces :: ParsePat PatternArg
    parseDoubleBraces = do
      res <- P.token bracesPat mempty
      either (P.lift . throw) return res
      where
        bracesPat :: PatternAtom 'Scoped -> Maybe (Either ScoperError PatternArg)
        bracesPat = \case
          PatternAtomDoubleBraces r
            | Implicit <- r ^. patternArgIsImplicit ->
                Just (Left (ErrDoubleBracesPattern (DoubleBracesPattern r True)))
            | ImplicitInstance <- r ^. patternArgIsImplicit ->
                Just (Left (ErrDoubleBracesPattern (DoubleBracesPattern r True)))
            | otherwise -> Just (Right (set patternArgIsImplicit ImplicitInstance r))
          _ -> Nothing

    parseBraces :: ParsePat PatternArg
    parseBraces = do
      res <- P.token bracesPat mempty
      case res of
        Left er -> P.lift (throw er)
        Right a -> return a
      where
        bracesPat :: PatternAtom 'Scoped -> Maybe (Either ScoperError PatternArg)
        bracesPat = \case
          PatternAtomBraces r
            | Implicit <- r ^. patternArgIsImplicit ->
                Just (Left (ErrDoubleBracesPattern (DoubleBracesPattern r False)))
            | ImplicitInstance <- r ^. patternArgIsImplicit ->
                Just (Left (ErrDoubleBracesPattern (DoubleBracesPattern r False)))
            | otherwise -> Just (Right (set patternArgIsImplicit Implicit r))
          _ -> Nothing

    parseParens :: ParsePat PatternArg
    parseParens = P.token parenPat mempty
      where
        parenPat :: PatternAtom 'Scoped -> Maybe PatternArg
        parenPat = \case
          PatternAtomParens r -> Just r
          _ -> Nothing

    parseWildcardConstructor :: ParsePat PatternArg
    parseWildcardConstructor = explicitP <$> P.token wildcardConstr mempty
      where
        wildcardConstr :: PatternAtom 'Scoped -> Maybe Pattern
        wildcardConstr = \case
          PatternAtomWildcardConstructor r -> Just (PatternWildcardConstructor r)
          _ -> Nothing

mkPatternParser ::
  [[P.Operator ParsePat PatternArg]] ->
  ParsePat PatternArg
mkPatternParser table = pPattern
  where
    pPattern :: ParsePat PatternArg
    pPattern = P.makeExprParser parsePatternTerm table

parsePatternAtom ::
  (Members '[Error ScoperError, State Scope, InfoTableBuilder, Reader InfoTable] r) =>
  PatternAtom 'Scoped ->
  Sem r PatternArg
parsePatternAtom = parsePatternAtoms . singletonAtom
  where
    singletonAtom :: PatternAtom 'Scoped -> PatternAtoms 'Scoped
    singletonAtom a = PatternAtoms (NonEmpty.singleton a) (Irrelevant (getLoc a))

parsePatternAtoms ::
  (Members '[Error ScoperError, State Scope, InfoTableBuilder, Reader InfoTable] r) =>
  PatternAtoms 'Scoped ->
  Sem r PatternArg
parsePatternAtoms atoms = do
  checkPatternPrecedences atoms
  case run (runError res) of
    Left e -> throw e -- Scoper effect error
    Right Left {} -> throw (ErrInfixPattern (InfixErrorP atoms)) -- Megaparsec error
    Right (Right r) -> return r
  where
    sec = toList (atoms ^. patternAtoms)
    tbl = makePatternTable atoms
    parser :: ParsePat PatternArg
    parser = mkPatternParser tbl <* P.eof
    res = P.runParserT parser filePath sec

    filePath :: FilePath
    filePath = "tmp"
