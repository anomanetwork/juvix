module Juvix.Compiler.Core.Extra.Utils
  ( module Juvix.Compiler.Core.Extra.Utils,
    module Juvix.Compiler.Core.Extra.Base,
    module Juvix.Compiler.Core.Extra.Recursors,
    module Juvix.Compiler.Core.Extra.Info,
    module Juvix.Compiler.Core.Extra.Equality,
    module Juvix.Compiler.Core.Extra.Recursors.Fold.Named,
    module Juvix.Compiler.Core.Extra.Recursors.Map.Named,
    module Juvix.Compiler.Core.Extra.Recursors.RMap.Named,
    module Juvix.Compiler.Core.Extra.Recursors.Utils,
    module Juvix.Compiler.Core.Extra.Utils.Base,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.Set qualified as Set
import Juvix.Compiler.Core.Data.BinderList qualified as BL
import Juvix.Compiler.Core.Data.InfoTable
import Juvix.Compiler.Core.Data.Module
import Juvix.Compiler.Core.Extra.Base
import Juvix.Compiler.Core.Extra.Equality
import Juvix.Compiler.Core.Extra.Info
import Juvix.Compiler.Core.Extra.Recursors
import Juvix.Compiler.Core.Extra.Recursors.Fold.Named
import Juvix.Compiler.Core.Extra.Recursors.Map.Named
import Juvix.Compiler.Core.Extra.Recursors.RMap.Named
import Juvix.Compiler.Core.Extra.Recursors.Utils
import Juvix.Compiler.Core.Extra.Utils.Base
import Juvix.Compiler.Core.Info qualified as Info
import Juvix.Compiler.Core.Info.ExpansionInfo
import Juvix.Compiler.Core.Info.LocationInfo qualified as Info

substEnvInBranch :: Env -> CaseBranch -> CaseBranch
substEnvInBranch env br = over caseBranchBody (substEnv env') br
  where
    env' = map mkVar' [0 .. br ^. caseBranchBindersNum - 1] ++ env

isClosed :: Node -> Bool
isClosed = not . has freeVars

mkAxiom :: Interval -> Type -> Node
mkAxiom loc = mkBottom (Info.setInfoLocation loc mempty)

isTypeConstr :: Module -> Type -> Bool
isTypeConstr md ty = case typeTarget ty of
  NUniv {} ->
    True
  NIdt Ident {..} ->
    isTypeConstr md (lookupIdentifierNode md _identSymbol)
  _ -> False

getTypeParams :: Module -> Type -> [Type]
getTypeParams md ty = filter (isTypeConstr md) (typeArgs ty)

getTypeParamsNum :: Module -> Type -> Int
getTypeParamsNum md ty = length $ getTypeParams md ty

filterOutTypeSynonyms :: Module -> Module
filterOutTypeSynonyms md = pruneInfoTable md'
  where
    md' = set (moduleInfoTable . infoIdentifiers) idents' md
    idents' = HashMap.filter (\ii -> not (isTypeConstr md (ii ^. identifierType))) (md ^. moduleInfoTable . infoIdentifiers)

isType' :: Node -> Bool
isType' = \case
  NPi {} -> True
  NUniv {} -> True
  NPrim {} -> True
  NTyp {} -> True
  NDyn {} -> True
  NBot {} -> False
  NVar {} -> False
  NIdt {} -> False
  NCst {} -> False
  NApp {} -> False
  NBlt {} -> False
  NCtr {} -> False
  NLam {} -> False
  NLet {} -> False
  NRec {} -> False
  NCase {} -> False
  NMatch {} -> False
  Closure {} -> False

isType :: Module -> BinderList Binder -> Node -> Bool
isType md bl node = case node of
  NVar Var {..}
    | Just Binder {..} <- BL.lookupMay _varIndex bl ->
        isTypeConstr md _binderType
  NIdt Ident {..}
    | Just ii <- lookupIdentifierInfo' md _identSymbol ->
        isTypeConstr md (ii ^. identifierType)
  _ -> isType' node

isZeroOrderType' :: HashSet Symbol -> Module -> Type -> Bool
isZeroOrderType' foinds md = \case
  NPi {} -> False
  NDyn {} -> False
  NTyp TypeConstr {..} ->
    isFirstOrderInductive' foinds md _typeConstrSymbol
      && all (isZeroOrderType' foinds md) _typeConstrArgs
  ty -> isType' ty

isFirstOrderType' :: HashSet Symbol -> Module -> Type -> Bool
isFirstOrderType' foinds md ty = case ty of
  NVar {} -> True
  NPi Pi {..} ->
    isZeroOrderType' foinds md (_piBinder ^. binderType)
      && isFirstOrderType' foinds md _piBody
  NUniv {} -> True
  NPrim {} -> True
  NTyp {} -> isZeroOrderType' foinds md ty
  NDyn {} -> False
  _ -> assert (not (isType' ty)) False

isFirstOrderInductive' :: HashSet Symbol -> Module -> Symbol -> Bool
isFirstOrderInductive' foinds md sym
  | HashSet.member sym foinds = True
  | otherwise = case lookupInductiveInfo' md sym of
      Nothing -> False
      Just ii ->
        all
          (isFirstOrderType' (HashSet.insert sym foinds) md . (^. constructorType) . lookupConstructorInfo md)
          (ii ^. inductiveConstructors)

isFirstOrderType :: Module -> Type -> Bool
isFirstOrderType = isFirstOrderType' mempty

isZeroOrderType :: Module -> Type -> Bool
isZeroOrderType = isZeroOrderType' mempty

-- | True for nodes whose evaluation immediately returns a value, i.e.,
-- no reduction or memory allocation in the runtime is required.
isImmediate :: Module -> Node -> Bool
isImmediate md = \case
  NVar {} -> True
  NIdt {} -> True
  NCst {} -> True
  NCtr Constr {..}
    | Just ci <- lookupConstructorInfo' md _constrTag ->
        let paramsNum = length (takeWhile (isTypeConstr md) (typeArgs (ci ^. constructorType)))
         in length _constrArgs <= paramsNum
    | otherwise -> all (isType md mempty) _constrArgs
  node@(NApp {}) ->
    let (h, args) = unfoldApps' node
     in case h of
          NIdt Ident {..}
            | Just ii <- lookupIdentifierInfo' md _identSymbol ->
                let paramsNum = length (takeWhile (isTypeConstr md) (typeArgs (ii ^. identifierType)))
                 in length args <= paramsNum
          _ -> all (isType md mempty) args
  node -> isType md mempty node

isImmediate' :: Node -> Bool
isImmediate' = isImmediate (emptyModule defaultModuleId)

isImmediateOrLambda :: Module -> Node -> Bool
isImmediateOrLambda md node = isImmediate md node || isLambda node

-- | True if the argument is fully evaluated first-order data
isDataValue :: Node -> Bool
isDataValue = \case
  NCst {} -> True
  NCtr Constr {..} -> all isDataValue _constrArgs
  _ -> False

isFullyApplied :: Module -> BinderList Binder -> Node -> Bool
isFullyApplied md bl node = case h of
  NIdt Ident {..}
    | Just ii <- lookupIdentifierInfo' md _identSymbol ->
        length args == ii ^. identifierArgsNum
  NVar Var {..} ->
    case BL.lookupMay _varIndex bl of
      Just Binder {..} ->
        length args == length (typeArgs _binderType)
      Nothing ->
        False
  _ ->
    False
  where
    (h, args) = unfoldApps' node

isFailNode :: Node -> Bool
isFailNode = \case
  NBlt (BuiltinApp {..}) | _builtinAppOp == OpFail -> True
  _ -> False

isLambda :: Node -> Bool
isLambda = \case
  NLam {} -> True
  _ -> False

isTrueConstr :: Node -> Bool
isTrueConstr = \case
  NCtr Constr {..} | _constrTag == BuiltinTag TagTrue -> True
  _ -> False

isFalseConstr :: Node -> Bool
isFalseConstr = \case
  NCtr Constr {..} | _constrTag == BuiltinTag TagFalse -> True
  _ -> False

isDebugOp :: Node -> Bool
isDebugOp = \case
  NBlt BuiltinApp {..} ->
    case _builtinAppOp of
      OpTrace -> True
      OpFail -> True
      OpSeq -> True
      OpAssert -> True
      OpAnomaSha256 -> False
      OpAnomaByteArrayFromAnomaContents -> False
      OpAnomaByteArrayToAnomaContents -> False
      OpAnomaDecode -> False
      OpAnomaEncode -> False
      OpAnomaGet -> False
      OpAnomaSign -> False
      OpAnomaSignDetached -> False
      OpAnomaVerifyDetached -> False
      OpAnomaVerifyWithMessage -> False
      OpAnomaResourceCommitment -> False
      OpAnomaResourceNullifier -> False
      OpAnomaResourceKind -> False
      OpAnomaResourceDelta -> False
      OpAnomaActionDelta -> False
      OpAnomaActionsDelta -> False
      OpAnomaProveAction -> False
      OpAnomaProveDelta -> False
      OpAnomaZeroDelta -> False
      OpAnomaAddDelta -> False
      OpAnomaSubDelta -> False
      OpAnomaRandomGeneratorInit -> False
      OpAnomaRandomNextBytes -> False
      OpAnomaRandomSplit -> False
      OpAnomaIsCommitment -> False
      OpAnomaIsNullifier -> False
      OpAnomaSetToList -> False
      OpAnomaSetFromList -> False
      OpEc -> False
      OpFieldAdd -> False
      OpFieldDiv -> False
      OpFieldFromInt -> False
      OpFieldMul -> False
      OpFieldSub -> False
      OpPoseidonHash -> False
      OpRandomEcPoint -> False
      OpStrConcat -> False
      OpStrToInt -> False
      OpUInt8FromInt -> False
      OpUInt8ToInt -> False
      OpByteArrayFromListByte -> False
      OpByteArrayLength -> False
      OpEq -> False
      OpIntAdd -> False
      OpIntDiv -> False
      OpIntLe -> False
      OpIntLt -> False
      OpIntMod -> False
      OpIntMul -> False
      OpIntSub -> False
      OpFieldToInt -> False
      OpShow -> False
  _ -> False

-- | Check if the node contains `trace`, `fail` or `seq` (`>->`).
containsDebugOps :: Node -> Bool
containsDebugOps = ufold (\x xs -> x || or xs) isDebugOp

freeVarsSortedMany :: [Node] -> Set Var
freeVarsSortedMany n = Set.fromList (n ^.. each . freeVars)

freeVarsSorted :: Node -> Set Var
freeVarsSorted = freeVarsSortedMany . pure

freeVarsSet :: Node -> HashSet Var
freeVarsSet n = HashSet.fromList (n ^.. freeVars)

getIdents :: Node -> HashSet Ident
getIdents n = HashSet.fromList (n ^.. nodeIdents)

nodeIdents :: Traversal' Node Ident
nodeIdents f = ufoldA reassemble go
  where
    go = \case
      NIdt i -> NIdt <$> f i
      n -> pure n

getInductives :: Node -> HashSet Symbol
getInductives n = HashSet.fromList (n ^.. nodeInductives)

nodeInductives :: Traversal' Node Symbol
nodeInductives f = ufoldA reassemble go
  where
    go = \case
      NTyp ty -> NTyp <$> traverseOf typeConstrSymbol f ty
      n -> pure n

getSymbols :: Module -> Node -> HashSet Symbol
getSymbols md = gather go mempty
  where
    go :: HashSet Symbol -> Node -> HashSet Symbol
    go acc = \case
      NTyp TypeConstr {..} -> HashSet.insert _typeConstrSymbol acc
      NIdt Ident {..} -> HashSet.insert _identSymbol acc
      NCase Case {..} -> HashSet.insert _caseInductive acc
      NCtr Constr {..}
        | Just ci <- lookupConstructorInfo' md _constrTag ->
            HashSet.insert (ci ^. constructorInductive) acc
      _ -> acc

getSymbols' :: InfoTable -> Node -> HashSet Symbol
getSymbols' tab = getSymbols (emptyModule defaultModuleId) {_moduleInfoTable = tab}

-- | Prism for NRec
_NRec :: SimpleFold Node LetRec
_NRec f = \case
  NRec l -> NRec <$> f l
  n -> pure n

-- | Prism for NLam
_NLam :: SimpleFold Node Lambda
_NLam f = \case
  NLam l -> NLam <$> f l
  n -> pure n

-- | Fold over all of the transitive descendants of a Node, including itself.
cosmos :: SimpleFold Node Node
cosmos f = ufoldA reassemble f

-- | The free vars are given in the context of the node.
captureFreeVarsType :: [(Index, Binder)] -> (Node, Type) -> (Node, Type)
captureFreeVarsType freevars (n, ty) =
  let bodyTy = mapFreeVars ty
      body' = mapFreeVars n
   in ( mkLambdasB captureBinders' body',
        mkPis captureBinders' bodyTy
      )
  where
    mapFreeVars :: Node -> Node
    mapFreeVars = dmapN go
      where
        s :: HashMap Index Index
        s = HashMap.fromList (zip (reverse (map fst freevars)) [0 ..])
        go :: Index -> Node -> Node
        go k = \case
          NVar (Var i u)
            | Just v <- s ^. at (u - k) -> NVar (Var i (v + k))
          m -> m
    captureBinders' :: [Binder]
    captureBinders' = goBinders freevars []
      where
        goBinders :: [(Index, Binder)] -> [Binder] -> [Binder]
        goBinders fv acc = case unsnoc fv of
          Nothing -> acc
          Just (fvs, (idx, bin)) -> goBinders fvs (mapBinder idx bin : acc)
          where
            indices = map fst fv
            mapBinder :: Index -> Binder -> Binder
            mapBinder binderIndex = over binderType (dmapN go)
              where
                go :: Index -> Node -> Node
                go k = \case
                  NVar u
                    | u ^. varIndex >= k ->
                        let uCtx = u ^. varIndex - k + binderIndex + 1
                            err = error ("impossible: could not find " <> show uCtx <> " in " <> show indices)
                            u' = length indices - 2 - fromMaybe err (elemIndex uCtx indices) + k
                         in NVar (set varIndex u' u)
                  m -> m

-- | The list should not contain repeated indices.
-- if fv = x1, x2, .., xn
-- the result is of the form λx1 λx2 .. λ xn b
captureFreeVars :: [(Index, Binder)] -> Node -> Node
captureFreeVars freevars n = fst (captureFreeVarsType freevars (n, mkDynamic'))

-- | Captures all free variables of a node. It also returns the list of captured
-- variables in left-to-right order: if snd is of the form λxλy... then fst is
-- [x, y]
captureFreeVarsCtx :: BinderList Binder -> Node -> ([(Var, Binder)], Node)
captureFreeVarsCtx bl n =
  let assocs = freeVarsCtx bl n
   in (assocs, captureFreeVars (map (first (^. varIndex)) assocs) n)

captureFreeVarsCtxType :: BinderList Binder -> (Node, Type) -> ([(Var, Binder)], (Node, Type))
captureFreeVarsCtxType bl (n, ty) =
  let assocs = freeVarsCtx bl n
      assocsi = map (first (^. varIndex)) assocs
   in (assocs, captureFreeVarsType assocsi (n, ty))

freeVarsCtxMany' :: BinderList Binder -> [Node] -> [Var]
freeVarsCtxMany' bl = map fst . freeVarsCtxMany bl

freeVarsCtx :: BinderList Binder -> Node -> [(Var, Binder)]
freeVarsCtx ctx = freeVarsCtxMany ctx . pure

-- | The output list does not contain repeated elements and is sorted by
-- *decreasing* variable index. The indices are relative to the given binder
-- list
freeVarsCtxMany :: BinderList Binder -> [Node] -> [(Var, Binder)]
freeVarsCtxMany ctx =
  BL.lookupsSortedRev ctx . run . fmap fst . runOutputList . go . freeVarsSortedMany
  where
    go ::
      -- set of free variables relative to the original ctx
      Set Var ->
      Sem '[Output Var] ()
    go fv = case Set.minView fv of
      Nothing -> return ()
      Just (v, vs) -> do
        output v
        let idx = v ^. varIndex
            bi :: Binder = BL.lookup idx ctx
            fbi = freeVarsSorted (bi ^. binderType)
            freevarsbi' :: Set Var
            freevarsbi' = Set.mapMonotonic (over varIndex (+ (idx + 1))) fbi
        go (freevarsbi' <> vs)

-- | Increase the indices of free variables in the binderType by a given value
shiftBinder :: Int -> Binder -> Binder
shiftBinder = over binderType . shift

-- | Increase the indices of free variables in the item binder and value
shiftLetItem :: Int -> LetItem -> LetItem
shiftLetItem n l =
  LetItem
    { _letItemBinder = shiftBinder n (l ^. letItemBinder),
      _letItemValue = shift n (l ^. letItemValue)
    }

-- | subst for multiple bindings; the first element in the list of substitutions
-- corresponds to de Bruijn index 0
substs :: [Node] -> Node -> Node
substs t = umapN go
  where
    len = length t
    go k n = case n of
      NVar (Var i idx)
        | idx >= k, idx - k < len -> shift k (t !! (idx - k))
        | idx > k -> mkVar i (idx - len)
      _ -> n

-- | substitute a term t for the free variable with de Bruijn index 0, avoiding
-- variable capture; shifts all free variables with de Bruijn index > 0 by -1 (as
-- if the topmost binder was removed)
subst :: Node -> Node -> Node
subst t = substs [t]

-- | substitute a term t for the free variable with de Bruijn index idx, avoiding
-- variable capture; shifts all free variables with de Bruijn index > idx by -1 (as
-- if the binder idx was removed)
substVar :: Index -> Node -> Node -> Node
substVar idx t = umapN go
  where
    go k n = case n of
      NVar Var {..}
        | _varIndex == k + idx -> shift k t
        | _varIndex > k + idx -> mkVar _varInfo (_varIndex - 1)
      _ -> n

removeClosures :: Node -> Node
removeClosures = dmap go
  where
    go :: Node -> Node
    go = \case
      Closure {..} -> substEnv _closureEnv _closureNode
      node -> node

etaExpand :: [Type] -> Node -> Node
etaExpand [] n = n
etaExpand argtys n =
  mkLambdas (replicate (length argtys) (Info.singleton (ExpansionInfo ()))) (map mkBinder' argtys) (mkApps' (shift k n) (map mkVar' (reverse [0 .. k - 1])))
  where
    k = length argtys

squashApps :: Node -> Node
squashApps = dmap go
  where
    go :: Node -> Node
    go n =
      let (l, args) = unfoldApps' n
       in case l of
            NCtr (Constr i tag args') -> mkConstr i tag (args' ++ args)
            NBlt (BuiltinApp i op args') -> mkBuiltinApp i op (args' ++ args)
            NTyp (TypeConstr i sym args') -> mkTypeConstr i sym (args' ++ args)
            _ -> n

patternType :: Pattern -> Node
patternType = \case
  PatWildcard w -> w ^. patternWildcardBinder . binderType
  PatConstr c -> c ^. patternConstrBinder . binderType

builtinOpArgTypes :: BuiltinOp -> [Type]
builtinOpArgTypes = \case
  OpIntAdd -> [mkTypeInteger', mkTypeInteger']
  OpIntSub -> [mkTypeInteger', mkTypeInteger']
  OpIntMul -> [mkTypeInteger', mkTypeInteger']
  OpIntDiv -> [mkTypeInteger', mkTypeInteger']
  OpIntMod -> [mkTypeInteger', mkTypeInteger']
  OpIntLt -> [mkTypeInteger', mkTypeInteger']
  OpIntLe -> [mkTypeInteger', mkTypeInteger']
  OpFieldAdd -> [mkTypeField', mkTypeField']
  OpFieldSub -> [mkTypeField', mkTypeField']
  OpFieldMul -> [mkTypeField', mkTypeField']
  OpFieldDiv -> [mkTypeField', mkTypeField']
  OpFieldFromInt -> [mkTypeInteger']
  OpFieldToInt -> [mkTypeField']
  OpEq -> [mkDynamic', mkDynamic']
  OpShow -> [mkDynamic']
  OpStrConcat -> [mkTypeString', mkTypeString']
  OpStrToInt -> [mkTypeString']
  OpAssert -> [mkTypeBool']
  OpSeq -> [mkDynamic', mkDynamic']
  OpTrace -> [mkDynamic']
  OpFail -> [mkTypeString']
  OpAnomaGet -> [mkDynamic']
  OpAnomaEncode -> [mkDynamic']
  OpAnomaDecode -> [mkDynamic']
  OpAnomaVerifyDetached -> [mkDynamic', mkDynamic', mkDynamic']
  OpAnomaSign -> [mkDynamic', mkDynamic']
  OpAnomaSignDetached -> [mkDynamic', mkDynamic']
  OpAnomaVerifyWithMessage -> [mkDynamic', mkDynamic']
  OpAnomaByteArrayToAnomaContents -> [mkDynamic']
  OpAnomaByteArrayFromAnomaContents -> [mkTypeInteger', mkTypeInteger']
  OpAnomaSha256 -> [mkTypeInteger']
  OpAnomaResourceCommitment -> [mkDynamic']
  OpAnomaResourceNullifier -> [mkDynamic']
  OpAnomaResourceKind -> [mkDynamic']
  OpAnomaResourceDelta -> [mkDynamic']
  OpAnomaActionDelta -> [mkDynamic']
  OpAnomaActionsDelta -> [mkDynamic']
  OpAnomaProveAction -> [mkDynamic']
  OpAnomaProveDelta -> [mkDynamic']
  OpAnomaZeroDelta -> []
  OpAnomaAddDelta -> [mkDynamic', mkDynamic']
  OpAnomaSubDelta -> [mkDynamic', mkDynamic']
  OpAnomaRandomGeneratorInit -> [mkTypeInteger']
  OpAnomaRandomNextBytes -> [mkTypeInteger', mkTypeRandomGenerator']
  OpAnomaRandomSplit -> [mkTypeRandomGenerator']
  OpAnomaIsCommitment -> [mkTypeInteger']
  OpAnomaIsNullifier -> [mkTypeInteger']
  OpAnomaSetToList -> [mkDynamic']
  OpAnomaSetFromList -> [mkDynamic']
  OpPoseidonHash -> [mkDynamic']
  OpEc -> [mkDynamic', mkTypeField', mkDynamic']
  OpRandomEcPoint -> []
  OpUInt8ToInt -> [mkTypeUInt8']
  OpUInt8FromInt -> [mkTypeInteger']
  OpByteArrayFromListByte -> [mkDynamic']
  OpByteArrayLength -> [mkTypeByteArray']

translateCase :: (Node -> Node -> Node -> a) -> a -> Case -> a
translateCase translateIfFun dflt Case {..} = case _caseBranches of
  [br@CaseBranch {..}]
    | _caseBranchTag == BuiltinTag TagTrue ->
        translateIfFun _caseValue (br ^. caseBranchBody) (fromMaybe branchFailure _caseDefault)
  [br@CaseBranch {..}]
    | _caseBranchTag == BuiltinTag TagFalse ->
        translateIfFun _caseValue (fromMaybe branchFailure _caseDefault) (br ^. caseBranchBody)
  [br1, br2]
    | br1 ^. caseBranchTag == BuiltinTag TagTrue
        && br2 ^. caseBranchTag == BuiltinTag TagFalse ->
        translateIfFun _caseValue (br1 ^. caseBranchBody) (br2 ^. caseBranchBody)
    | br1 ^. caseBranchTag == BuiltinTag TagFalse
        && br2 ^. caseBranchTag == BuiltinTag TagTrue ->
        translateIfFun _caseValue (br2 ^. caseBranchBody) (br1 ^. caseBranchBody)
  _ ->
    dflt
  where
    branchFailure :: Node
    branchFailure = mkBuiltinApp' OpFail [mkConstant' (ConstString "illegal `if` branch")]

translateCaseIf :: (Node -> Node -> Node -> a) -> Case -> a
translateCaseIf f = translateCase f impossible

checkDepth :: Module -> BinderList Binder -> Int -> Node -> Bool
checkDepth md bl d node
  | d == 0 = isType md bl node
  | otherwise = case node of
      NApp App {..} ->
        checkDepth md bl d _appLeft && checkDepth md bl (d - 1) _appRight
      _ ->
        all go (children node)
        where
          go :: NodeChild -> Bool
          go NodeChild {..} =
            checkDepth md (BL.prependRev _childBinders bl) (d - 1) _childNode

isCaseBoolean :: [CaseBranch] -> Bool
isCaseBoolean = \case
  [CaseBranch {..}]
    | _caseBranchTag == BuiltinTag TagTrue -> True
  [CaseBranch {..}]
    | _caseBranchTag == BuiltinTag TagFalse -> True
  [br1, br2]
    | br1 ^. caseBranchTag == BuiltinTag TagTrue
        && br2 ^. caseBranchTag == BuiltinTag TagFalse ->
        True
    | br1 ^. caseBranchTag == BuiltinTag TagFalse
        && br2 ^. caseBranchTag == BuiltinTag TagTrue ->
        True
  _ ->
    False

checkInfoTable :: InfoTable -> Bool
checkInfoTable tab =
  all isClosed (tab ^. identContext)
    && all (isClosed . (^. identifierType)) (tab ^. infoIdentifiers)
    && all (isClosed . (^. constructorType)) (tab ^. infoConstructors)

-- | Checks if the `n`th argument (zero-based) is passed without modification to
-- direct recursive calls.
isArgRecursiveInvariant :: Module -> Symbol -> Int -> Bool
isArgRecursiveInvariant md sym argNum = run $ execState True $ dmapNRM go body
  where
    nodeSym = lookupIdentifierNode md sym
    (lams, body) = unfoldLambdas nodeSym
    n = length lams

    go :: (Member (State Bool) r) => Level -> Node -> Sem r Recur
    go lvl node = case node of
      NApp {} ->
        let (h, args) = unfoldApps' node
         in case h of
              NIdt Ident {..}
                | _identSymbol == sym ->
                    let b =
                          argNum < length args
                            && case args !! argNum of
                              NVar Var {..} | _varIndex == lvl + n - argNum - 1 -> True
                              _ -> False
                     in do
                          modify' (&& b)
                          mapM_ (dmapNRM' (lvl, go)) args
                          return $ End node
              _ -> return $ Recur node
      NIdt Ident {..}
        | _identSymbol == sym -> do
            put False
            return $ End node
      _ -> return $ Recur node

isDirectlyRecursive :: Module -> Symbol -> Bool
isDirectlyRecursive md sym = ufold (\x xs -> or (x : xs)) go (lookupIdentifierNode md sym)
  where
    go :: Node -> Bool
    go = \case
      NIdt Ident {..} -> _identSymbol == sym
      _ -> False

-- Returns a map from symbols to their number of occurrences in the given node.
getSymbolsMap :: Module -> Node -> HashMap Symbol Int
getSymbolsMap md = gather go mempty
  where
    go :: HashMap Symbol Int -> Node -> HashMap Symbol Int
    go acc = \case
      NTyp TypeConstr {..} -> mapInc _typeConstrSymbol acc
      NIdt Ident {..} -> mapInc _identSymbol acc
      NCase Case {..} -> mapInc _caseInductive acc
      NCtr Constr {..}
        | Just ci <- lookupConstructorInfo' md _constrTag ->
            mapInc (ci ^. constructorInductive) acc
      _ -> acc

    mapInc :: Symbol -> HashMap Symbol Int -> HashMap Symbol Int
    mapInc k = HashMap.insertWith (+) k 1

getTableSymbolsMap :: InfoTable -> HashMap Symbol Int
getTableSymbolsMap tab =
  foldr
    (HashMap.unionWith (+))
    mempty
    (map (getSymbolsMap md) (HashMap.elems $ tab ^. identContext))
  where
    md = (emptyModule defaultModuleId) {_moduleInfoTable = tab}

getModuleSymbolsMap :: Module -> HashMap Symbol Int
getModuleSymbolsMap = getTableSymbolsMap . computeCombinedInfoTable
