module Juvix.Compiler.Nockma.Evaluator
  ( module Juvix.Compiler.Nockma.Evaluator,
    module Juvix.Compiler.Nockma.Evaluator.Error,
    module Juvix.Compiler.Nockma.Evaluator.Options,
    module Juvix.Compiler.Nockma.Evaluator.Storage,
  )
where

import Crypto.Sign.Ed25519
import Data.ByteString qualified as BS
import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Nockma.Encoding
import Juvix.Compiler.Nockma.Encoding qualified as Encoding
import Juvix.Compiler.Nockma.Encoding.Ed25519
import Juvix.Compiler.Nockma.Evaluator.Error
import Juvix.Compiler.Nockma.Evaluator.Options
import Juvix.Compiler.Nockma.Evaluator.Storage
import Juvix.Compiler.Nockma.Language
import Juvix.Compiler.Nockma.Pretty
import Juvix.Prelude hiding (Atom, Path)
import System.Random qualified as R
import System.Random.SplitMix qualified as R

newtype OpCounts = OpCounts
  { _opCountsMap :: HashMap NockOp Int
  }

makeLenses ''OpCounts

initOpCounts :: OpCounts
initOpCounts = OpCounts mempty

ignoreOpCounts :: Sem (State OpCounts ': r) a -> Sem r a
ignoreOpCounts = evalState initOpCounts

countOp :: (Members '[State OpCounts] r) => NockOp -> Sem r ()
countOp op =
  modify
    ( over
        (opCountsMap . at op)
        ( \case
            Nothing -> Just 1
            Just n -> Just (n + 1)
        )
    )

runOpCounts :: Sem (State OpCounts ': r) a -> Sem r (OpCounts, a)
runOpCounts = runState initOpCounts

instance Pretty OpCounts where
  pretty :: OpCounts -> Doc a
  pretty (OpCounts m) = vsepHard [pretty op <+> ":" <+> pretty (fromMaybe 0 (m ^. at op)) | op <- allElements]

asAtom :: (Members '[Reader EvalCtx, Error (NockEvalError a)] r) => Term a -> Sem r (Atom a)
asAtom = \case
  TermAtom a -> return a
  TermCell c -> throwExpectedAtom c

asCell :: (Members '[Reader EvalCtx, Error (NockEvalError a)] r) => Term a -> Sem r (Cell a)
asCell = \case
  TermAtom a -> throwExpectedCell a
  TermCell c -> return c

asBool :: (Members '[Reader EvalCtx, Error (NockEvalError a)] r, NockNatural a) => Term a -> Sem r Bool
asBool t = do
  a <- asAtom t
  return (nockmaEq a nockTrue)

asPath ::
  (Members '[Reader EvalCtx, Error (NockEvalError a), Error (ErrNockNatural a)] r, NockNatural a) =>
  Term a ->
  Sem r Path
asPath = asAtom >=> nockPath

subTermT' :: Path -> Traversal (Term a) (Term a) (First (Term a)) (Term a)
subTermT' pos f = subTermT pos (f . First . Just)

subTermT :: Path -> Traversal' (Term a) (Term a)
subTermT = go
  where
    go :: Path -> (forall f. (Applicative f) => (Term a -> f (Term a)) -> Term a -> f (Term a))
    go = \case
      [] -> id
      d : ds -> \g t -> case t of
        TermAtom {} -> pure t
        TermCell c -> case d of
          L -> (\l' -> TermCell (set cellLeft l' c)) <$> go ds g (c ^. cellLeft)
          R -> (\r' -> TermCell (set cellRight r' c)) <$> go ds g (c ^. cellRight)

subTerm :: (Members '[Reader EvalCtx, Error (NockEvalError a)] r) => Term a -> Path -> Maybe Interval -> Sem r (Term a)
subTerm term pos posLoc =
  case term ^? subTermT pos of
    Nothing -> throwInvalidPath posLoc term pos
    Just t -> return t

setSubTerm :: forall a r. (Members '[Error (NockEvalError a), Reader EvalCtx] r) => Term a -> Path -> Term a -> Sem r (Term a)
setSubTerm term pos repTerm =
  let (old, new) = setAndRemember (subTermT' pos) repTerm term
   in if
          | isNothing (getFirst old) -> throwInvalidPath Nothing term pos
          | otherwise -> return new

parseCell ::
  forall r a.
  (Members '[Reader EvalCtx, Error (NockEvalError a), Error (ErrNockNatural a)] r, NockNatural a) =>
  Cell a ->
  Sem r (ParsedCell a)
parseCell c = case c ^. cellLeft of
  TermAtom a -> operatorOrAnomaLibCall a (c ^. cellRight) (c ^. cellCall)
  TermCell l -> return (ParsedAutoConsCell (AutoConsCell l (c ^. cellRight)))
  where
    operatorOrAnomaLibCall :: Atom a -> Term a -> Maybe (AnomaLibCall a) -> Sem r (ParsedCell a)
    operatorOrAnomaLibCall a t mcall = do
      opCell <- parseOperatorCell a t
      return $ case mcall of
        Nothing -> ParsedOperatorCell opCell
        Just call -> ParsedAnomaLibCallCell (parseAnomaLibCall opCell call)

    parseAnomaLibCall :: OperatorCell a -> AnomaLibCall a -> AnomaLibCallCell a
    parseAnomaLibCall op call = AnomaLibCallCell call op

    parseOperatorCell :: Atom a -> Term a -> Sem r (OperatorCell a)
    parseOperatorCell a t = do
      op <- catch @(ErrNockNatural a) (nockOp a) $ \e ->
        let atm :: Atom a = errGetAtom e
         in throwInvalidNockOp atm
      return
        OperatorCell
          { _operatorCellOp = op,
            _operatorCellTag = c ^. cellTag,
            _operatorCellTerm = t
          }

fromReplTerm :: forall a r. (Members '[Error (NockEvalError a)] r) => HashMap Text (Term a) -> ReplTerm a -> Sem r (Term a)
fromReplTerm namedTerms = \case
  ReplName n -> maybe (throw @(NockEvalError a) (ErrAssignmentNotFound n)) return (namedTerms ^. at n)
  ReplTerm t -> return t

programAssignments :: Maybe (Program a) -> HashMap Text (Term a)
programAssignments mprog =
  hashMap
    [ (as ^. assignmentName, as ^. assignmentBody)
      | StatementAssignment as <- mprog ^. _Just . programStatements
    ]

-- | The stack provided in the replExpression has priority
evalRepl ::
  forall r a.
  (Hashable a, Integral a, Members '[Reader EvalOptions, Error (NockEvalError a), Error (ErrNockNatural a)] r, NockNatural a) =>
  (Term a -> Sem r ()) ->
  Maybe (Program a) ->
  Maybe (Term a) ->
  ReplExpression a ->
  Sem r (Term a)
evalRepl handleTrace mprog defaultStack expr = do
  (mstack, t) <- case expr of
    ReplExpressionTerm tm -> return (defaultStack, tm)
    ReplExpressionWithStack w -> do
      t' <- fromReplTerm namedTerms (w ^. withStackStack)
      return (Just t', w ^. withStackTerm)
  stack <- maybe errNoStack return mstack
  fromReplTerm namedTerms t >>= runOutputSem @(Term a) handleTrace . runReader @(Storage a) emptyStorage . eval stack
  where
    errNoStack :: Sem r x
    errNoStack = throw @(NockEvalError a) (ErrNoStack NoStack)

    namedTerms :: HashMap Text (Term a)
    namedTerms = programAssignments mprog

eval ::
  forall s a.
  (Hashable a, Integral a, Members '[Reader (Storage a), Reader EvalOptions, Output (Term a), Error (NockEvalError a), Error (ErrNockNatural a)] s, NockNatural a) =>
  Term a ->
  Term a ->
  Sem s (Term a)
eval initstack initterm = ignoreOpCounts (evalProfile initstack initterm)

evalProfile ::
  forall s a.
  ( Hashable a,
    Integral a,
    Members
      '[ Reader (Storage a),
         State OpCounts,
         Reader EvalOptions,
         Output (Term a),
         Error (NockEvalError a),
         Error (ErrNockNatural a)
       ]
      s,
    NockNatural a
  ) =>
  Term a ->
  Term a ->
  Sem s (Term a)
evalProfile inistack initerm =
  topEvalCtx $
    recEval inistack initerm
  where
    recEval ::
      forall r.
      (r ~ Reader EvalCtx ': s) =>
      Term a ->
      Term a ->
      Sem r (Term a)
    recEval stack term = case term of
      TermAtom a -> throwExpectedCell a
      TermCell c ->
        parseCell c >>= \case
          ParsedAutoConsCell a -> goAutoConsCell a
          ParsedOperatorCell o -> goOperatorCell o
          ParsedAnomaLibCallCell o -> do
            intercept' <- asks (^. evalInterceptAnomaLibCalls)
            let nonInterceptCall = goOperatorCell (o ^. anomaLibCallRaw)
            -- Pass the raw call to goAnomaLibCall so that stdlib intercepts
            -- can choose to use the raw call instead.
            if
                | intercept' -> goAnomaLibCall nonInterceptCall (o ^. anomaLibCallCell)
                | otherwise -> nonInterceptCall
      where
        loc :: Maybe Interval
        loc = term ^. termLoc

        goAnomaLibCall :: Sem r (Term a) -> AnomaLibCall a -> Sem r (Term a)
        goAnomaLibCall nonInterceptCall AnomaLibCall {..} = do
          let w = EvalCrumbAnomaLibCallArgs (CrumbAnomaLibCallArgs _anomaLibCallRef)
          args' <- withCrumb w (recEval stack _anomaLibCallArgs)
          let binArith :: (a -> a -> a) -> Sem r (Term a)
              binArith f = case args' of
                TCell (TAtom l) (TAtom r) -> return (TAtom (f l r))
                _ -> error "expected a cell with two atoms"

              unaArith :: (a -> a) -> Sem r (Term a)
              unaArith f = case args' of
                TAtom n -> return (TAtom (f n))
                _ -> error "expected an atom"

              binCmp :: (a -> a -> Bool) -> Sem r (Term a)
              binCmp f = case args' of
                TCell (TAtom l) (TAtom r) -> return (TermAtom (nockBool (f l r)))
                _ -> error "expected a cell with two atoms"

          case _anomaLibCallRef of
            AnomaLibValue (AnomaRmValue v) -> case v of
              RmZeroDelta -> nonInterceptCall
            AnomaLibFunction (AnomaRmFunction _) -> nonInterceptCall
            AnomaLibFunction (AnomaStdlibFunction f) -> case f of
              StdlibDec -> unaArith pred
              StdlibAdd -> binArith (+)
              StdlibMul -> binArith (*)
              StdlibSub -> binArith (-)
              StdlibDiv -> binArith div
              StdlibMod -> binArith mod
              StdlibLt -> binCmp (<)
              StdlibLe -> binCmp (<=)
              StdlibPow2 -> unaArith (2 ^)
              StdlibEncode -> TermAtom <$> Encoding.jam args'
              StdlibDecode -> case args' of
                TermAtom a -> do
                  r <- Encoding.cueEither a
                  either (throwDecodingFailed args') return r
                TermCell {} -> throwDecodingFailed args' DecodingErrorExpectedAtom
              StdlibVerifyDetached -> case args' of
                TCell (TermAtom sig) (TCell (TermAtom message) (TermAtom pubKey)) -> goVerifyDetached sig message pubKey
                _ -> error "expected a term of the form [signature (atom) message (encoded term) public_key (atom)]"
              StdlibSign -> case args' of
                TCell (TermAtom message) (TermAtom privKey) -> goSign message privKey
                _ -> error "expected a term of the form [message (encoded term) private_key (atom)]"
              StdlibSignDetached -> case args' of
                TCell (TermAtom message) (TermAtom privKey) -> goSignDetached message privKey
                _ -> error "expected a term of the form [message (encoded term) private_key (atom)]"
              StdlibVerify -> case args' of
                TCell (TermAtom signedMessage) (TermAtom pubKey) -> goVerify signedMessage pubKey
                _ -> error "expected a term of the form [signedMessage (atom) public_key (atom)]"
              StdlibCatBytes -> case args' of
                TCell (TermAtom arg1) (TermAtom arg2) -> goCat arg1 arg2
                _ -> error "expected a term with two atoms"
              StdlibFoldBytes -> TermAtom <$> goFoldBytes args'
              StdlibLengthList -> do
                let xs = checkTermToList args'
                let len = integerToNatural (toInteger (length xs))
                TermAtom . mkEmptyAtom <$> fromNatural len
              StdlibLengthBytes -> case args' of
                TermAtom a -> TermAtom <$> goLengthBytes a
                _ -> error "expected an atom"
              -- Use the raw nock code for curry. The nock stdlib curry function is
              -- small. There's no benefit in implementing it separately in the evaluator.
              StdlibCurry -> nonInterceptCall
              StdlibSha256 -> case args' of
                TermAtom a -> TermAtom <$> goSha256 a
                _ -> error "StdlibSha256 expects to be called with an atom"
              StdlibRandomInitGen -> case args' of
                TermAtom a -> goRandomInitGen a
                _ -> error "StdlibRandomInitGen must be called with an atom"
              StdlibRandomNextBytes -> case args' of
                TermCell (Cell g (TermAtom n)) -> goRandomNextBytes n g
                _ -> error "StdlibRandomNextBytes must be called with a cell containing an atom and a term"
              StdlibRandomSplit -> goRandomSplit args'
              StdlibAnomaSetFromList -> return (goAnomaSetFromList args')
              StdlibAnomaSetToList -> return args'
          where
            goAnomaSetFromList :: Term a -> Term a
            goAnomaSetFromList arg =
              foldr
                (\t acc -> TermCell (Cell' t acc emptyCellInfo))
                (TermAtom nockNil)
                (nubHashable (checkTermToList arg))

            serializeSMGen :: R.SMGen -> Term a
            serializeSMGen s =
              let (seed, gamma) = R.unseedSMGen s
                  seedAtom = TermAtom (mkEmptyAtom (fromIntegral seed))
                  gammaAtom = TermAtom (mkEmptyAtom (fromIntegral gamma))
               in TermCell (Cell seedAtom gammaAtom)

            deserializeSMGen :: Term a -> Sem r (R.SMGen)
            deserializeSMGen = \case
              TermCell (Cell (TermAtom s) (TermAtom g)) -> do
                seed :: Word64 <- fromIntegral <$> nockNatural s
                gamma :: Word64 <- fromIntegral <$> nockNatural g
                return (R.seedSMGen' (seed, gamma))
              _ -> error "deserializeSMGen must be called with a cell containing two atoms"

            goRandomNextBytes :: Atom a -> Term a -> Sem r (Term a)
            goRandomNextBytes n g = do
              gen <- deserializeSMGen g
              len :: Int <- fromIntegral <$> nockNatural n
              let (bs, newGen) = R.genByteString len gen
                  newGenTerm = serializeSMGen newGen
              atomBs <- TermAtom <$> byteStringToAtom bs
              return (TermCell (Cell atomBs newGenTerm))

            goRandomInitGen :: Atom a -> Sem r (Term a)
            goRandomInitGen s = do
              seed :: Word64 <- fromIntegral <$> nockNatural s
              return (serializeSMGen (R.mkSMGen seed))

            goRandomSplit :: Term a -> Sem r (Term a)
            goRandomSplit gt = do
              gen <- deserializeSMGen gt
              let (g1, g2) = R.splitSMGen gen
              return (TermCell (Cell (serializeSMGen g1) (serializeSMGen g2)))

            goCat :: Atom a -> Atom a -> Sem r (Term a)
            goCat arg1 arg2 = TermAtom . setAtomHint AtomHintString <$> atomConcatenateBytes arg1 arg2

            goSha256 :: Atom a -> Sem r (Atom a)
            goSha256 a = Encoding.sha256Atom a >>= byteStringToAtom

            goFoldBytes :: Term a -> Sem r (Atom a)
            goFoldBytes c = do
              bs <- mapM nockNatural (checkTermToListAtom c)
              byteStringToAtom (BS.pack (fromIntegral <$> bs))

            goLengthBytes :: Atom a -> Sem r (Atom a)
            goLengthBytes x = do
              bs <- atomToByteString x
              return (mkEmptyAtom (fromIntegral (BS.length bs)))

            checkTermToList :: Term a -> [Term a]
            checkTermToList = \case
              TermAtom x ->
                if
                    | x `nockmaEq` nockNil -> []
                    | otherwise -> error "expected a list to be terminated by nil"
              TermCell c -> c ^. cellLeft : checkTermToList (c ^. cellRight)

            checkTermToListAtom :: Term a -> [Atom a]
            checkTermToListAtom = map check . checkTermToList
              where
                check :: Term a -> Atom a
                check = \case
                  TermAtom x -> x
                  TermCell {} -> error "expect list element to be an atom"

            goVerifyDetached :: Atom a -> Atom a -> Atom a -> Sem r (Term a)
            goVerifyDetached sigT messageT pubKeyT = do
              sig <- Signature <$> atomToByteStringLen signatureLength sigT
              pubKey <- PublicKey <$> atomToByteStringLen publicKeyLength pubKeyT
              message <- atomToByteString messageT
              let res = dverify pubKey message sig
              return (TermAtom (nockBool res))

            goSign :: Atom a -> Atom a -> Sem r (Term a)
            goSign messageT privKeyT = do
              privKey <- SecretKey <$> atomToByteStringLen privateKeyLength privKeyT
              message <- atomToByteString messageT
              res <- byteStringToAtom (sign privKey message)
              return (TermAtom res)

            goSignDetached :: Atom a -> Atom a -> Sem r (Term a)
            goSignDetached messageT privKeyT = do
              privKey <- SecretKey <$> atomToByteStringLen privateKeyLength privKeyT
              message <- atomToByteString messageT
              let (Signature sig) = dsign privKey message
              res <- byteStringToAtom sig
              return (TermAtom res)

            goVerify :: Atom a -> Atom a -> Sem r (Term a)
            goVerify signedMessageT pubKeyT = do
              pubKey <- PublicKey <$> atomToByteStringLen publicKeyLength pubKeyT
              signedMessage <- atomToByteString signedMessageT
              if
                  | verify pubKey signedMessage -> mkMaybeJust . TermAtom <$> byteStringToAtom (removeSignature signedMessage)
                  | otherwise -> return mkMaybeNothing

        mkMaybeNothing :: Term a
        mkMaybeNothing = TermAtom nockNil

        mkMaybeJust :: Term a -> Term a
        mkMaybeJust t = TermCell (Cell (TermAtom nockNil) t)

        goAutoConsCell :: AutoConsCell a -> Sem r (Term a)
        goAutoConsCell c = do
          let w a =
                EvalCrumbAutoCons
                  CrumbAutoCons
                    { _crumbAutoConsTag = a,
                      _crumbAutoConsLoc = loc
                    }
          l' <- withCrumb (w crumbEvalFirst) (recEval stack (TermCell (c ^. autoConsCellLeft)))
          r' <- withCrumb (w crumbEvalSecond) (recEval stack (c ^. autoConsCellRight))
          return (TermCell (Cell l' r'))

        goOperatorCell :: OperatorCell a -> Sem r (Term a)
        goOperatorCell c = do
          countOp (c ^. operatorCellOp)
          case c ^. operatorCellOp of
            OpAddress -> goOpAddress
            OpQuote -> return goOpQuote
            OpApply -> goOpApply
            OpIsCell -> goOpIsCell
            OpInc -> goOpInc
            OpEq -> goOpEq
            OpIf -> goOpIf
            OpSequence -> goOpSequence
            OpPush -> goOpPush
            OpCall -> goOpCall
            OpReplace -> goOpReplace
            OpHint -> goOpHint
            OpScry -> goOpScry
          where
            crumb crumbTag =
              EvalCrumbOperator $
                CrumbOperator
                  { _crumbOperatorOp = c ^. operatorCellOp,
                    _crumbOperatorCellTag = c ^. operatorCellTag,
                    _crumbOperatorTag = crumbTag,
                    _crumbOperatorLoc = loc
                  }

            evalArg :: CrumbTag -> Term a -> Term a -> Sem r (Term a)
            evalArg crumbTag stack' arg = do
              withCrumb (crumb crumbTag) (recEval stack' arg)

            goOpAddress :: Sem r (Term a)
            goOpAddress = do
              cr <- withCrumb (crumb crumbDecodeFirst) (asPath (c ^. operatorCellTerm))
              withCrumb (crumb crumbEval) (subTerm stack cr (c ^. operatorCellTerm . termLoc))

            goOpQuote :: Term a
            goOpQuote = c ^. operatorCellTerm

            goOpIsCell :: Sem r (Term a)
            goOpIsCell = do
              cr <- evalArg crumbEvalFirst stack (c ^. operatorCellTerm)
              return . TermAtom $ case cr of
                TermCell {} -> nockTrue
                TermAtom {} -> nockFalse

            goOpHint :: Sem r (Term a)
            goOpHint = do
              Cell' l r _ <- withCrumb (crumb crumbDecodeFirst) (asCell (c ^. operatorCellTerm))
              case l of
                TAtom {} -> evalArg crumbEvalFirst stack r
                TCell t1 t2 -> do
                  t2' <- evalArg crumbEvalFirst stack t2
                  putsHint <- fromNatural (nockHintValue NockHintPuts)
                  case t1 of
                    TAtom a
                      | a == putsHint ->
                          output t2'
                    _ -> return ()
                  evalArg crumbEvalSecond stack r

            goOpPush :: Sem r (Term a)
            goOpPush = do
              cellTerm <- withCrumb (crumb crumbDecodeFirst) (asCell (c ^. operatorCellTerm))
              l <- evalArg crumbEvalFirst stack (cellTerm ^. cellLeft)
              let s = TermCell (Cell l stack)
              evalArg crumbEvalSecond s (cellTerm ^. cellRight)

            goOpReplace :: Sem r (Term a)
            goOpReplace = do
              Cell' rot1 t2 _ <- withCrumb (crumb crumbDecodeFirst) (asCell (c ^. operatorCellTerm))
              Cell' ro t1 _ <- withCrumb (crumb crumbDecodeSecond) (asCell rot1)
              r <- withCrumb (crumb crumbDecodeThird) (asPath ro)
              t1' <- evalArg crumbEvalFirst stack t1
              t2' <- evalArg crumbEvalSecond stack t2
              setSubTerm t2' r t1'

            goOpApply :: Sem r (Term a)
            goOpApply = do
              cellTerm <- withCrumb (crumb crumbDecodeFirst) (asCell (c ^. operatorCellTerm))
              t1' <- evalArg crumbEvalFirst stack (cellTerm ^. cellLeft)
              t2' <- evalArg crumbEvalSecond stack (cellTerm ^. cellRight)
              evalArg crumbEvalSecond t1' t2'

            goOpIf :: Sem r (Term a)
            goOpIf = do
              cellTerm <- withCrumb (crumb crumbDecodeFirst) (asCell (c ^. operatorCellTerm))
              let t0 = cellTerm ^. cellLeft
              Cell' t1 t2 _ <- withCrumb (crumb crumbDecodeSecond) (asCell (cellTerm ^. cellRight))
              cond <- evalArg crumbEvalFirst stack t0 >>= asBool
              if
                  | cond -> evalArg crumbTrueBranch stack t1
                  | otherwise -> evalArg crumbFalseBranch stack t2

            goOpInc :: Sem r (Term a)
            goOpInc =
              TermAtom . nockSucc
                <$> ( evalArg crumbEvalFirst stack (c ^. operatorCellTerm)
                        >>= withCrumb (crumb crumbDecodeFirst) . asAtom
                    )

            goOpEq :: Sem r (Term a)
            goOpEq = do
              cellTerm <- withCrumb (crumb crumbDecodeFirst) (asCell (c ^. operatorCellTerm))
              l <- evalArg crumbEvalFirst stack (cellTerm ^. cellLeft)
              r <- evalArg crumbEvalSecond stack (cellTerm ^. cellRight)
              return . TermAtom $
                if
                    | nockmaEq l r -> nockTrue
                    | otherwise -> nockFalse

            goOpCall :: Sem r (Term a)
            goOpCall = do
              cellTerm <- withCrumb (crumb crumbDecodeFirst) (asCell (c ^. operatorCellTerm))
              r <- withCrumb (crumb crumbDecodeSecond) (asPath (cellTerm ^. cellLeft))
              t' <- evalArg crumbEvalFirst stack (cellTerm ^. cellRight)
              subTerm t' r (cellTerm ^. cellLeft . termLoc) >>= evalArg crumbEvalSecond t'

            goOpSequence :: Sem r (Term a)
            goOpSequence = do
              cellTerm <- withCrumb (crumb crumbDecodeFirst) (asCell (c ^. operatorCellTerm))
              t1' <- evalArg crumbEvalFirst stack (cellTerm ^. cellLeft)
              evalArg crumbEvalSecond t1' (cellTerm ^. cellRight)

            goOpScry :: Sem r (Term a)
            goOpScry = do
              Cell' typeFormula subFormula _ <- withCrumb (crumb crumbDecodeFirst) (asCell (c ^. operatorCellTerm))
              void (evalArg crumbEvalFirst stack typeFormula)
              key <- evalArg crumbEvalSecond stack subFormula
              fromMaybeM (throwKeyNotInStorage key) (HashMap.lookup (StorageKey key) <$> asks (^. storageKeyValueData))
