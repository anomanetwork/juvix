module Juvix.Compiler.Nockma.Evaluator.Error
  ( module Juvix.Compiler.Nockma.Evaluator.Error,
    module Juvix.Compiler.Nockma.Evaluator.Crumbs,
    module Juvix.Compiler.Nockma.Evaluator.Storage,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Nockma.Encoding
import Juvix.Compiler.Nockma.Evaluator.Crumbs
import Juvix.Compiler.Nockma.Evaluator.Storage
import Juvix.Compiler.Nockma.Language
import Juvix.Compiler.Nockma.Pretty.Base
import Juvix.Prelude hiding (Atom, Path)

data NockEvalError a
  = ErrInvalidPath (InvalidPath a)
  | ErrInvalidNockOp (InvalidNockOp a)
  | ErrExpectedAtom (ExpectedAtom a)
  | ErrExpectedCell (ExpectedCell a)
  | -- TODO perhaps this should be a repl error type
    ErrNoStack NoStack
  | -- TODO perhaps this should be a repl error type
    ErrAssignmentNotFound Text
  | ErrKeyNotInStorage (KeyNotInStorage a)
  | ErrDecodingFailed (DecodingFailed a)
  | ErrVerificationFailed (VerificationFailed a)

data ExpectedCell a = ExpectedCell
  { _expectedCellCtx :: EvalCtx,
    _expectedCellAtom :: Atom a
  }

data ExpectedAtom a = ExpectedAtom
  { _expectedAtomCtx :: EvalCtx,
    _expectedAtomCell :: Cell a
  }

data InvalidPath a = InvalidPath
  { _invalidPathCtx :: EvalCtx,
    _invalidPathTerm :: Term a,
    _invalidPathPath :: Path,
    _invalidPathLocation :: Maybe Interval
  }

data KeyNotInStorage a = KeyNotInStorage
  { _keyNotInStorageKey :: Term a,
    _keyNotInStorageStorage :: Storage a
  }

data InvalidNockOp a = InvalidNockOp
  { _invalidNockOpCtx :: EvalCtx,
    _invalidNockOp :: Atom a
  }

data NoStack = NoStack

data DecodingFailed a = DecodingFailed
  { _decodingFailedCtx :: EvalCtx,
    _decodingFailedTerm :: Term a,
    _decodingFailedReason :: DecodingError
  }

data VerificationFailed a = VerificationFailed
  { _verificationFailedCtx :: EvalCtx,
    _verificationFailedMessage :: Atom a,
    _verificationFailedPublicKey :: Atom a
  }

throwInvalidNockOp :: (Members '[Error (NockEvalError a), Reader EvalCtx] r) => Atom a -> Sem r x
throwInvalidNockOp a = do
  ctx <- ask
  throw $
    ErrInvalidNockOp
      InvalidNockOp
        { _invalidNockOpCtx = ctx,
          _invalidNockOp = a
        }

throwInvalidPath :: (Members '[Error (NockEvalError a), Reader EvalCtx] r) => Maybe Interval -> Term a -> Path -> Sem r x
throwInvalidPath mi tm p = do
  ctx <- ask
  throw $
    ErrInvalidPath
      InvalidPath
        { _invalidPathCtx = ctx,
          _invalidPathTerm = tm,
          _invalidPathPath = p,
          _invalidPathLocation = mi
        }

throwExpectedCell :: (Members '[Error (NockEvalError a), Reader EvalCtx] r) => Atom a -> Sem r x
throwExpectedCell a = do
  ctx <- ask
  throw $
    ErrExpectedCell
      ExpectedCell
        { _expectedCellCtx = ctx,
          _expectedCellAtom = a
        }

throwExpectedAtom :: (Members '[Error (NockEvalError a), Reader EvalCtx] r) => Cell a -> Sem r x
throwExpectedAtom a = do
  ctx <- ask
  throw $
    ErrExpectedAtom
      ExpectedAtom
        { _expectedAtomCtx = ctx,
          _expectedAtomCell = a
        }

throwKeyNotInStorage :: (Members '[Reader (Storage a), Error (NockEvalError a)] r) => Term a -> Sem r x
throwKeyNotInStorage k = do
  s <- ask
  throw $
    ErrKeyNotInStorage
      KeyNotInStorage
        { _keyNotInStorageKey = k,
          _keyNotInStorageStorage = s
        }

throwDecodingFailed :: (Members '[Error (NockEvalError a), Reader EvalCtx] r) => Term a -> DecodingError -> Sem r x
throwDecodingFailed a e = do
  ctx <- ask
  throw $
    ErrDecodingFailed
      DecodingFailed
        { _decodingFailedCtx = ctx,
          _decodingFailedTerm = a,
          _decodingFailedReason = e
        }

throwVerificationFailed :: (Members '[Error (NockEvalError a), Reader EvalCtx] r) => Atom a -> Atom a -> Sem r x
throwVerificationFailed m k = do
  ctx <- ask
  throw $
    ErrVerificationFailed
      VerificationFailed
        { _verificationFailedCtx = ctx,
          _verificationFailedMessage = m,
          _verificationFailedPublicKey = k
        }

instance PrettyCode NoStack where
  ppCode _ = return "Missing stack"

instance (PrettyCode a, NockNatural a) => PrettyCode (InvalidPath a) where
  ppCode InvalidPath {..} = do
    ctx <- ppCtx _invalidPathCtx
    path <- ppCode _invalidPathPath
    tm <- ppCode _invalidPathTerm
    loc <- mapM ppCode _invalidPathLocation
    return (ctx <> "The path" <+> path <+> "is invalid for the following term:" <> line <> tm <>? ((line <>) <$> loc))

instance (PrettyCode a, NockNatural a) => PrettyCode (ExpectedAtom a) where
  ppCode ExpectedAtom {..} = do
    cell <- ppCode _expectedAtomCell
    ctx <- ppCtx _expectedAtomCtx
    let atm = annotate AnnImportant "atom"
    return (ctx <> "Expected an" <+> atm <+> "but got:" <> line <> cell)

instance (PrettyCode a, NockNatural a) => PrettyCode (InvalidNockOp a) where
  ppCode InvalidNockOp {..} = do
    atm <- ppCode _invalidNockOp
    ctx <- ppCtx _invalidNockOpCtx
    return (ctx <> "Invalid nockOp or path: " <+> atm)

instance (PrettyCode a, NockNatural a) => PrettyCode (ExpectedCell a) where
  ppCode ExpectedCell {..} = do
    atm <- ppCode _expectedCellAtom
    ctx <- ppCtx _expectedCellCtx
    let cell = annotate AnnImportant "cell"
    return (ctx <> "Expected a" <+> cell <+> "but got:" <> line <> atm)

instance (PrettyCode a, NockNatural a) => PrettyCode (KeyNotInStorage a) where
  ppCode :: forall r. (Member (Reader Options) r) => KeyNotInStorage a -> Sem r (Doc Ann)
  ppCode KeyNotInStorage {..} = do
    tm <- ppCode _keyNotInStorageKey
    hashMapKvs <- vsep <$> mapM combineKeyValue (HashMap.toList (_keyNotInStorageStorage ^. storageKeyValueData))
    return ("The key" <+> tm <+> "is not found in the storage." <> line <> "Storage contains the following key value pairs:" <> line <> hashMapKvs)
    where
      combineKeyValue :: (StorageKey a, Term a) -> Sem r (Doc Ann)
      combineKeyValue (t1, t2) = do
        pt1 <- ppCode t1
        pt2 <- ppCode t2
        return (pt1 <+> ":=" <+> pt2)

instance (PrettyCode a, NockNatural a) => PrettyCode (DecodingFailed a) where
  ppCode DecodingFailed {..} = do
    t <- ppCode _decodingFailedTerm
    ctx <- ppCtx _decodingFailedCtx
    r <- ppCode _decodingFailedReason
    return (ctx <> "Decoding the term" <+> t <+> "failed with reason:" <> line <> r)

instance (PrettyCode a, NockNatural a) => PrettyCode (VerificationFailed a) where
  ppCode VerificationFailed {..} = do
    m <- ppCode _verificationFailedMessage
    ctx <- ppCtx _verificationFailedCtx
    k <- ppCode _verificationFailedPublicKey
    return
      ( ctx
          <> "Signature verification failed for message atom:"
          <> line
          <> m
          <> line
          <> "and public key atom:"
          <> line
          <> k
      )

instance (PrettyCode a, NockNatural a) => PrettyCode (NockEvalError a) where
  ppCode = \case
    ErrInvalidPath e -> ppCode e
    ErrInvalidNockOp e -> ppCode e
    ErrExpectedAtom e -> ppCode e
    ErrExpectedCell e -> ppCode e
    ErrNoStack e -> ppCode e
    ErrAssignmentNotFound e -> return (pretty e)
    ErrKeyNotInStorage e -> ppCode e
    ErrDecodingFailed e -> ppCode e
    ErrVerificationFailed e -> ppCode e
