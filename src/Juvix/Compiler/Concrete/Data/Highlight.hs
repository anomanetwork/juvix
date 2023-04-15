module Juvix.Compiler.Concrete.Data.Highlight
  ( module Juvix.Compiler.Concrete.Data.Highlight,
    module Juvix.Compiler.Concrete.Data.Highlight.Input,
    module Juvix.Compiler.Concrete.Data.Highlight.Properties,
  )
where

import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as ByteString
import Data.Text.Encoding qualified as Text
import Juvix.Compiler.Concrete.Data.Highlight.Input
import Juvix.Compiler.Concrete.Data.Highlight.Properties
import Juvix.Compiler.Concrete.Data.Highlight.SExp
import Juvix.Compiler.Concrete.Data.ScopedName
-- import Juvix.Compiler.Internal.Language qualified as Internal
-- import Juvix.Compiler.Internal.Pretty qualified as Internal

import Juvix.Compiler.Internal.Pretty qualified as Internal
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Data.Context qualified as Internal
import Juvix.Prelude as Prelude hiding (show)
import Prelude qualified

data HighlightBackend
  = Emacs
  | Json
  deriving stock (Bounded, Enum, Data)

instance Show HighlightBackend where
  show = \case
    Emacs -> "emacs"
    Json -> "json"

go :: HighlightBackend -> HighlightInput -> ByteString
go = \case
  Emacs -> Text.encodeUtf8 . renderSExp . toSExp . buildProperties
  Json -> ByteString.toStrict . Aeson.encode . rawProperties . buildProperties

goErrors :: HighlightBackend -> [Interval] -> ByteString
goErrors = \case
  Emacs -> Text.encodeUtf8 . renderSExp . toSExp . errorProperties
  Json -> ByteString.toStrict . Aeson.encode . rawProperties . errorProperties

errorProperties :: [Interval] -> Properties
errorProperties l =
  Properties
    { _propertiesFace = map goFaceError l,
      _propertiesGoto = [],
      _propertiesType = []
    }

buildProperties :: HighlightInput -> Properties
buildProperties HighlightInput {..} =
  Properties
    { _propertiesFace =
        map goFaceParsedItem _highlightParsed
          <> mapMaybe goFaceName _highlightNames,
      _propertiesGoto = map goGotoProperty _highlightNames,
      _propertiesType = mapMaybe (goTypeProperty _highlightTypes) _highlightNames
    }

nameKindFace :: NameKind -> Maybe Face
nameKindFace = \case
  KNameConstructor -> Just FaceConstructor
  KNameInductive -> Just FaceInductive
  KNameFunction -> Just FaceFunction
  KNameTopModule -> Just FaceModule
  KNameLocalModule -> Just FaceModule
  KNameAxiom -> Just FaceAxiom
  KNameLocal -> Nothing

goFaceError :: Interval -> PropertyFace
goFaceError i =
  PropertyFace
    { _faceFace = FaceError,
      _faceInterval = i
    }

goFaceParsedItem :: ParsedItem -> PropertyFace
goFaceParsedItem i =
  PropertyFace
    { _faceFace = f,
      _faceInterval = i ^. parsedLoc
    }
  where
    f = case i ^. parsedTag of
      ParsedTagKeyword -> FaceKeyword
      ParsedTagLiteralInt -> FaceNumber
      ParsedTagLiteralString -> FaceString
      ParsedTagComment -> FaceComment

goFaceName :: AName -> Maybe PropertyFace
goFaceName n = do
  f <- nameKindFace (getNameKind n)
  return (PropertyFace (getLoc n) f)

goGotoProperty :: AName -> PropertyGoto
goGotoProperty (AName n) = PropertyGoto {..}
  where
    _gotoInterval = getLoc n
    _gotoPos = n ^. nameDefined . intervalStart
    _gotoFile = n ^. nameDefined . intervalFile

goTypeProperty :: Internal.TypesTable -> AName -> Maybe PropertyType
goTypeProperty tbl (AName n) = case tbl ^. at (n ^. nameId) of
  Nothing -> Nothing
  Just ty ->
    let _typeType :: Text = Internal.ppPrint ty
     in Just PropertyType {..}
  where
    _typeInterval = getLoc n
