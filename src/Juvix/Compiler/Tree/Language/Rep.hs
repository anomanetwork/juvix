module Juvix.Compiler.Tree.Language.Rep where

import Juvix.Prelude

-- | Memory representation of a constructor.
data MemRep
  = -- | Standard representation of a constructor: [tag, field 0, .., field n]
    MemRepConstr
  | -- | A constructor with zero fields (arguments) can be represented as an
    -- unboxed tag.
    MemRepTag
  | -- | If a constructor is the only non-zero-field constructor in its inductive
    -- type, then it can be represented as a tagless tuple (the tag is not needed
    -- to distinguish from other unboxed tag constructors)
    MemRepTuple
  | -- | If a zero-field constructor is the only constructor in its inductive
    -- type, then it's a unit and can be omitted altogether.
    MemRepUnit
  | -- | If a constructor has exactly one field and there are no other
    -- constructors in its inductive type (in Haskell, such types are
    -- "newtypes"), then it can be unpacked and represented by the value of its
    -- field. If the tags are globally unique, this can be applied even if there
    -- are other constructors, as long as no two unpacked fields have the same
    -- type or an untagged representation (we can't distinguish between tuples
    -- representing constructors of different inductive types because they have
    -- no tag). The argument is the representation of the wrapped value.
    MemRepUnpacked ValRep
  deriving stock (Eq, Generic)

-- | Representation of values.
data ValRep
  = -- | Unboxed integer or pointer to boxed integer.
    ValRepInteger
  | -- | Unboxed boolean.
    ValRepBool
  | -- | Pointer to string.
    ValRepString
  | -- | Raw word with arbitrary bit pattern.
    ValRepWord
  | -- | Constructor of an inductive type with a given representation.
    ValRepInd IndRep
  deriving stock (Eq, Generic)

-- | Representation of an inductive type.
data IndRep
  = -- | Standard representation of an inductive type: all constructors have
    -- MemRepConstr representation.
    IndRepStandard
  | -- | All constructors have MemRepTag representation.
    IndRepEnum
  | -- | All constructors have MemRepTag representation, except one which has
    -- MemRepTuple representation.
    IndRepEnumRecord
  | -- | The inductive type has one constructor with MemRepUnpacked
    -- representation and one or more constructors with MemRepTag
    -- representation. There are no other constructors. See @IndRepNewtype@.
    IndRepEnumMaybe ValRep
  | -- | The inductive type has one constructor with MemRepTuple representation.
    -- There are no other constructors.
    IndRepRecord
  | -- | The inductive type has one constructor with MemRepUnit representation.
    -- There are no other constructors.
    IndRepUnit
  | -- | The inductive type has one constructor with MemRepUnpacked
    -- representation. There are no other constructors. The argument is the
    -- representation of the wrapped value.
    IndRepNewtype ValRep
  | -- | The constructors can have any representation as long as there is no
    -- ambiguity arising from unpacking.
    IndRepMixed
  deriving stock (Eq, Generic)

instance Serialize MemRep

instance Serialize ValRep

instance Serialize IndRep
