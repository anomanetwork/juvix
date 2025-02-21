module Juvix.Compiler.Tree.Language.Builtins where

import Juvix.Prelude

data BoolOp
  = OpIntLt
  | OpIntLe
  | OpEq
  deriving stock (Eq, Generic)

instance Serialize BoolOp

data BinaryOp
  = OpBool BoolOp
  | OpIntAdd
  | OpIntSub
  | OpIntMul
  | OpIntDiv
  | OpIntMod
  | OpFieldAdd
  | OpFieldSub
  | OpFieldMul
  | OpFieldDiv
  | OpStrConcat
  deriving stock (Eq, Generic)

instance Serialize BinaryOp

isCommutative :: BinaryOp -> Bool
isCommutative = \case
  OpIntAdd -> True
  OpIntSub -> False
  OpIntMul -> True
  OpIntDiv -> False
  OpIntMod -> False
  OpBool OpIntLt -> False
  OpBool OpIntLe -> False
  OpFieldAdd -> True
  OpFieldSub -> False
  OpFieldMul -> True
  OpFieldDiv -> False
  OpBool OpEq -> True
  OpStrConcat -> False

data UnaryOp
  = -- | Convert the argument to a string. JV* opcode: `show`.
    OpShow
  | -- | Convert a string to an integer. JV* opcode: `atoi`.
    OpStrToInt
  | -- | Convert an integer to a field element. JV* opcode: `itof`.
    OpIntToField
  | -- | Convert a field element to an integer. JV* opcode: `ftoi`.
    OpFieldToInt
  | -- | Convert an integer to a UInt8. JV* opcode: `itou8`
    OpIntToUInt8
  | -- | Convert an UInt8 to an integer. JV* opcode: `u8toi`
    OpUInt8ToInt
  | -- | Compute the number of expected arguments for the given closure. JV*
    -- opcode: `argsnum`.
    OpArgsNum
  deriving stock (Eq, Generic)

instance Serialize UnaryOp

-- | Builtin Cairo operations. Implemented only in the Cairo backend.
data CairoOp
  = -- | Cairo Poseidon hash builtin (hades permutation).
    OpCairoPoseidon
  | -- | Cairo Elliptic Curve operation.
    OpCairoEc
  | -- | Cairo random elliptic curve point generation.
    OpCairoRandomEcPoint
  deriving stock (Eq, Generic)

instance Serialize CairoOp

-- | Builtin ByteArray operations
data ByteArrayOp
  = -- | Convert a list of UInt8 to a ByteArray
    OpByteArrayFromListUInt8
  | -- | Get the size of a ByteArray
    OpByteArrayLength
  deriving stock (Eq, Generic)

instance Serialize ByteArrayOp

cairoOpArgsNum :: CairoOp -> Int
cairoOpArgsNum = \case
  OpCairoPoseidon -> 1
  OpCairoEc -> 3
  OpCairoRandomEcPoint -> 0

-- | Builtin Anoma operations. Implemented only in the Anoma backend.
data AnomaOp
  = -- | Get a value by key from Anoma storage
    OpAnomaGet
  | -- | Encode a value to an Anoma atom
    OpAnomaEncode
  | -- | Decode a value from an Anoma atom
    OpAnomaDecode
  | -- | Verify a cryptogtaphic signature of an Anoma value
    OpAnomaVerifyDetached
  | -- | Cryptographically sign an Anoma value using a secret key
    OpAnomaSign
  | -- | Verify a signature obtained from OpAnomaSign using a public key
    OpAnomaVerifyWithMessage
  | -- | Produce a cryptographic signature of an Anoma value
    OpAnomaSignDetached
  | -- | Construct an Anoma atom from a ByteArray with LSB ordering
    OpAnomaByteArrayToAnomaContents
  | -- | Construct a ByteArray from the bytes of an Anoma atom with LSB ordering
    OpAnomaByteArrayFromAnomaContents
  | -- | Hash a value using SHA256
    OpAnomaSha256
  | -- | Compute the commitment of a Resource
    OpAnomaResourceCommitment
  | -- | Compute the nullifier of a Resource
    OpAnomaResourceNullifier
  | -- | Compute the kind of a Resource
    OpAnomaResourceKind
  | -- | Compute the delta of a Resource
    OpAnomaResourceDelta
  | -- | Compute the delta of an Action
    OpAnomaActionDelta
  | -- | Compute the delta of a list of Actions
    OpAnomaActionsDelta
  | -- | Compute the proof of an Action
    OpAnomaProveAction
  | -- | Compute the proof of a Delta
    OpAnomaProveDelta
  | -- | The zero Delta
    OpAnomaZeroDelta
  | -- | Add Deltas
    OpAnomaAddDelta
  | -- | Subtract Deltas
    OpAnomaSubDelta
  | -- | Initialize a pseudorandom number generator
    OpAnomaRandomGeneratorInit
  | -- | Generate n random bytes using the pseudorandom number generator
    OpAnomaRandomNextBytes
  | -- | Split a pseudorandom number generator into two uncorrelated generators
    OpAnomaRandomSplit
  | -- | Returns true if its argument is a commitment
    OpAnomaIsCommitment
  | -- | Returns true if its argument is a nullifier
    OpAnomaIsNullifier
  | OpAnomaSetToList
  | OpAnomaSetFromList
  deriving stock (Eq, Show, Generic)

instance Serialize AnomaOp
