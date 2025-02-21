module Juvix.Compiler.Core.Data.TransformationId where

import Juvix.Compiler.Core.Data.TransformationId.Base
import Juvix.Compiler.Core.Data.TransformationId.Strings
import Juvix.Prelude

data TransformationId
  = LambdaLetRecLifting
  | LetRecLifting
  | TopEtaExpand
  | RemoveTypeArgs
  | MoveApps
  | NatToPrimInt
  | IntToPrimInt
  | ConvertBuiltinTypes
  | IdentityTrans
  | UnrollRecursion
  | ComputeTypeInfo
  | DetectConstantSideConditions
  | DetectRedundantPatterns
  | MatchToCase
  | EtaExpandApps
  | DisambiguateNames
  | CombineInfoTables
  | CheckExec
  | CheckRust
  | CheckAnoma
  | CheckCairo
  | Normalize
  | LetFolding
  | LambdaFolding
  | LoopHoisting
  | Inlining
  | MandatoryInlining
  | SimplifyIfs
  | SimplifyComparisons
  | SpecializeArgs
  | CaseFolding
  | CasePermutation
  | ConstantFolding
  | FilterUnreachable
  | OptPhaseEval
  | OptPhaseExec
  | OptPhaseMain
  | OptPhasePreLifting
  deriving stock (Data, Bounded, Enum, Show)

data PipelineId
  = PipelineEval
  | PipelineExec
  | PipelineTypecheck
  | PipelineNormalize
  | PipelineStripped
  deriving stock (Data, Bounded, Enum)

type TransformationLikeId = TransformationLikeId' TransformationId PipelineId

toTypecheckTransformations :: [TransformationId]
toTypecheckTransformations =
  [DetectConstantSideConditions, DetectRedundantPatterns, MatchToCase]

toEvalTransformations :: [TransformationId]
toEvalTransformations =
  [ EtaExpandApps,
    DetectConstantSideConditions,
    DetectRedundantPatterns,
    MatchToCase,
    NatToPrimInt,
    IntToPrimInt,
    ConvertBuiltinTypes,
    OptPhaseEval,
    DisambiguateNames
  ]

toExecTransformations :: [TransformationId]
toExecTransformations =
  toEvalTransformations ++ [OptPhasePreLifting, LambdaLetRecLifting, TopEtaExpand, OptPhaseExec, MoveApps]

toNormalizeTransformations :: [TransformationId]
toNormalizeTransformations =
  toEvalTransformations ++ [CombineInfoTables, LetRecLifting, LetFolding, UnrollRecursion]

toStrippedTransformations :: TransformationId -> [TransformationId]
toStrippedTransformations checkId =
  [FilterUnreachable, checkId, RemoveTypeArgs, DisambiguateNames]

instance TransformationId' TransformationId where
  transformationText :: TransformationId -> Text
  transformationText = \case
    LambdaLetRecLifting -> strLifting
    LetRecLifting -> strLetRecLifting
    TopEtaExpand -> strTopEtaExpand
    DetectConstantSideConditions -> strDetectConstantSideConditions
    DetectRedundantPatterns -> strDetectRedundantPatterns
    MatchToCase -> strMatchToCase
    EtaExpandApps -> strEtaExpandApps
    IdentityTrans -> strIdentity
    RemoveTypeArgs -> strRemoveTypeArgs
    MoveApps -> strMoveApps
    NatToPrimInt -> strNatToPrimInt
    IntToPrimInt -> strIntToPrimInt
    ConvertBuiltinTypes -> strConvertBuiltinTypes
    ComputeTypeInfo -> strComputeTypeInfo
    UnrollRecursion -> strUnrollRecursion
    DisambiguateNames -> strDisambiguateNames
    CombineInfoTables -> strCombineInfoTables
    CheckExec -> strCheckExec
    CheckRust -> strCheckRust
    CheckAnoma -> strCheckAnoma
    CheckCairo -> strCheckCairo
    Normalize -> strNormalize
    LetFolding -> strLetFolding
    LambdaFolding -> strLambdaFolding
    LoopHoisting -> strLoopHoisting
    Inlining -> strInlining
    MandatoryInlining -> strMandatoryInlining
    SimplifyIfs -> strSimplifyIfs
    SimplifyComparisons -> strSimplifyComparisons
    SpecializeArgs -> strSpecializeArgs
    CaseFolding -> strCaseFolding
    CasePermutation -> strCasePermutation
    ConstantFolding -> strConstantFolding
    FilterUnreachable -> strFilterUnreachable
    OptPhaseEval -> strOptPhaseEval
    OptPhaseExec -> strOptPhaseExec
    OptPhaseMain -> strOptPhaseMain
    OptPhasePreLifting -> strOptPhasePreLifting

instance PipelineId' TransformationId PipelineId where
  pipelineText :: PipelineId -> Text
  pipelineText = \case
    PipelineEval -> strEvalPipeline
    PipelineExec -> strExecPipeline
    PipelineTypecheck -> strTypecheckPipeline
    PipelineNormalize -> strNormalizePipeline
    PipelineStripped -> strStrippedPipeline

  pipeline :: PipelineId -> [TransformationId]
  pipeline = \case
    PipelineEval -> toEvalTransformations
    PipelineExec -> toExecTransformations
    PipelineTypecheck -> toTypecheckTransformations
    PipelineNormalize -> toNormalizeTransformations
    PipelineStripped -> toStrippedTransformations IdentityTrans
