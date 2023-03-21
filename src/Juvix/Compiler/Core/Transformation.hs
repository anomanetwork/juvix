module Juvix.Compiler.Core.Transformation
  ( module Juvix.Compiler.Core.Transformation.Base,
    module Juvix.Compiler.Core.Transformation,
    module Juvix.Compiler.Core.Transformation.Eta,
    module Juvix.Compiler.Core.Transformation.LambdaLetRecLifting,
    module Juvix.Compiler.Core.Transformation.TopEtaExpand,
    module Juvix.Compiler.Core.Data.TransformationId,
  )
where

import Juvix.Compiler.Core.Data.TransformationId
import Juvix.Compiler.Core.Error
import Juvix.Compiler.Core.Options
import Juvix.Compiler.Core.Transformation.Base
import Juvix.Compiler.Core.Transformation.CheckGeb
import Juvix.Compiler.Core.Transformation.ComputeTypeInfo
import Juvix.Compiler.Core.Transformation.ConvertBuiltinTypes
import Juvix.Compiler.Core.Transformation.DisambiguateNames
import Juvix.Compiler.Core.Transformation.Eta
import Juvix.Compiler.Core.Transformation.Identity
import Juvix.Compiler.Core.Transformation.LambdaLetRecLifting
import Juvix.Compiler.Core.Transformation.MatchToCase
import Juvix.Compiler.Core.Transformation.MoveApps
import Juvix.Compiler.Core.Transformation.NaiveMatchToCase qualified as Naive
import Juvix.Compiler.Core.Transformation.NatToInt
import Juvix.Compiler.Core.Transformation.RemoveTypeArgs
import Juvix.Compiler.Core.Transformation.TopEtaExpand
import Juvix.Compiler.Core.Transformation.UnrollRecursion

applyTransformations :: forall r. Members '[Error JuvixError, Reader Options] r => [TransformationId] -> InfoTable -> Sem r InfoTable
applyTransformations ts tbl = foldl' (\acc tid -> acc >>= appTrans tid) (return tbl) ts
  where
    appTrans :: TransformationId -> InfoTable -> Sem r InfoTable
    appTrans = \case
      LambdaLetRecLifting -> return . lambdaLetRecLifting
      LetRecLifting -> return . letRecLifting
      Identity -> return . identity
      TopEtaExpand -> return . topEtaExpand
      RemoveTypeArgs -> return . removeTypeArgs
      MoveApps -> return . moveApps
      NatToInt -> return . natToInt
      ConvertBuiltinTypes -> return . convertBuiltinTypes
      ComputeTypeInfo -> return . computeTypeInfo
      UnrollRecursion -> return . unrollRecursion
      MatchToCase -> mapError (JuvixError @CoreError) . matchToCase
      NaiveMatchToCase -> return . Naive.matchToCase
      EtaExpandApps -> return . etaExpansionApps
      DisambiguateNames -> return . disambiguateNames
      CheckGeb -> mapError (JuvixError @CoreError) . checkGeb
