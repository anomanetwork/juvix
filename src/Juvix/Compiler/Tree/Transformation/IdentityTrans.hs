module Juvix.Compiler.Tree.Transformation.IdentityTrans
  ( module Juvix.Compiler.Tree.Transformation.IdentityTrans,
    module Juvix.Compiler.Tree.Transformation.Base,
  )
where

import Juvix.Compiler.Tree.Data.InfoTable
import Juvix.Compiler.Tree.Extra.Recursors
import Juvix.Compiler.Tree.Transformation.Base

identity :: Module -> Module
identity = run . mapT' (const return)

identityU :: Module -> Module
identityU = run . mapT' (const (return . umap id))

identityD :: Module -> Module
identityD = run . mapT' (const (return . dmap id))
