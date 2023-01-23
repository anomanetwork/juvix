module Scope.Negative (allTests) where

import Base
import Juvix.Compiler.Builtins (iniState)
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver.Error
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Error
import Juvix.Compiler.Pipeline

type FailMsg = String

data NegTest a = NegTest
  { _name :: String,
    _relDir :: Path Rel Dir,
    _file :: Path Rel File,
    _checkErr :: a -> Maybe FailMsg
  }

root :: Path Abs Dir
root = relToProject $(mkRelDir "tests/negative")

testDescr :: (Typeable a) => NegTest a -> TestDescr
testDescr NegTest {..} =
  let tRoot = root <//> _relDir
      file' = tRoot <//> _file
   in TestDescr
        { _testName = _name,
          _testRoot = tRoot,
          _testAssertion = Single $ do
            let entryPoint = defaultEntryPoint tRoot file'
            res <- runIOEither iniState entryPoint upToAbstract
            case mapLeft fromJuvixError res of
              Left (Just err) -> whenJust (_checkErr err) assertFailure
              Left Nothing -> assertFailure "An error ocurred but it was not in the scoper."
              Right {} -> assertFailure "The scope checker did not find an error."
        }

allTests :: TestTree
allTests =
  testGroup
    "Scope negative tests"
    ( map (mkTest . testDescr) scoperErrorTests
        <> map (mkTest . testDescr) filesErrorTests
    )

wrongError :: Maybe FailMsg
wrongError = Just "Incorrect error"

scoperErrorTests :: [NegTest ScoperError]
scoperErrorTests =
  [ NegTest
      "Not in scope"
      $(mkRelDir ".")
      $(mkRelFile "NotInScope.juvix")
      $ \case
        ErrSymNotInScope {} -> Nothing
        _ -> wrongError,
    NegTest
      "Qualified not in scope"
      $(mkRelDir ".")
      $(mkRelFile "QualSymNotInScope.juvix")
      $ \case
        ErrQualSymNotInScope {} -> Nothing
        _ -> wrongError,
    NegTest
      "Multiple declarations"
      $(mkRelDir ".")
      $(mkRelFile "MultipleDeclarations.juvix")
      $ \case
        ErrMultipleDeclarations {} -> Nothing
        _ -> wrongError,
    NegTest
      "Import cycle"
      $(mkRelDir "ImportCycle")
      $(mkRelFile "A.juvix")
      $ \case
        ErrImportCycle {} -> Nothing
        _ -> wrongError,
    NegTest
      "Binding group conflict (function clause)"
      $(mkRelDir "BindGroupConflict")
      $(mkRelFile "Clause.juvix")
      $ \case
        ErrBindGroup {} -> Nothing
        _ -> wrongError,
    NegTest
      "Binding group conflict (lambda clause)"
      $(mkRelDir "BindGroupConflict")
      $(mkRelFile "Lambda.juvix")
      $ \case
        ErrBindGroup {} -> Nothing
        _ -> wrongError,
    NegTest
      "Infix error (expression)"
      $(mkRelDir ".")
      $(mkRelFile "InfixError.juvix")
      $ \case
        ErrInfixParser {} -> Nothing
        _ -> wrongError,
    NegTest
      "Infix error (pattern)"
      $(mkRelDir ".")
      $(mkRelFile "InfixErrorP.juvix")
      $ \case
        ErrInfixPattern {} -> Nothing
        _ -> wrongError,
    NegTest
      "Duplicate fixity declaration"
      $(mkRelDir ".")
      $(mkRelFile "DuplicateFixity.juvix")
      $ \case
        ErrDuplicateFixity {} -> Nothing
        _ -> wrongError,
    NegTest
      "Multiple export conflict"
      $(mkRelDir ".")
      $(mkRelFile "MultipleExportConflict.juvix")
      $ \case
        ErrMultipleExport {} -> Nothing
        _ -> wrongError,
    NegTest
      "Module not in scope"
      $(mkRelDir ".")
      $(mkRelFile "ModuleNotInScope.juvix")
      $ \case
        ErrModuleNotInScope {} -> Nothing
        _ -> wrongError,
    NegTest
      "Unused operator syntax definition"
      $(mkRelDir ".")
      $(mkRelFile "UnusedOperatorDef.juvix")
      $ \case
        ErrUnusedOperatorDef {} -> Nothing
        _ -> wrongError,
    NegTest
      "Ambiguous symbol"
      $(mkRelDir ".")
      $(mkRelFile "AmbiguousSymbol.juvix")
      $ \case
        ErrAmbiguousSym {} -> Nothing
        _ -> wrongError,
    NegTest
      "Lacks function clause"
      $(mkRelDir ".")
      $(mkRelFile "LacksFunctionClause.juvix")
      $ \case
        ErrLacksFunctionClause {} -> Nothing
        _ -> wrongError,
    NegTest
      "Lacks function clause inside let"
      $(mkRelDir ".")
      $(mkRelFile "LetMissingClause.juvix")
      $ \case
        ErrLacksFunctionClause {} -> Nothing
        _ -> wrongError,
    NegTest
      "Incorrect top module path"
      $(mkRelDir ".")
      $(mkRelFile "WrongModuleName.juvix")
      $ \case
        ErrWrongTopModuleName {} -> Nothing
        _ -> wrongError,
    NegTest
      "Ambiguous export"
      $(mkRelDir ".")
      $(mkRelFile "AmbiguousExport.juvix")
      $ \case
        ErrMultipleExport {} -> Nothing
        _ -> wrongError,
    NegTest
      "Ambiguous nested modules"
      $(mkRelDir ".")
      $(mkRelFile "AmbiguousModule.juvix")
      $ \case
        ErrAmbiguousModuleSym {} -> Nothing
        _ -> wrongError,
    NegTest
      "Ambiguous nested constructors"
      $(mkRelDir ".")
      $(mkRelFile "AmbiguousConstructor.juvix")
      $ \case
        ErrAmbiguousSym {} -> Nothing
        _ -> wrongError,
    NegTest
      "Wrong location of a compile block"
      $(mkRelDir "CompileBlocks")
      $(mkRelFile "WrongLocationCompileBlock.juvix")
      $ \case
        ErrWrongLocationCompileBlock {} -> Nothing
        _ -> wrongError,
    NegTest
      "Implicit argument on the left of an application"
      $(mkRelDir ".")
      $(mkRelFile "AppLeftImplicit.juvix")
      $ \case
        ErrAppLeftImplicit {} -> Nothing
        _ -> wrongError,
    NegTest
      "Multiple compile blocks for the same name"
      $(mkRelDir "CompileBlocks")
      $(mkRelFile "MultipleCompileBlockSameName.juvix")
      $ \case
        ErrMultipleCompileBlockSameName {} -> Nothing
        _ -> wrongError,
    NegTest
      "Multiple rules for a backend inside a compile block"
      $(mkRelDir "CompileBlocks")
      $(mkRelFile "MultipleCompileRuleSameBackend.juvix")
      $ \case
        ErrMultipleCompileRuleSameBackend {} -> Nothing
        _ -> wrongError,
    NegTest
      "issue 230"
      $(mkRelDir "230")
      $(mkRelFile "Prod.juvix")
      $ \case
        ErrQualSymNotInScope {} -> Nothing
        _ -> wrongError,
    NegTest
      "Double braces in pattern"
      $(mkRelDir ".")
      $(mkRelFile "NestedPatternBraces.juvix")
      $ \case
        ErrDoubleBracesPattern {} -> Nothing
        _ -> wrongError,
    NegTest
      "As-Pattern aliasing variable"
      $(mkRelDir ".")
      $(mkRelFile "AsPatternAlias.juvix")
      $ \case
        ErrAliasBinderPattern {} -> Nothing
        _ -> wrongError,
    NegTest
      "Nested As-Patterns"
      $(mkRelDir ".")
      $(mkRelFile "NestedAsPatterns.juvix")
      $ \case
        ErrDoubleBinderPattern {} -> Nothing
        _ -> wrongError,
    NegTest
      "Pattern matching an implicit argument on the left of an application"
      $(mkRelDir ".")
      $(mkRelFile "ImplicitPatternLeftApplication.juvix")
      $ \case
        ErrImplicitPatternLeftApplication {} -> Nothing
        _ -> wrongError,
    NegTest
      "Constructor expected on the left of a pattern application"
      $(mkRelDir ".")
      $(mkRelFile "ConstructorExpectedLeftApplication.juvix")
      $ \case
        ErrConstructorExpectedLeftApplication {} -> Nothing
        _ -> wrongError,
    NegTest
      "Compile block for a unsupported kind of expression"
      $(mkRelDir "CompileBlocks")
      $(mkRelFile "WrongKindExpressionCompileBlock.juvix")
      $ \case
        ErrWrongKindExpressionCompileBlock {} -> Nothing
        _ -> wrongError,
    NegTest
      "A type parameter name occurs twice when declaring an inductive type"
      $(mkRelDir ".")
      $(mkRelFile "DuplicateInductiveParameterName.juvix")
      $ \case
        ErrDuplicateInductiveParameterName {} -> Nothing
        _ -> wrongError
  ]

filesErrorTests :: [NegTest ScoperError]
filesErrorTests =
  [ NegTest
      "A module that conflicts with a module in the stdlib"
      $(mkRelDir "StdlibConflict")
      $(mkRelFile "Input.juvix")
      $ \case
        ErrTopModulePath
          TopModulePathError {_topModulePathError = ErrDependencyConflict {}} -> Nothing
        _ -> wrongError,
    NegTest
      "Importing a module that conflicts with a module in the stdlib"
      $(mkRelDir "StdlibConflict")
      $(mkRelFile "Input.juvix")
      $ \case
        ErrTopModulePath
          TopModulePathError {_topModulePathError = ErrDependencyConflict {}} -> Nothing
        _ -> wrongError
  ]
