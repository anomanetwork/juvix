module Main (main) where

import Anoma qualified
import Asm qualified
import BackendMarkdown qualified
import Base
import Casm qualified
import Compilation qualified
import Core qualified
import Examples qualified
import Format qualified
import Formatter qualified
import Internal qualified
import Isabelle qualified
import Juvix.Config qualified as Config
import Nockma qualified
import Package qualified
import Parsing qualified
import Reg qualified
import Repl qualified
import Resolver qualified
import Runtime qualified
import Rust qualified
import Scope qualified
import Termination qualified
import Tree qualified
import Typecheck qualified

slowTests :: IO TestTree
slowTests =
  sequentialTestGroup
    "Juvix slow tests"
    AllFinish
    <$> sequence
      [ return Runtime.allTests,
        return Reg.allTests,
        return Asm.allTests,
        return Tree.allTests,
        return Core.allTests,
        return Internal.allTests,
        return Compilation.allTests,
        return Examples.allTests,
        Casm.allTests,
        return Anoma.allTests,
        return Repl.allTests
      ]
      <> sequence (if Config.config ^. Config.configRust then [Rust.allTests] else [])

fastTests :: IO TestTree
fastTests =
  return $
    testGroup
      "Juvix fast tests"
      [ Parsing.allTests,
        Resolver.allTests,
        Scope.allTests,
        Termination.allTests,
        Typecheck.allTests,
        Format.allTests,
        Formatter.allTests,
        Package.allTests,
        BackendMarkdown.allTests,
        Isabelle.allTests,
        Nockma.allTests
      ]

main :: IO ()
main = do
  tests <- sequence [fastTests, slowTests]
  defaultMain (testGroup "Juvix tests" tests)
