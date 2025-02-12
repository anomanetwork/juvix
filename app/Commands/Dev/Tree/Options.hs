module Commands.Dev.Tree.Options where

import Commands.Dev.Tree.Compile.Options
import Commands.Dev.Tree.Eval.Options
import Commands.Dev.Tree.FromAsm.Options
import Commands.Dev.Tree.Read.Options
import Commands.Dev.Tree.Repl.Options
import CommonOptions

data TreeCommand
  = Eval TreeEvalOptions
  | Compile CompileCommand
  | Read TreeReadOptions
  | FromAsm TreeFromAsmOptions
  | Repl TreeReplOptions
  deriving stock (Data)

parseTreeCommand :: Parser TreeCommand
parseTreeCommand =
  hsubparser $
    mconcat
      [ commandRepl,
        commandEval,
        commandCompile,
        commandRead,
        commandFromAsm
      ]
  where
    commandRepl :: Mod CommandFields TreeCommand
    commandRepl = command "repl" replInfo
      where
        replInfo :: ParserInfo TreeCommand
        replInfo =
          info
            (Repl <$> parseTreeReplOptions)
            (progDesc "Launch the JuvixTree REPL")

    commandEval :: Mod CommandFields TreeCommand
    commandEval = command "eval" evalInfo
      where
        evalInfo :: ParserInfo TreeCommand
        evalInfo =
          info
            (Eval <$> parseTreeEvalOptions)
            (progDesc "Evaluate a JuvixTree file")

    commandCompile :: Mod CommandFields TreeCommand
    commandCompile = command "compile" compileInfo
      where
        compileInfo :: ParserInfo TreeCommand
        compileInfo =
          info
            (Compile <$> parseCompileCommand)
            (progDesc "Compile a JuvixTree file")

    commandRead :: Mod CommandFields TreeCommand
    commandRead = command "read" readInfo
      where
        readInfo :: ParserInfo TreeCommand
        readInfo =
          info
            (Read <$> parseTreeReadOptions)
            (progDesc "Parse a JuvixTree file and pretty print it")

    commandFromAsm :: Mod CommandFields TreeCommand
    commandFromAsm = command "from-asm" fromAsmInfo
      where
        fromAsmInfo :: ParserInfo TreeCommand
        fromAsmInfo =
          info
            (FromAsm <$> parseTreeFromAsmOptions)
            (progDesc "Convert a JuvixAsm file to JuvixTree and pretty print it")
