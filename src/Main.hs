module Main where
import Idris.AbsSyntax
import Idris.Core.TT
import Idris.ElabDecls
import Idris.Main
import Idris.ModeCommon
import Idris.REPL
import Idris.Options
import IRTS.Simplified

import IRTS.Compiler
import IRTS.CodegenPython
import IRTS.CodegenCommon

import System.Environment
import System.Exit
import Control.Monad

import Paths_idris_python

data Opts = Opts { inputs :: [FilePath],
                   output :: FilePath }

showUsage = do putStrLn "A code generator which is intended to be called by the compiler, not by a user."
               putStrLn "Usage: idris-codegen-python <ibc-files> [-o <output-file>]"
               exitSuccess

getOpts :: IO Opts
getOpts = process (Opts []  "a.out") <$> getArgs
  where
    process opts ("-o":o:xs) = process (opts { output = o }) xs
    process opts (x:xs) = process (opts { inputs = x:inputs opts }) xs
    process opts [] = opts

c_main :: Opts -> Idris ()
c_main opts = do elabPrims
                 loadInputs (inputs opts) Nothing
                 mainProg <- fmap Just elabMain
                 ir <- compile (Via IBCFormat "python") (output opts) mainProg
                 runIO $ forM_ (fmap (show . snd) (defunDecls  ir)) putStrLn
                 runIO $ codegenPython ir

main :: IO ()
main = do opts <- getOpts
          if null (inputs opts)
             then showUsage
             else  runMain (c_main opts)