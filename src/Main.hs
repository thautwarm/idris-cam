module Main where
import Idris.AbsSyntax
import Idris.Core.TT
import Idris.ElabDecls
import Idris.Main
import Idris.ModeCommon
import Idris.REPL
import Idris.Options

import IRTS.Compiler
import IRTS.CodegenPython

import System.Environment
import System.Exit
import Control.Monad


import Paths_idris_python

data Opts = Opts { inputs :: [FilePath],
                   interface :: Bool,
                   output :: FilePath }

showUsage = do putStrLn "A code generator which is intended to be called by the compiler, not by a user."
               putStrLn "Usage: idris-python <ibc-files> [-o <output-file>]"
               exitWith ExitSuccess

getOpts :: IO Opts
getOpts = do xs <- getArgs
             return $ process (Opts [] False "a.out") xs
  where
    process opts ("-o":o:xs) = process (opts { output = o }) xs
    process opts ("--interface":xs) = process (opts { interface = True }) xs
    process opts (x:xs) = process (opts { inputs = x:inputs opts }) xs
    process opts [] = opts

c_main :: Opts -> Idris ()
c_main opts = do elabPrims
                 loadInputs (inputs opts) Nothing
                 mainProg <- if interface opts
                                then liftM Just elabMain
                                else return Nothing
                 ir <- compile (Via IBCFormat "python") (output opts) mainProg
                 runIO $ codegenPython ir

main :: IO ()
main = do opts <- getOpts
          if (null (inputs opts))
             then showUsage
             else  runMain (c_main opts)