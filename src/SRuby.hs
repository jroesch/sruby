module Main where

import Control.Monad (forever)
import Control.Monad.Trans (lift)
import System.Environment
import SRuby.REPL
import SRuby.Interpreter

main :: IO ()
main = do
    args <- getArgs
    case (args !! 0) of
        "-i"  -> repl
        fname -> do
            contents <- readFile (args !! 0)
            interpretProgram (args !! 0) contents
