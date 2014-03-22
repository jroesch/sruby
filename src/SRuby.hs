module Main where

import Control.Monad (forever)
import Control.Monad.Trans (lift)
import System.Console.Haskeline
import System.Environment

import SRuby.Interpreter

main :: IO ()
main = do
    args <- getArgs
    case (args !! 0) of
        "-f" -> do
            contents <- readFile (args !! 1)
            result <- interpretProgram (args !! 1) contents
            print result
        "-i" -> repl

repl :: IO ()
repl = runInputT defaultSettings $ forever $ do
            minput <- getInputLine "srbi> "
            case minput of
                Nothing -> return ()
                Just ":quit" ->
                  return ()
                Just input -> do
                  r <- lift $ interpretTerm "Interactive" input
                  outputStrLn $ show r
