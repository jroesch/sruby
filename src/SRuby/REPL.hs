module SRuby.REPL (repl) where

import qualified Data.Text.Lazy as T
import SRuby.Interpreter
import SRuby.Parser.Core (program)
import SRuby.Codegen.Ruby (generateRuby)
import Control.Monad (forever)
import Control.Monad.Trans (lift)
import System.Console.Haskeline
import System.Process
import System.Environment
import System.IO

import SRuby.Typecheck

{- tryCheck :: IO ()
tryCheck =
  print $ cofreeMu (Value (Fixnum 1))
  print . attribute $ cofreeMu example
  print . typeTree $ cofreeMu example -}

repl :: IO ()
repl = runInputT defaultSettings $ do
         (irbin, irbout, irberr, _) <- lift $ createIRB
         forever $ do
           minput <- getInputLine "srbi> "
           case minput of
             Nothing -> return ()
             Just ":quit" ->
               return ()
             Just input -> do
               let staticRuby = runParserWithInput program "sirb" input
               let compiledRuby = T.unpack $ generateRuby $ typeProgram $ staticRuby
               -- lift $ putStrLn $ "Compiled: \n" ++ compiledRuby
               lift $ hPutStrLn irbin compiledRuby
               echo <- lift $ hGetLine {- readUntilResult -} irbout
               result <- lift $ hGetLine irbout
               outputStrLn result

{- readUntilResult :: Handle -> IO [String]
readUntilResult handle = do
    ln <- hGetLine handle
    if isResult ln
      then return [ln]
      else do
        rest <- readUntilResult handle
        return $ (ln:rest)
  where isResult ('=':'>':_) = True
        isResult _ = False -}

createIRB :: IO (Handle, Handle, Handle, ProcessHandle)
createIRB = do
    result @ (inn, out, error, proc) <- runInteractiveCommand "irb"
    hSetBinaryMode inn False
    hSetBinaryMode out False
    hSetBuffering inn LineBuffering
    hSetBuffering out NoBuffering
    return result
