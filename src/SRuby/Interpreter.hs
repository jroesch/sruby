module SRuby.Interpreter where

import Text.Parsec (runParser)

import SRuby.Lexer
import SRuby.Parser
-- import SRuby.Parser.Core (coreDecl, coreTerm, coreIdentifier)
import SRuby.Parser.Types (ParserState(..))
-- import SRuby.Inference
-- import SRuby.Util.UnionFind (newUnionFind)
import SRuby.Codegen.Ruby (generateRuby)
import SRuby.Typecheck
import qualified Data.Text.Lazy as T

import Debug.Trace

interpretProgram = interpretWith programParser

interpretTerm = interpretWith replParser

interpretWith p f = putStrLn . T.unpack . generateRuby . typeProgram . (runParserWithInput p f)

runParserWithInput p fname input =
    let tokens = filter notTComment (tokenize fname input)
        in case runParser p init fname tokens of
              Left e -> error $ show e
              Right v -> {- check builtinTypes -} v
    where init  = ParserState 0
          notTComment ((TComment _), _) = False
          notTComment _ = True
