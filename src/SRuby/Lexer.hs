module SRuby.Lexer
  ( tokenize,
    IToken (..),
    Token
  ) where

import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import qualified Data.Map as M
import Text.Parsec hiding (satisfy, tokens)
import qualified Text.Parsec as TP (satisfy)
import Text.Parsec.Prim (getPosition)
import Text.Parsec.String -- hiding (Parser)
--import Text.Parsec.Language
import Control.Applicative ((*>), (<*), (<*>), (<$>))
import Control.Monad.State as S
import System.Environment

data IToken = TDelim
            | TIdent String
            | TFixnum Int
            | TString String
            | TDef
            | TEq
            | TLParen
            | TRParen
            | TComma
            | TEnd
            | TForeign
            deriving (Eq, Show)

type Token = (IToken, SourcePos)

tokenize :: String -> [Token]
tokenize input = case runParser tokens () "Tokenizer" input of
    Left  e  -> error $ show e
    Right ts -> ts

type TParser = Parser

tagToken :: TParser IToken -> TParser Token
tagToken t = do
    pos <- getPosition
    r   <- t
    return $ (r, pos)

tokens = many $ hSpace *> (tagToken tokens')
    where tokens' =  keyword 
                 <|> ident 
                 <|> delim 
                 <|> paren
                 <|> comma
                 <|> eq
                 <|> fixnum

hSpace :: TParser ()
hSpace = skipMany $ TP.satisfy (\c -> c == ' ' || c == '\t')

keyword :: TParser IToken
keyword = try $ def <|> end <|> foreignK
  where def = fmap (\_ -> TDef) $ string "def"
        end = fmap (\_ -> TEnd) $ string "end"
        foreignK = fmap (\_ -> TForeign) $ string "foreign"

ident :: TParser IToken
ident = do
    x <- first
    xs <- many $ rest
    return $ TIdent (x:xs)
  where first = letter <|> char '_'
        rest  = first <|> digit

delim :: TParser IToken
delim = (\_ -> TDelim) <$> TP.satisfy (\c -> c == ';' || c == '\n')

paren :: TParser IToken
paren = lparen <|> rparen
    where lparen = (\_ -> TLParen) <$> char '('
          rparen = (\_ -> TRParen) <$> char ')'

comma :: TParser IToken
comma = (\_ -> TComma) <$> char ','

eq :: TParser IToken
eq = (\x -> TEq) <$> char '='

fixnum :: TParser IToken
fixnum = TFixnum . read <$> (many1 $ digit)
