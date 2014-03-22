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
import Text.Parsec.Pos (newPos)
import Text.Parsec.String -- hiding (Parser)
--import Text.Parsec.Language
import Control.Applicative ((*>), (<*), (<*>), (<$>))
import Control.Monad.State as S
import System.Environment

data IToken = TIdent String
            | TSelf
            | TClass
            | TDef
            | TEnd
            | TDelim
            | TLParen
            | TRParen
            | TLSquare
            | TRSquare
            | TComma
            | TColon
            | TPeriod
            | TAt
            | TEq
            | TFixnum Int
            | TString String
            | TComment String
            | TEOF
            deriving (Eq, Show)

type Token = (IToken, SourcePos)

tokenize :: String -> String -> [Token]
tokenize filename input = case runParser tokens () filename input of
    Left  e  -> error $ show e
    Right ts -> ts ++ [teof]

type TParser = Parser

tagToken :: TParser IToken -> TParser Token
tagToken t = do
    pos <- getPosition
    r   <- t
    return $ (r, pos)

teof :: Token
teof = (TEOF, pos)
    where pos = newPos "EOF_TOKEN" 0 0

tokens = (many $ hSpace *> (tagToken tokens')) <* eof
    where tokens' =  keyword
                 <|> ident
                 <|> delim
                 <|> paren
                 <|> comma
                 <|> colon
                 <|> at
                 <|> eq
                 <|> period
                 <|> fixnum
                 <|> comment
                 <|> square
                 {- <|> angular
                 <|> curly
                 <|> colon
                 <|> period
                 <|> (try fatArrow)
                 <|> eq
                 <|> dollar
                 <|> fixnum -}

hSpace :: TParser ()
hSpace = skipMany $ TP.satisfy (\c -> c == ' ' || c == '\t')

keyword :: TParser IToken
keyword = try $ defT <|> endT <|> classT <|> selfT
  where defT   = const TDef <$> string "def"
        endT   = const TEnd <$> string "end"
        classT = const TClass <$> string "class"
        selfT  = const TSelf <$> string "self"

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
comma = const TComma <$> char ','

colon :: TParser IToken
colon = const TColon <$> char ':'

at :: TParser IToken
at = const TAt <$> char '@'

eq :: TParser IToken
eq = (\x -> TEq) <$> char '='

period :: TParser IToken
period = (\_ -> TPeriod) <$> char '.'

square :: TParser IToken
square = lsq <|> rsq
    where lsq = (\_ -> TLSquare) <$> char '['
          rsq = (\_ -> TRSquare) <$> char ']'
{- angular :: TParser IToken
angular = lang <|> rang
    where lang = (\_ -> TLAng) <$> char '<'
          rang = (\_ -> TRAng) <$> char '>'

curly :: TParser IToken
curly = lcurly <|> rcurly
    where lcurly = (\_ -> TLCurly) <$> char '{'
          rcurly = (\_ -> TRCurly) <$> char '}'

fatArrow :: TParser IToken
fatArrow = (\x -> TFatArr) <$> string "=>"

dollar :: TParser IToken
dollar = (\x -> TDollar) <$> char '$' -}

comment :: TParser IToken
comment = do
  char '#'
  text <- manyTill anyToken (char '\n')
  return $ TComment text

fixnum :: TParser IToken
fixnum = TFixnum . read <$> (many1 $ digit)
