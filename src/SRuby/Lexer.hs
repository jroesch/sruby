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

data IToken = TDelim
            | TIdent String
            | TFixnum Int
            | TString String
            | TCase
            | TOf
            | TLet
            | TIn
            | TSyntax
            | TEq
            | TFatArr
            | TDollar
            | TLParen
            | TRParen
            | TLAng
            | TRAng
            | TLCurly
            | TRCurly
            | TComma
            | TColon
            | TPeriod
            | TForeign
            | TEOF
            deriving (Eq, Show)

type Token = (IToken, SourcePos)

tokenize :: String -> [Token]
tokenize input = case runParser tokens () "Tokenizer" input of
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
    where pos = newPos "FOOBAR" 0 0

tokens = (many $ hSpace *> (tagToken tokens')) <* eof
    where tokens' =  keyword 
                 <|> ident 
                 <|> delim 
                 <|> paren
                 <|> angular
                 <|> curly
                 <|> comma
                 <|> colon
                 <|> period
                 <|> (try fatArrow)
                 <|> eq
                 <|> dollar
                 <|> fixnum

hSpace :: TParser ()
hSpace = skipMany $ TP.satisfy (\c -> c == ' ' || c == '\t')

keyword :: TParser IToken
keyword = try $ caseT <|> ofT <|> letT <|> inT <|> syntax <|> foreignK
  where caseT    = fmap (\_ -> TCase) $ string "case"
        ofT      = fmap (\_ -> TOf)   $ string "of"
        letT     = fmap (\_ -> TLet)  $ string "let"
        inT      = fmap (\_ -> TIn)   $ string "in"
        foreignK = fmap (\_ -> TForeign) $ string "foreign"
        syntax   = fmap (\_ -> TSyntax)  $ string "syntax"

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

angular :: TParser IToken
angular = lang <|> rang
    where lang = (\_ -> TLAng) <$> char '<'
          rang = (\_ -> TRAng) <$> char '>'

curly :: TParser IToken
curly = lcurly <|> rcurly
    where lcurly = (\_ -> TLCurly) <$> char '{'
          rcurly = (\_ -> TRCurly) <$> char '}'

comma :: TParser IToken
comma = (\_ -> TComma) <$> char ','

colon :: TParser IToken
colon = (\_ -> TColon) <$> char ':'

period :: TParser IToken
period = (\_ -> TPeriod) <$> char '.'

eq :: TParser IToken
eq = (\x -> TEq) <$> char '='

fatArrow :: TParser IToken
fatArrow = (\x -> TFatArr) <$> string "=>"

dollar :: TParser IToken
dollar = (\x -> TDollar) <$> char '$'

fixnum :: TParser IToken
fixnum = TFixnum . read <$> (many1 $ digit)
