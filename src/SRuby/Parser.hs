{-# LANGUAGE FlexibleContexts #-}
module SRuby.Parser where

import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import qualified Data.Map as M
import Text.Parsec hiding (satisfy)
import Control.Applicative ((*>), (<*), (<*>), (<$>))
import Control.Monad.State as S
import System.Environment

import SRuby.Lexer

type Parser = Parsec [Token] Int

data Method = Method String [Var] MethodBody deriving (Show, Eq)

data Type = TyName String 
          | TyVar String
          deriving (Eq, Ord, Show)

data Statement = Decl Var Value deriving (Eq, Show)

data Var = Var String Type deriving (Eq, Show)

type MethodBody = [Statement]

data Value = Nat Int
           | Str String
           deriving (Eq, Show)

satisfy :: Stream [Token] m Token => (Token -> Maybe a) -> ParsecT [Token] u m a
satisfy = tokenPrim showT nextPos
  where
    showT = 
      show
    nextPos _ (t, pos) _ = pos

match :: IToken -> Parser IToken
match t = satisfy (\(x, _) -> if t == x then Just t else Nothing)

interpret = interpretWith method

interpretWith p = print . (testParser p)

testParser p input = 
    let tokens = tokenize input
        in case runParser p 0 "REPL" tokens of
              Left e -> error $ show e
              Right v -> {- infer (newUnionFind (builtinTypes)) -} v

delim :: Parser ()
delim = match TDelim >> return ()

parens :: Parser a -> Parser a
parens p = do
    match TLParen
    r <- p
    match TRParen
    return r

method :: Parser Method
method = do
    match TDef
    name <- identifier
    args <- parens mArgList <* delim
    body <- methodBody
    match TEnd <* delim
    return $ Method name args body

identifier :: Parser String
identifier = satisfy $ \(t, _) ->
  case t of
    TIdent s -> Just s
    _        -> Nothing
      
methodBody :: Parser MethodBody
methodBody = statement `endBy` delim

-- mArgList :: Parser String
mArgList = do
    params <- sepBy identifier (match TComma)
    forM params $ \p -> do
      tyvar <- freshTyVar
      return $ Var p tyvar 

statement :: Parser Statement
statement = do
  v <- identifier
  tyvar <- freshTyVar
  match TEq
  i <- fixnum
  return $ Decl (Var v tyvar) (Nat i)

fixnum :: Parser Int
fixnum = satisfy $ \(t, _) ->
  case t of
    TFixnum n -> Just n
    _         -> Nothing

freshTyVar :: Parser Type
freshTyVar = do
    n <- getState
    putState (n + 1)
    return $ TyVar $ "t" ++ (show n)

