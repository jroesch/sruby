{-# LANGUAGE DataKinds #-}
module SRuby.Parser.Core where

import Control.Applicative ((*>), (<*), (<*>), (<$>))
import Control.Comonad.Cofree
import Control.Lens
import Control.Monad.State as S

import Data.List (intercalate)
import Data.Functor.Identity
import Data.Maybe (fromMaybe)
import qualified Data.Map as M
import Text.Parsec hiding (satisfy)
import System.Environment

import SRuby.Lexer
import SRuby.Parser.Prim
import SRuby.Parser.Types as T hiding (TFixnum)

program :: Parser (Program Unchecked)
program = do
    clss <- many (classP <* delim)
    ms <- many (method <* delim)
    body <- statement
    return $ Program clss ms body

classP :: Parser (Class Unchecked)
classP = do
    match TClass
    name <- identifier
    vars <- fromMaybe [] <$> optionMaybe tyvars <* delim
    ms <- methods
    match TEnd
    return $ Class name vars ms
  where methods = method `sepEndBy` delim

tyvars :: Parser [TyParam]
tyvars = do
  match TLSquare
  types <- (TyParam <$> identifier) `sepBy1` (match TComma)
  match TRSquare
  return types

typeP :: Parser Type
typeP = do
  name <- identifier
  return $ TName name

method :: Parser (Method Unchecked)
method = do
  match TDef
  methodName <- identifier
  mvars <- optionMaybe tyvars
  let tvars = case mvars of
               Nothing -> []
               Just v -> v
  mparams <- (optionMaybe paramList) <* delim
  let params = case mparams of
                Nothing -> []
                Just ps -> ps
  body <- statement
  let rtype = undefined
  match TEnd
  return $ Method methodName tvars params rtype body

paramList :: Parser [(Id, Type)]
paramList = parens $ param `sepBy` (match TComma)
    where param = do
            name <- identifier
            mtpe <- optionMaybe $ match TColon *> typename
            case mtpe of
              Nothing  -> return $ (name, TDynamic)
              Just tpe -> return $ (name,  TName tpe)

typename = identifier

statement :: Parser (Cofree Statement ())
statement = undefined {- assignment
         <|> (Exp . Val) <$> value -}

assignment :: Parser (Cofree Statement ())
assignment = undefined {- do
    r <- ref
    match TEq
    exp <- expression
    return $ Exp $ Assign r exp -}

ref :: Parser Ref
ref = Var <$> identifier
   <|> IVar <$> (match TAt >> identifier)
   <|> Self <$> (match TSelf >> identifier)
   <|> path
 where path = Path <$> identifier <*> ref

expression :: Cofree Expression ()
expression = undefined {- Val <$> value <|> Access <$> ref -}

value :: Parser Value
value = Fixnum <$> fixnum

identifier :: Parser Id
identifier = satisfy $ \(t, sp) ->
  case t of
    TIdent s -> Just $ Id s sp
    _        -> Nothing

fixnum :: Parser Integer
fixnum = satisfy $ \(t, _) ->
  case t of
    TFixnum n -> Just $ fromIntegral $ n
    _         -> Nothing
