module SRuby.Parser.Core where

import Control.Applicative ((*>), (<*), (<*>), (<$>))
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
import SRuby.Parser.Types

program :: Parser Program
program = do
    clss <- many (classP <* delim)
    ms <- many (method <* delim)
    statements <- many (statement)
    return $ Program clss ms statements

classP :: Parser Class
classP = do
    match TClass
    name <- identifier
    mvars <- optionMaybe tyvars <* delim
    let vars = case mvars of
                 Nothing -> []
                 Just v -> v
    ms <- methods
    match TEnd
    return $ Class name vars ms
  where methods = method `sepEndBy` delim

tyvars :: Parser [TyVar]
tyvars = do
  match TLSquare
  types <- (TyVar <$> identifier) `sepBy1` (match TComma)
  match TRSquare
  return types

typeP :: Parser Type
typeP = do
  name <- identifier
  return $ Name name

method :: Parser Method
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
  statements <- assignment `sepEndBy` delim
  let rtype = undefined
  match TEnd
  return $ Method methodName tvars params rtype statements

paramList :: Parser [(Id, Type)]
paramList = parens $ param `sepBy` (match TComma)
    where param = do
            name <- identifier
            mtpe <- optionMaybe $ match TColon *> typename
            case mtpe of
              Nothing  -> return $ (name, Dynamic)
              Just tpe -> return $ (name,  Name tpe)

typename = identifier

statement :: Parser Statement
statement = assignment
         <|> (Exp . Val) <$> value

assignment :: Parser Statement
assignment = do
    r <- ref
    match TEq
    exp <- expression
    return $ Exp $ Assign r exp

ref :: Parser Ref
ref = Var <$> identifier
   <|> IVar <$> (match TAt >> identifier)
   <|> Self <$> (match TSelf >> identifier)
   <|> path
 where path = Path <$> identifier <*> ref

expression = Val <$> value
          <|> Access <$> ref

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
