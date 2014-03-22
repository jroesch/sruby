{-# LANGUAGE FlexibleContexts #-}
module SRuby.Parser.Prim
  ( satisfy,
    match,
    delim,
    parens
  ) where

import Text.Parsec hiding (satisfy)
import Text.Parsec.Prim (ParsecT(..))

import Control.Applicative ((*>), (<*), (<*>), (<$>))
import Control.Lens
import Control.Lens.Setter ((%=))

import Data.Functor.Identity

import SRuby.Lexer
import SRuby.Parser.Types

satisfy :: Stream [Token] m Token => (Token -> Maybe a) -> ParsecT [Token] u m a
satisfy = tokenPrim showT nextPos
  where
    showT =
      show
    nextPos _ (t, pos) _ = pos

match :: IToken -> Parser IToken
match t = satisfy (\(x, _) -> if t == x then Just t else Nothing)

delim :: Parser ()
delim = (many1 $ match TDelim) >> return ()

parens :: Parser a -> Parser a
parens p = do
    match TLParen
    r <- p
    match TRParen
    return r
