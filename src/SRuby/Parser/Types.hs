{-# LANGUAGE TemplateHaskell, FlexibleInstances, MultiParamTypeClasses, OverlappingInstances #-}
module SRuby.Parser.Types where

import Control.Lens
import Control.Monad.State.Class (MonadState(..))
import Control.Monad.Reader.Class (MonadReader(..))
import Text.Parsec (Parsec, ParsecT)
import Text.Parsec.Prim (getState, putState)
import Text.Parsec.Pos (SourcePos)

import SRuby.Lexer (Token)

type Parser = Parsec [Token] ParserState

data ParserState = ParserState {
    _tyvarL      :: Int
} deriving (Eq, Show)

instance (Monad m) => MonadState ParserState (ParsecT [Token] ParserState m) where
    get = getState
    put = putState

instance (Monad m) => MonadReader ParserState (ParsecT [Token] ParserState m) where
    ask       = getState

    local f m = do
      rs <- ask
      putState $ f rs
      result <- m
      putState rs
      return result

data Program = Program {
    _classes :: [Class],
    _topLevelMethods :: [Method],
    _statements :: [Statement]
} deriving (Show)

data Class = Class {
    _className :: Id,
    _classTyvars :: [TyVar],
    _methods :: [Method]
} deriving (Show)

data Method = Method {
    _methodName :: Id,
    _methodTyvars :: [TyVar],
    _params :: [(Id, Type)],
    _rettype :: Type,
    _body :: [Statement]
} deriving (Show)

data Statement = Exp Expression deriving (Show)

data Expression = Access Ref
                | Assign Ref Expression
                | Val Value
                deriving (Show)

data Ref = IVar Id
         | Var Id
         | Self Id
         | Path Id Ref
         deriving (Eq, Show)
         -- | Path Id

data Value = Fixnum Integer
           deriving (Eq, Show)


data Type = Name Id
          | Dynamic
          deriving (Eq, Show)

data TyVar = TyVar Id deriving (Eq, Show)

data Id = Id String SourcePos deriving (Eq, Show)

-- Lens TH Decls are order sensitive
makeLenses ''ParserState
