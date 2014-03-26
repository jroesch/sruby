{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module SRuby.Parser.Types where

import Data.Foldable
import Control.Lens
import Control.Monad.State.Class (MonadState(..))
import Control.Monad.Reader.Class (MonadReader(..))
import Control.Comonad.Cofree
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

data Typecheck = Unchecked | Checked

type family CofreeType (c :: Typecheck)

type instance CofreeType Unchecked = ()
type instance CofreeType Checked = Type

data Program (c :: Typecheck) = Program {
    _classes :: [Class c],
    _topLevelMethods :: [Method c],
    _statement :: Cofree Statement (CofreeType c)
}

deriving instance (CofreeType c ~ checked, Show checked) => Show (Program c)

data Class (c :: Typecheck) = Class {
    _className :: Id,
    _classTyvars :: [TyParam],
    _methods :: [Method c]
}

deriving instance (CofreeType c ~ checked, Show checked) => Show (Class c)

data Method (c :: Typecheck) = Method {
    _methodName :: Id,
    _methodTyvars :: [TyParam],
    _params :: [(Id, Type)],
    _rettype :: Type,
    _body :: Cofree Statement (CofreeType c)
}

deriving instance (CofreeType c ~ checked, Show checked) => Show (Method c)

data Statement a = Exp (Cofree Expression ())
                 | Seq a (Statement a)
                 | Empty

deriving instance Show a => Show (Statement a)
deriving instance Functor Statement
deriving instance Foldable Statement
deriving instance Traversable Statement
deriving instance Eq a => Eq (Statement a)
deriving instance Ord a => Ord (Statement a)

data Expression a = Access Ref
                  | Assign Ref a
                  | Val Value
                  | BinOp Op a a
                  -- | Apply

deriving instance Show a => Show (Expression a)
deriving instance Functor Expression
deriving instance Foldable Expression
deriving instance Traversable Expression
deriving instance Eq a => Eq (Expression a)
deriving instance Ord a => Ord (Expression a)

data Ref = IVar Id
         | Var Id
         | Self Id
         | Path Id Ref
         deriving (Eq, Ord, Show)
         -- | Path Id

data Op = Plus | Minus deriving (Eq, Ord, Show)

data Value = Fixnum Integer
           deriving (Eq, Ord, Show)

data Type = TName Id
          | TyVar Int
          | TFixnum
          | TDynamic
          deriving (Eq, Show)

data TyParam = TyParam Id deriving (Eq, Show)

data Id = Id String SourcePos deriving (Eq, Ord, Show)

-- Lens TH Decls are order sensitive
makeLenses ''ParserState
