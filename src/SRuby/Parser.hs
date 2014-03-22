{-# LANGUAGE FlexibleContexts #-}
module SRuby.Parser where

import Text.Parsec hiding (satisfy)

import Control.Applicative ((<*))
import SRuby.Parser.Core
import SRuby.Parser.Types

programParser :: Parser Module
programParser = moduleP <* ceof

replParser :: Parser Term
replParser = term
