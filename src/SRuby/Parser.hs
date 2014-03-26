{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
module SRuby.Parser where

import Text.Parsec hiding (satisfy)

import Control.Applicative ((<*))
import SRuby.Parser.Core
import SRuby.Parser.Types

programParser :: Parser (Program Unchecked)
programParser = program

replParser :: Parser (Program Unchecked)
replParser = program
