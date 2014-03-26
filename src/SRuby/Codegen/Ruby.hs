{-# LANGUAGE OverloadedStrings #-}
module SRuby.Codegen.Ruby where

import Control.Applicative
import Control.Monad.Trans.State.Strict
import Control.Monad.Writer
import Data.Functor
import Data.List
import Data.Monoid
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as TB

import SRuby.Parser.Types

type Codegen a = StateT Int (Writer Builder) a

generateRuby :: (RubyGen a) => a -> T.Text
generateRuby v = dropNewlines $ TB.toLazyText $ snd $ runWriter $ evalStateT (generate v) 0
  where dropNewlines = snd . T.span (== '\n')

output :: Text -> Codegen ()
output = tell . TB.fromText . T.toStrict

class RubyGen a where
  asBuilder :: a -> Codegen Builder

  generate :: a -> Codegen ()
  generate v = asBuilder v >>= tell

indent :: Codegen a -> Codegen a
indent action = do
  modify (1+)
  result <- action
  modify (1-)
  return result

-- Basic Combinators for Gen
text :: T.Text -> Codegen Builder
text t = return $ TB.fromText $ T.toStrict t

linestart :: Text -> Codegen Builder
linestart t = do
  identation <- get
  let space = T.concat $ replicate identation "  "
  return $ TB.fromText $ T.toStrict $ T.fromChunks [T.toStrict space, T.toStrict t]

newline :: Builder
newline = TB.singleton '\n'

end :: Builder
end = TB.fromText "end"

sepBy :: Builder -> [Builder] -> Builder
sepBy sep bs = concatBuilders $ intersperse sep bs

sepEndBy :: Builder -> [Builder] -> Builder
sepEndBy sep [] = TB.fromText ""
sepEndBy sep bs = sepBy sep bs `mappend` sep

parens :: Builder -> Builder
parens b = TB.singleton '(' `mappend` (b `mappend` (TB.singleton ')'))

instance RubyGen Program where
  asBuilder (Program classes methods statements) = do
      cs <- sepEndBy (newline `mappend` newline) <$> mapM asBuilder classes
      ms <- sepEndBy newline <$> mapM asBuilder methods
      ss <- sepBy newline <$> mapM asBuilder statements
      pure $ concatBuilders [cs, ms, ss]

instance RubyGen Class where
  asBuilder (Class name _ ms) = do
      cl <- linestart "class "
      className <- asBuilder name
      methods <- indent (sepEndBy newline <$> mapM asBuilder ms)
      return $ concatBuilders [cl, className, newline, methods, end]

instance RubyGen Method where
  asBuilder (Method name _ params _ body) = do
      def <- linestart "def "
      end <- linestart "end"
      methodName <- asBuilder name
      paramNames <- mapM asBuilder (fst $ unzip params)
      let paramList = ternary (null paramNames)
                              (TB.fromText "")
                              (parens $ sepBy ", " paramNames)
      -- body?
      return $ concatBuilders [def, methodName, paramList, newline, end]
    where ternary cond t f = if cond then t else f

instance RubyGen Statement where
  asBuilder (Exp e) = asBuilder e

instance RubyGen Expression where
  asBuilder (Val v) = asBuilder v
  asBuilder _ = error "unimplemented case for exp"

instance RubyGen Value where
  asBuilder (Fixnum v) = text (T.pack (show v))

instance RubyGen Id where
  asBuilder (Id name spos) = text $ T.pack name

concatBuilders :: [Builder] -> Builder
concatBuilders [] = TB.fromText ""
concatBuilders (b:bs) = b `mappend` (foldMR bs)
  where foldMR [] = TB.fromText ""
        foldMR (b:bs) = b `mappend` (foldMR bs)
