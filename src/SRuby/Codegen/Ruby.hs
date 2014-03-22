{-# LANGUAGE OverloadedStrings #-}
module SRuby.Codegen.Ruby where

import Control.Monad.Trans.State.Strict
import Data.Monoid
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as TB

import SRuby.Parser.Types

type Codegen a = State Int a

generateRuby :: (RubyGen a) => a -> T.Text
generateRuby v = TB.toLazyText $ evalState (generate v) 0

class RubyGen a where
  generate :: a -> Codegen Builder

indent :: Codegen a -> Codegen a
indent action = do
  modify (1+)
  result <- action
  modify (1-)
  return result

text :: T.Text -> Codegen Builder
text t = do
  identation <- get
  let space = T.concat $ replicate identation "  "
  return $ TB.fromText $ T.toStrict $ T.fromChunks [T.toStrict space, T.toStrict t]

instance RubyGen Program where
  generate (Program classes methods statements) = do
    cs <- mapM generate classes
    ms <- mapM generate methods
    ss <- mapM generate statements
    return $ concatBuilders $ concat [cs, ms, ss]

instance RubyGen Class where
  generate (Class name methods) = do
      className <- generate name
      meths <- indent $ do
        ms <- mapM generate methods
        return $ concatBuilders ms
      return $ classDecl className meths
    where classDecl name methods = concatBuilders [
            TB.fromText "class ",
            name,
            TB.fromText "\n",
            methods,
            TB.fromText "\n",
            TB.fromText "end\n\n"]

instance RubyGen Method where
  generate _ = text "foobar"

instance RubyGen Statement where
  generate _ = text "foobar"

instance RubyGen Id where
  generate (Id name spos) = text $ T.pack name

concatBuilders :: [Builder] -> Builder
concatBuilders [] = TB.fromText ""
concatBuilders (b:bs) = b `mappend` (foldMR bs)
  where foldMR [] = TB.fromText ""
        foldMR (b:bs) = b `mappend` (foldMR bs)
