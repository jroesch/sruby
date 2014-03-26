{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DataKinds #-}
-- Adapted from Brian McKenna's approach described here:
-- https://gist.github.com/puffnfresh/5940556
module SRuby.Typecheck (typeProgram) where

import Prelude hiding (sequence)
import Control.Comonad
import Control.Comonad.Cofree
import Control.Monad.State hiding (sequence)
import Data.Foldable (Foldable, fold)
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Traversable (Traversable, sequence)
import qualified Data.Map as M

import SRuby.Parser.Types

data Constraint = EqualityConstraint Type Type deriving (Show)

data TypeResult = TypeResult {
  constraints :: [Constraint],
  assumptions :: M.Map String [Type]
} deriving (Show)

instance Monoid TypeResult where
  mempty = TypeResult { constraints = mempty, assumptions = mempty }
  mappend a b = TypeResult {
    constraints = constraints a `mappend` constraints b,
    assumptions = assumptions a `mappend` assumptions b
  }

data TyChkState t m = TyChkState {
    varId :: Int,
    memo :: M.Map t m
}

type TypeCheck t = State (TyChkState t (Type, TypeResult)) (Type, TypeResult)

freshTyVar :: State (TyChkState t m) Type
freshTyVar = do
  v <- gets varId
  modify $ \s -> s { varId = succ v }
  return $ TyVar v

memoizedTC :: Ord c => (c -> TypeCheck c) -> c -> TypeCheck c
memoizedTC f c = gets memo >>= maybe memoize return . M.lookup c
  where memoize = do
          r <- f c
          modify $ \s -> s { memo = M.insert c r $ memo s }
          return r

attribute :: Cofree Statement () -> Cofree Statement (Type, TypeResult)
attribute c =
    let initial = TyChkState { memo = M.empty, varId = 0 }
    in evalState (sequence $ extend (memoizedTC generateConstraints) c) initial

generateConstraints :: Cofree Statement () -> TypeCheck (Cofree Statement ())
generateConstraints (() :< Exp e) = undefined
generateConstraints _ = error "Jared needs to implement this"

solveConstraints :: [Constraint] -> Maybe (M.Map Int Type)
solveConstraints =
    foldl (\b a -> liftM2 mappend (solve b a) b) $ Just M.empty
          where solve maybeSubs (EqualityConstraint a b) = do
                  subs <- maybeSubs
                  mostGeneralUnifier (substitute subs a) (substitute subs b)

mostGeneralUnifier :: Type -> Type -> Maybe (M.Map Int Type)
mostGeneralUnifier = error "implement unification"

substitute :: M.Map Int Type -> Type -> Type
substitute subs v@(TyVar i) = maybe v (substitute subs) $ M.lookup i subs
-- substitute subs (TLambda a b) = TLambda (substitute subs a) (substitute subs b)
substitute _ t = t

typeStatement :: Cofree Statement () -> Maybe (Cofree Statement Type)
typeStatement c =
    let result = attribute c
        (r :< _) = result
        maybeSubs = solveConstraints . constraints $ snd r
    in fmap (\subs -> fmap (substitute subs . fst) result) maybeSubs

typeMethod :: Method Unchecked -> Method Checked
typeMethod = undefined

typeClass :: Class Unchecked -> Class Checked
typeClass = undefined

-- not dealing with classes yet
-- not dealing with methods yet
typeProgram :: Program Unchecked -> Program Checked
typeProgram (Program classes methods stms) = undefined
