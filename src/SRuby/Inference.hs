module SRuby.Inference
  ( TypeEnv,
    infer,
    check,
    builtinTypes
  ) where

import Control.Arrow ((&&&))
import Control.Monad
import Control.Monad.State as S
import Data.Function
import Data.Functor ((<$>))
import Data.List
import Data.Maybe (fromMaybe)
import qualified Data.Map as M

import SRuby.Parser
import SRuby.Parser.Types
import SRuby.Util.UnionFind as U

data TypeEnv = TypeEnv 
             { count :: Int
             , equality :: M.Map Type Int
             , envr :: M.Map String Type
             } deriving (Show)

type InferenceM a = S.State TypeEnv a

extendTyEnv :: String -> Type -> InferenceM ()
extendTyEnv n t = modify $ \(TypeEnv cnt eq ev) ->
  TypeEnv (cnt + 1) (M.insert t (cnt + 1) eq) (M.insert n t ev)

introduceTyVar :: Type -> InferenceM ()
introduceTyVar t = modify $ \(TypeEnv cnt eq ev) ->
  TypeEnv (cnt + 1) (M.insert t (cnt + 1) eq) ev

unify :: Type -> Type -> InferenceM ()
unify t1 t2 = do
    uf <- equality <$> get
    case (t1, t2) of
        ((TyName _), (TyName _)) ->
            error "can't unify two concrete types"
        _ -> let uf' = fromMaybe (error $ "union failed of: " ++ (show (t1, t2, uf))) $ U.union uf t1 t2
                in modify (\x -> x { equality = uf' })

builtinTypes :: [Type]
builtinTypes = [TyName "Fixnum", TyName "String"]

infer :: [Type] -> Method -> InferenceM ()
infer ts m@(Method n params body) = do
    forM_ params $ \(Var n t) -> extendTyEnv n t
    mapM_ inferStatement body
    --return $ Method n params body

check :: [Type] -> Method -> (Method, TypeEnv)
check ts m = (flip runState) te $ infer ts m >> return m
  where te = TypeEnv (length ts) (newUnionFind ts) (M.empty)

{- groupBy' :: (Ord b) => (a -> b) -> [a] -> [(b, [a])]
groupBy' f = map (f . head &&& id)
                   . groupBy ((==) `on` f)
                   . sortBy (compare `on` f) -}
    
inferStatement :: Statement -> InferenceM ()
inferStatement st = case st of
    Decl (Var n t) v -> do
      introduceTyVar t
      tv <- typeOf v
      unify t tv

lookupM :: String -> InferenceM Type
lookupM i = get >>= (\(TypeEnv _ _ m) -> return $ U.find m i)

typeOf :: Value -> InferenceM Type
typeOf v = case v of
            Nat   _   -> return $ TyName "Fixnum"
            Str   _   -> return $ TyName "String"
            Ident i   -> lookupM i

