module Main where

import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import qualified Data.Map as M
import Text.Parsec
import Text.Parsec.String hiding (Parser)
import Control.Applicative ((*>), (<*), (<*>), (<$>))
import Control.Monad.State as S
import System.Environment

import SRuby.Parser
import SRuby.Util.UnionFind 

main :: IO ()
main = do
    args <- getArgs
    case (args !! 0) of
        "-f" -> do
            contents <- readFile (args !! 1)
            result <- interpret contents
            print result
        "-i" -> do
            line <- getLine
            result <- interpret line
            print result

extendTyEnv :: Type -> S.State (M.Map Type Int, Int) ()
extendTyEnv t = do
    (uf, max) <- get
    let uf' = M.insert t (max + 1) uf
    put (uf', max + 1)

unify :: Type -> Type -> S.State (M.Map Type Int, Int) ()
unify t1 t2 = do
    (uf, max) <- get
    case (t1, t2) of
        ((TyName _), (TyName _)) ->
            error "can't unify two concrete types"
        _ -> let uf' = fromMaybe (error $ "union failed of: " ++ (show (t1, t2, uf))) $ union uf t1 t2
                in put (uf', max)
              
-- infer :: M.Map Type Int -> Method -> Method
infer uf m@(Method n params body) = (flip runState) (uf, M.size uf) $ do
    forM_ params $ \(Var _ t) -> extendTyEnv t
    body' <- mapM inferStatement body
    return $ Method n params body'
    
inferStatement st = case st of
    Decl (Var s t) v -> return $ Decl (Var s (typeOf v)) v
         
builtinTypes = [TyName "Fixnum", TyName "String"]

typeOf :: Value -> Type
typeOf v = case v of
            Nat _ -> TyName "Fixnum"
            Str _ -> TyName "String"
    
