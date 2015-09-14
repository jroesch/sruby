module SRuby.Util.UnionFind where

import qualified Data.Map as M

newUnionFind xs = M.fromList $ zip xs [0..]

find m i = M.findWithDefault (error "No such type binding for name") i m

union m i j = do
    x <- M.lookup i m
    y <- M.lookup j m
    return $ if x /= y
      then replace m x y
      else m
  where replace m i j = 
          let pairs = M.assocs m
              union (k, v) = if v == i then (k, j) else (k, v)
            in M.fromList $ map union pairs

