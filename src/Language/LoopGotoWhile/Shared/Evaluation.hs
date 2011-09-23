-- | Helper functions for evaluation.
module Language.LoopGotoWhile.Shared.Evaluation
    ( Env
    , nullEnv
    , getVar
    , setVar
    ) where

import Control.Monad.ST
import Data.STRef
import qualified Data.HashMap as M

type Index = Integer

type Env s = STRef s (M.Map Index (STRef s Integer))

nullEnv :: ST s (Env s)
nullEnv = newSTRef M.empty

getVar :: Env s -> Index -> ST s Integer
getVar envRef i = do
    env <- readSTRef envRef
    case M.lookup i env of
      Just varRef -> readSTRef varRef
      Nothing     -> do x <- newSTRef 0
                        writeSTRef envRef (M.insert i x env) 
                        return $ fromInteger 0

setVar :: Env s -> Index -> Integer -> ST s ()
setVar envRef i v = do
    env <- readSTRef envRef
    case M.lookup i env of
      Just varRef -> writeSTRef varRef v
      Nothing     -> do x <- newSTRef v
                        writeSTRef envRef (M.insert i x env) 
