module Language.LoopGotoWhile.Common.Transform 
    ( getUnusedVar
    , getStrictUnusedVars
    ) where

import Control.Monad
import Control.Monad.State
import Data.List ((\\))

import Language.LoopGotoWhile.Common.ExtendedAS


-- ** General Helper Functions for Transformation to a Strict Subset
--    --------------------------------------------------------------

-- | Return an unused variable and remove it from the list of unused variables
-- (the state).
getUnusedVar :: State [VarIdent] VarIdent
getUnusedVar = do
    unusedVars <- get
    put $ drop 1 unusedVars
    return $ head unusedVars

-- | Create an infinite list of strict, unused variable names. A list of
-- already used variable names is given which will not be inside the created
-- list.
--
-- The purpose of this function is to provide an unlimited supply of strict
-- variables *without* any strict variable that is already used in the program.
getStrictUnusedVars :: [VarIdent] -> [VarIdent]
getStrictUnusedVars used = varstream \\ used
  where varstream = iterate (\x -> 'x' : (succ (tail x))) "x1"
        succ s = show $ toInteger (read s) + 1
