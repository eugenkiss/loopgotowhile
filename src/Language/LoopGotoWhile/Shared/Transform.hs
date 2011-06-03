-- | General helper functions for transformation to a strict subset.
module Language.LoopGotoWhile.Shared.Transform 
    ( getUnusedVar
    , getStrictUnusedVars
    , partitionVars
    , renameVarInAExp
    , renameVarInBExp
    , getVarNamesInAExp
    , getVarNamesInBExp
    ) where

import Control.Monad.State
import Data.List ((\\), partition, nub)
import Data.Char (isDigit)

import Language.LoopGotoWhile.Shared.ExtendedAS


-- | Return an unused variable name and remove it from the list of unused
-- variable names (the state).
getUnusedVar :: State [VarIdent] VarIdent
getUnusedVar = do
    unusedVars <- get
    put $ drop 1 unusedVars
    return $ head unusedVars

-- | Create an infinite list of strict, unused variable names. A list of
-- already used variable names is given whose elements will not be inside the
-- created list.
--
-- The purpose of this function is to provide an unlimited supply of strict
-- variables *without* any strict variable that is already used in the program.
getStrictUnusedVars :: [VarIdent] -> [VarIdent]
getStrictUnusedVars used = varStream \\ used
  where varStream = iterate (\x -> 'x' : succ' (tail x)) "x1"
        succ' s   = show $ stoi s + 1
          where stoi :: String -> Integer
                stoi = read

-- | Rename all occurences of 'from' as a variable identifier to 'to' in the 
-- given arithmetic expression.
renameVarInAExp :: VarIdent -> VarIdent -> AExp -> AExp
renameVarInAExp from to aexp = case aexp of
    Const c      -> Const c
    Var var      -> Var (rename var)
    AOp op e1 e2 -> AOp op (renameVarInAExp from to e1)  
                           (renameVarInAExp from to e2)
  where rename var 
            | var == from = to 
            | otherwise   = var

-- | Rename all occurences of 'from' as a variable identifier to 'to' in the 
-- given boolean expression.
renameVarInBExp :: VarIdent -> VarIdent -> BExp -> BExp
renameVarInBExp from to bexp = case bexp of
    RelOp op aexp1 aexp2 -> RelOp op (renameVarInAExp from to aexp1)
                                     (renameVarInAExp from to aexp2)
    BOp op bexp1 bexp2   -> BOp   op (renameVarInBExp from to bexp1) 
                                     (renameVarInBExp from to bexp2)
    BNegOp bexp1         -> BNegOp   (renameVarInBExp from to bexp1)

-- | Return a list without duplicates of all variable identifiers in the
-- arithemtic expression. 
getVarNamesInAExp :: AExp -> [VarIdent]
getVarNamesInAExp = nub . f
  where f (Const _)     = []
        f (Var var)     = [var]
        f (AOp _ e1 e2) = f e1 ++ f e2

-- | Return a list without duplicates of all variable identifiers in the
-- boolean expression. 
getVarNamesInBExp :: BExp -> [VarIdent]
getVarNamesInBExp = nub . f
  where f (RelOp _ aexp1 aexp2) = getVarNamesInAExp aexp1 ++ 
                                  getVarNamesInAExp aexp2
        f (BOp _ bexp1 bexp2)   = f bexp1 ++ f bexp2
        f (BNegOp bexp)         = f bexp

-- | Partition a list of variable names into strict and unstrict names.
partitionVars :: [VarIdent] -> ([VarIdent], [VarIdent])
partitionVars = partition isStrictVar      

-- | Return true if the given identifier is strict.
isStrictVar :: VarIdent -> Bool
isStrictVar (x:xs) | not (null xs) = x == 'x' && all isDigit xs
isStrictVar _      = False
