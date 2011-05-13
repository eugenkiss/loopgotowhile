module Language.LoopGotoWhile.Loop.Transform
    ( toStrict
    , toWhile
    , toGoto
    ) where

import Control.Monad
import Control.Monad.State
import Data.Char (isDigit)
import Data.List (partition, nub, (\\), union)

import Text.ParserCombinators.Parsec hiding (State)
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language (javaStyle)
import qualified Text.ParserCombinators.Parsec.Token as P

import Language.LoopGotoWhile.Loop.ExtendedADT
import qualified Language.LoopGotoWhile.Loop.Strict as Strict


-- * Transformation to strict Loop Subset
--   ====================================

-- | Transform an extended syntax tree into a strict one without changing the
-- semantics of the program.
toStrict :: Stat -> Strict.Statement
toStrict = toStrictLoop . toStrictStat . toStrictVars

-- | Transform an extended syntax tree into a strict one. Assume that the AST
-- is given in a directly translatable form.
toStrictLoop :: Stat -> Strict.Statement
toStrictLoop (Assign (_:i) (AOp "+" (Var (_:j)) (Const c))) = 
    Strict.Assignment (read i) (read j) Strict.Plus c
toStrictLoop (Assign (_:i) (AOp "-" (Var (_:j)) (Const c))) = 
    Strict.Assignment (read i) (read j) Strict.Minus c
toStrictLoop (Loop (Var (_:i)) stat) = Strict.Loop (read i) (toStrictLoop stat)
toStrictLoop (Seq stats)       = Strict.Sequence (map toStrictLoop stats)
toStrictLoop ast               = error $ "Extended AST is not in strict form: "
                                      ++ show ast 


-- ** Renaming of Variables
--    ---------------------

-- TODO: Would it be faster to give `renameVar` a list of renameMappings and
-- only traverse the AST once? (I suggest it isn't, since either way I need to
-- do the same number of steps; If I visit n stages of the AST k times (k is
-- the the length of the mappings list) or I need to check k mappings at each
-- of the n stages -> same difference.

-- | Change all occurences of unstrict variable names into strict variable
-- names without altering the semantics of the AST.
toStrictVars :: Stat -> Stat
toStrictVars ast = foldr (uncurry renameVar) ast renameMappings
  where renameMappings     = zip unstrict unused
        unused             = getStrictUnusedVars strict
        (strict, unstrict) = partitionVars $ getVarNames ast

-- | Rename all occurences of 'from' as a variable identifier to 'to' in the 
-- given AST.
renameVar :: VarIdent -> VarIdent -> Stat -> Stat
renameVar from to ast = case ast of
    Assign var aexp      -> Assign (r var) (rAExp aexp)
    Loop var stat        -> Loop (rAExp var) (rVar stat)
    If bexp s1 Nothing   -> If (rBExp bexp) (rVar s1) Nothing
    If bexp s1 (Just s2) -> If (rBExp bexp) (rVar s1) (Just (rVar s2))
    Seq stats            -> Seq (map rVar stats)
  where rVar      = renameVar from to
        rAExp (Var var)      = Var (r var)
        rAExp (AOp op e1 e2) = AOp op (rAExp e1) (rAExp e2)
        rAExp x              = x
        rBExp (RelOp op e1 e2) = RelOp op (rAExp e1) (rAExp e2)
        rBExp x                = x
        r var | var == from = to | otherwise = var
     
-- | Analyze the AST and return a list without duplicates of all used variable
-- names.
getVarNames :: Stat -> [VarIdent]
getVarNames = nub . f
  where f (Assign var aexp)      = var : f' aexp
        f (Loop aexp stat)       = f' aexp ++ f stat
        f (If bexp s1 Nothing)   = f'' bexp ++ f s1
        f (If bexp s1 (Just s2)) = f'' bexp ++ f s1 ++ f s2
        f (Seq stats)            = concatMap f stats
        f' (Var var)             = [var]
        f' (Const _)             = []
        f' (AOp _ e1 e2)         = f' e1 ++ f' e2
        f'' (RelOp _ e1 e2)      = f' e1 ++ f' e2
        f'' _                    = []

-- | Partition a list of variable names into strict and unstrict names.
partitionVars :: [VarIdent] -> ([VarIdent], [VarIdent])
partitionVars = partition isStrictVar      

-- | Return true if the given identifier is strict.
isStrictVar :: VarIdent -> Bool
isStrictVar (x:xs) | not (null xs) = x == 'x' && all isDigit xs
isStrictVar _      = False


-- ** Statements
--    ----------

-- | Transform an extended Loop AST into a strict Loop AST. Assume that all
-- variables are already strict. 
toStrictStat :: Stat -> Stat
toStrictStat ast = 
    evalState (toStrictStat' ast) (getStrictUnusedVars (getVarNames ast))

-- Since sometimes an unused variable is needed in order to correctly transform
-- a statement an infinite list of unused variable names is carried along as
-- state that gets updated whenever an unused variable name is requested.
toStrictStat' :: Stat -> State [VarIdent] Stat
-- v0 := v1 +- c. Keep unchanged!
toStrictStat' stat@(Assign v0 (AOp op (Var v1) (Const c)))
    | op `elem` ["+","-"] = return stat
-- v0 := v1
toStrictStat' (Assign v0 (Var v1)) = return $
    Assign v0 (AOp "+" (Var v1) (Const 0))
-- v0 := c
toStrictStat' (Assign v0 (Const c)) = do 
    unused <- getUnusedVar
    return $ Assign v0 (AOp "+" (Var unused) (Const c))
-- v0 := v1 o v2
toStrictStat' (Assign v0 (AOp op (Var v1) (Var v2))) = case op of  
    "+" -> toStrictStat' $ Seq  
               [ Assign v0 (Var v1)
               , Loop (Var v2) (Assign v0 (AOp "+" (Var v0) (Const 1)))
               ]
    "-" -> toStrictStat' $ Seq
               [ Assign v0 (Var v1)
               , Loop (Var v2) (Assign v0 (AOp "-" (Var v0) (Const 1)))
               ]
    "*" -> do u <- getUnusedVar
              toStrictStat' $ Seq 
                  [ Assign u (Const 0)
                  , Loop (Var v2) (Assign u (AOp "+" (Var u) (Var v1)))
                  , Assign v0 (Var u)
                  ]
    "^" -> do u <- getUnusedVar
              toStrictStat' $ Seq 
                  [ Assign u (Const 1)
                  , Loop (Var v2) (Assign u (AOp "*" (Var u) (Var v1)))
                  , Assign v0 (Var u)
                  ]
    "/" -> do c <- getUnusedVar
              toStrictStat' $ Seq $
                  [ Assign c (Const 0)
                  , Assign v0 (Var v1)
                  , Loop (Var v0) (If (RelOp ">=" (Var v0) (Var v2)) (Seq
                      [ Assign c  (AOp "+" (Var c) (Const 1))
                      , Assign v0 (AOp "-" (Var v0) (Var v2))
                      ]
                    ) Nothing) 
                  , Assign v0 (Var c)
                  ]
    "%" -> toStrictStat' $ Seq $
               [ Assign v0 (Var v1)
               , Loop (Var v0) (If (RelOp ">=" (Var v0) (Var v2))
                   (Assign v0 (AOp "-" (Var v0) (Var v2))) Nothing)
               ]
-- v0 := a o b
toStrictStat' (Assign v0 (AOp op a b)) = do    -- v0 := a o b =>
    ua <- getUnusedVar
    ub <- getUnusedVar
    toStrictStat' $ Seq                      
        [ Assign ua a                          -- ua := a
        , Assign ub b                          -- ub := b
        , Assign v0 (AOp op (Var ua) (Var ub)) -- v0 := ua o ub
        ]
-- IF b THEN P {ELSE P2} END
toStrictStat' (If (RelOp op a b) stat1 stat2) = case op of
    -- TODO: This is ugly because I want to handle both the if and ifelse case
    -- in one do expression. Maybe add code duplication for the sake of
    -- readability?
    "="  -> do u1 <- getUnusedVar
               u2 <- getUnusedVar
               let t = Assign u2 (Const 0)
               (body, u3) <- case stat2 of
                               Nothing -> return (t, Nothing)
                               Just _  -> do u3 <- getUnusedVar
                                             return (Seq [t, Assign u3 (Const 1)], Just u3)
               toStrictStat' $ Seq $ 
                   [ Assign u1 (AOp "+" (AOp "-" a b) (AOp "-" b a))
                   , Assign u2 (Const 1)
                   ] ++ case u3 of
                          Just u -> [Assign u (Const 0)]
                          _      -> []
                     ++
                   [ Loop (Var u1) body 
                   , Loop (Var u2) stat1
                   ] ++ case (stat2, u3) of
                          (Just s, Just u) -> [Loop (Var u) s]
                          _ -> []
    "!=" -> f (AOp "+" (AOp "-" a b) (AOp "-" b a))
    "<"  -> f (AOp "-" b a) 
    ">"  -> toStrictStat' $ If (RelOp "<" b a) stat1 stat2
    "<=" -> toStrictStat' $ If (BOp "||" (RelOp "<" a b) (RelOp "=" a b)) stat1 stat2
    ">=" -> toStrictStat' $ If (BOp "||" (RelOp ">" a b) (RelOp "=" a b)) stat1 stat2
  where f aexp = do 
            u1 <- getUnusedVar
            u2 <- getUnusedVar
            let t = Assign u2 (Const 1)
            (body, u3) <- case stat2 of
                            Nothing -> return (t, Nothing)
                            Just _  -> do u3 <- getUnusedVar
                                          return (Seq [t, Assign u3 (Const 0)], Just u3)
            toStrictStat' $ Seq $ 
                [ Assign u1 aexp
                , Assign u2 (Const 0)
                ] ++ case u3 of
                       Just u -> [Assign u (Const 1)]
                       _      -> []
                  ++
                [ Loop (Var u1) body 
                , Loop (Var u2) stat1
                ] ++ case (stat2, u3) of
                       (Just s, Just u) -> [Loop (Var u) s]
                       _ -> []
toStrictStat' (If (BOp "&&" a b) stat1 Nothing) = toStrictStat' $
    If a (If b stat1 Nothing) Nothing
toStrictStat' (If (BOp "&&" a b) stat1 (Just stat2)) = do
    u <- getUnusedVar
    toStrictStat' $ Seq $
        [ Assign u (Const 1) 
        , If a (If b (Seq [Assign u (Const 0), stat1]) Nothing) Nothing
        , Loop (Var u) stat2
        ]
toStrictStat' (If (BOp "||" a b) stat1 Nothing) = do
    u <- getUnusedVar
    toStrictStat' $ Seq $
        [ Assign u (Const 0)
        , If a (Assign u (Const 1)) Nothing
        , If b (Assign u (Const 1)) Nothing
        , Loop (Var u) stat1
        ]
toStrictStat' (If (BOp "||" a b) stat1 (Just stat2)) = do
    u1 <- getUnusedVar
    u2 <- getUnusedVar
    toStrictStat' $ Seq $
        [ Assign u1 (Const 0)
        , Assign u2 (Const 1)
        , If a (Seq [Assign u1 (Const 1), Assign u2 (Const 0)]) Nothing
        , If b (Seq [Assign u1 (Const 1), Assign u2 (Const 0)]) Nothing
        , Loop (Var u1) stat1
        , Loop (Var u2) stat2
        ]
toStrictStat' (If (BNegOp bexp) stat1 stat2) = toStrictStat' $ case bexp of
    RelOp "="  a b -> If (RelOp "!=" a b) stat1 stat2
    RelOp "!=" a b -> If (RelOp "="  a b) stat1 stat2
    RelOp "<"  a b -> If (RelOp ">=" a b) stat1 stat2
    RelOp ">"  a b -> If (RelOp "<=" a b) stat1 stat2
    RelOp "<=" a b -> If (RelOp ">"  a b) stat1 stat2
    RelOp ">=" a b -> If (RelOp "<"  a b) stat1 stat2
    BOp "&&" a b -> If (BOp "||" (BNegOp a) (BNegOp b)) stat1 stat2
    BOp "||" a b -> If (BOp "&&" (BNegOp a) (BNegOp b)) stat1 stat2
    BNegOp bexp' -> If bexp' stat1 stat2
-- LOOP a DO P END
toStrictStat' (Loop (Var v0) stat) = liftM (Loop (Var v0)) $ toStrictStat' stat
toStrictStat' (Loop aexp stat) = do
    unused <- getUnusedVar
    toStrictStat' $ Seq $
        [ Assign unused aexp
        , Loop (Var unused) stat
        ]
-- P1; P2;...
toStrictStat' (Seq stats) = liftM (Seq . flatten) $ mapM toStrictStat' stats
    where flatten = foldr f [] 
          f (Seq stmnts) acc = stmnts ++ acc
          f x            acc = x : acc
-- All cases must have been checked by now.
toStrictStat' _ = error $ "Not all cases were considered when transforming " ++
                          "extended Loop to strict Loop!"


-- ** General Helper Functions
--    ------------------------

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

-- TODO: toWhile
toWhile = undefined

-- TODO: toGoto
toGoto = undefined
