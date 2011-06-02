-- | Functions for transforming While into Goto or into a semantically
-- equivalent strict subset.
module Language.LoopGotoWhile.While.Transform
    ( toExtended
    , toStrict
    , toGoto
    ) where

import Control.Monad
import Control.Monad.State
import Data.List (nub)

import qualified Language.LoopGotoWhile.Goto.ExtendedAS as GotoAS
import qualified Language.LoopGotoWhile.While.StrictAS as Strict
import Language.LoopGotoWhile.While.ExtendedAS
import Language.LoopGotoWhile.Shared.Transform 
import Language.LoopGotoWhile.Goto.Transform 
    ( TransformState
    , getUnusedLabel
    , getStrictUnusedLabels
    )


-- * Transformation from strict While to extended While
--   ==================================================

-- | Transform a strict syntax tree into an extended one without changing the
-- semantics of the program.
-- 
-- Since the strict syntax is a subset of the extended syntax this functions
-- merely translates the strict data constructors to the extended ones in
-- a very direct manner. This function is only needed to make some
-- transformations / function compositions easier.
toExtended :: Strict.Program -> Program
toExtended (Strict.Assign i j Strict.Plus c) = 
    Assign ('x' : show i) (AOp "+" (Var ('x' : show j)) (Const c))
toExtended (Strict.Assign i j Strict.Minus c) = 
    Assign ('x' : show i) (AOp "-" (Var ('x' : show j)) (Const c))
toExtended (Strict.While i stat) = 
    While (RelOp "!=" (Var ('x' : show i)) (Const 0)) (toExtended stat)
toExtended (Strict.Seq stats) = Seq $ map toExtended stats


-- * Transformation from extended While to strict While
--   ==================================================

-- | Transform an extended syntax tree into a strict one without changing the
-- semantics of the program.
toStrict :: Program -> Strict.Program
toStrict = toStrict' . toStrictStat . toStrictVars

-- | Transform an extended syntax tree into a strict one. Assume that the AST
-- is given in a directly translatable form.
toStrict' :: Program -> Strict.Program
toStrict' (Assign (_:i) (AOp "+" (Var (_:j)) (Const c))) =
    Strict.Assign (read i) (read j) Strict.Plus c
toStrict' (Assign (_:i) (AOp "-" (Var (_:j)) (Const c))) =
    Strict.Assign (read i) (read j) Strict.Minus c
toStrict' (While (RelOp "!=" (Var (_:i)) (Const 0)) stat) = 
    Strict.While (read i) (toStrict' stat)
toStrict' (Seq stats) = Strict.Seq (map toStrict' stats)
toStrict' ast = error $ "Extended AST is not in strict form: " ++ show ast


-- ** Renaming of Variables
--    ---------------------

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
    While bexp stat      -> While (rBExp bexp) (rVar stat)
    If bexp s1 Nothing   -> If (rBExp bexp) (rVar s1) Nothing
    If bexp s1 (Just s2) -> If (rBExp bexp) (rVar s1) (Just (rVar s2))
    Seq stats            -> Seq (map rVar stats)
  where rVar  = renameVar from to
        rAExp = renameVarInAExp from to
        rBExp = renameVarInBExp from to
        r var | var == from = to | otherwise = var
     
-- | Analyze the AST and return a list without duplicates of all used variable
-- names.
getVarNames :: Stat -> [VarIdent]
getVarNames = nub . f
  where f (Assign var aexp)      = var : getVarNamesInAExp aexp
        f (While bexp stat)      = getVarNamesInBExp bexp ++ f stat
        f (If bexp s1 Nothing)   = getVarNamesInBExp bexp ++ f s1
        f (If bexp s1 (Just s2)) = getVarNamesInBExp bexp ++ f s1 ++ f s2
        f (Seq stats)            = concatMap f stats


-- ** Statements
--    ----------

-- | Transform an extended While AST into a strict While AST. Assume that all
-- variables are already strict.
toStrictStat :: Stat -> Stat
toStrictStat ast =
    evalState (toStrictStat' ast) (getStrictUnusedVars (getVarNames ast))

-- Since sometimes an unused variable is needed in order to correctly transform
-- a statement an infinite list of unused variable names is carried along as
-- state that gets updated whenever an unused variable name is requested.
toStrictStat' :: Stat -> State [VarIdent] Stat
-- v0 := v1 +- c. Keep unchanged!
toStrictStat' stat@(Assign _ (AOp op (Var _) (Const _)))
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
    "+" -> do ltw <- loopToWhile $ Loop (Var v2) (Assign v0 (AOp "+" (Var v0) (Const 1)))
              toStrictStat' $ Seq 
                  [ Assign v0 (Var v1)
                  , ltw
                  ]
    "-" -> do ltw <- loopToWhile $ Loop (Var v2) (Assign v0 (AOp "-" (Var v0) (Const 1)))
              toStrictStat' $ Seq
                  [ Assign v0 (Var v1)
                  , ltw
                  ]
    "*" -> do u <- getUnusedVar
              ltw <- loopToWhile $ Loop (Var v2) (Assign u (AOp "+" (Var u) (Var v1)))
              toStrictStat' $ Seq
                  [ Assign u (Const 0)
                  , ltw
                  , Assign v0 (Var u)
                  ]
    "^" -> do u <- getUnusedVar
              ltw <- loopToWhile $ Loop (Var v2) (Assign u (AOp "*" (Var u) (Var v1)))
              toStrictStat' $ Seq
                  [ Assign u (Const 1)
                  , ltw
                  , Assign v0 (Var u)
                  ]
    "/" -> do c <- getUnusedVar
              ltw <- loopToWhile $ Loop (Var v0) (If (RelOp ">=" (Var v0) (Var v2)) (Seq
                         [ Assign c  (AOp "+" (Var c) (Const 1))
                         , Assign v0 (AOp "-" (Var v0) (Var v2))
                         ]) Nothing)
              toStrictStat' $ Seq 
                  [ Assign c (Const 0)
                  , Assign v0 (Var v1)
                  , ltw
                  , Assign v0 (Var c)
                  ]
    "%" -> do ltw <- loopToWhile $ Loop (Var v0) (If (RelOp ">=" (Var v0) (Var v2))
                         (Assign v0 (AOp "-" (Var v0) (Var v2))) Nothing) 
              toStrictStat' $ Seq
                  [ Assign v0 (Var v1)
                  , ltw
                  ]
    _   -> error "Impossible! Wrong operator!"
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
               ltw1 <- loopToWhile $ Loop (Var u1) body
               ltw2 <- loopToWhile $ Loop (Var u2) stat1
               t' <- case (stat2, u3) of
                          (Just s, Just u) -> do ltw3 <- loopToWhile $ Loop (Var u) s
                                                 return [ltw3]
                          _                -> return []
               toStrictStat' $ Seq $
                   [ Assign u1 (AOp "+" (AOp "-" a b) (AOp "-" b a))
                   , Assign u2 (Const 1)
                   ] ++ case u3 of
                          Just u -> [Assign u (Const 0)]
                          _      -> []
                     ++
                   [ ltw1
                   , ltw2
                   ] ++ t'
    "!=" -> f (AOp "+" (AOp "-" a b) (AOp "-" b a))
    "<"  -> f (AOp "-" b a)
    ">"  -> toStrictStat' $ If (RelOp "<" b a) stat1 stat2
    "<=" -> toStrictStat' $ If (BOp "||" (RelOp "<" a b) (RelOp "=" a b)) stat1 stat2
    ">=" -> toStrictStat' $ If (BOp "||" (RelOp ">" a b) (RelOp "=" a b)) stat1 stat2
    _    -> error "Impossible! Wrong operator!"
  where f aexp = do
            u1 <- getUnusedVar
            u2 <- getUnusedVar
            let t = Assign u2 (Const 1)
            (body, u3) <- case stat2 of
                            Nothing -> return (t, Nothing)
                            Just _  -> do u3 <- getUnusedVar
                                          return (Seq [t, Assign u3 (Const 0)], Just u3)
            ltw1 <- loopToWhile $ Loop (Var u1) body
            ltw2 <- loopToWhile $ Loop (Var u2) stat1
            t' <- case (stat2, u3) of
                       (Just s, Just u) -> do ltw3 <- loopToWhile $ Loop (Var u) s
                                              return [ltw3]
                       _                -> return []
            toStrictStat' $ Seq $
                [ Assign u1 aexp
                , Assign u2 (Const 0)
                ] ++ case u3 of
                       Just u -> [Assign u (Const 1)]
                       _      -> []
                  ++
                [ ltw1
                , ltw2
                ] ++ t'
toStrictStat' (If (BOp "&&" a b) stat1 Nothing) = toStrictStat' $
    If a (If b stat1 Nothing) Nothing
toStrictStat' (If (BOp "&&" a b) stat1 (Just stat2)) = do
    u <- getUnusedVar
    ltw <- loopToWhile $ Loop (Var u) stat2
    toStrictStat' $ Seq 
        [ Assign u (Const 1)
        , If a (If b (Seq [Assign u (Const 0), stat1]) Nothing) Nothing
        , ltw
        ]
toStrictStat' (If (BOp "||" a b) stat1 Nothing) = do
    u <- getUnusedVar
    ltw <- loopToWhile $ Loop (Var u) stat1
    toStrictStat' $ Seq 
        [ Assign u (Const 0)
        , If a (Assign u (Const 1)) Nothing
        , If b (Assign u (Const 1)) Nothing
        , ltw
        ]
toStrictStat' (If (BOp "||" a b) stat1 (Just stat2)) = do
    u1 <- getUnusedVar
    u2 <- getUnusedVar
    ltw1 <- loopToWhile $ Loop (Var u1) stat1
    ltw2 <- loopToWhile $ Loop (Var u2) stat2
    toStrictStat' $ Seq 
        [ Assign u1 (Const 0)
        , Assign u2 (Const 1)
        , If a (Seq [Assign u1 (Const 1), Assign u2 (Const 0)]) Nothing
        , If b (Seq [Assign u1 (Const 1), Assign u2 (Const 0)]) Nothing
        , ltw1
        , ltw2
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
    _ -> error "Impossible! Wrong operator!"
-- WHILE a DO P END
toStrictStat' (While rel@(RelOp "!=" (Var _) (Const 0)) stat) = 
    liftM (While rel) $ toStrictStat' stat
toStrictStat' (While bexp stat) = do
    c <- getUnusedVar
    let if' = If bexp (Assign c (Const 1)) (Just (Assign c (Const 0)))
    toStrictStat' $ Seq 
        [ if'
        , While (RelOp "!=" (Var c) (Const 0)) $ Seq
              [ stat
              , if'
              ]
        ]
-- P1; P2;...
toStrictStat' (Seq stats) = liftM (Seq . flatten) $ mapM toStrictStat' stats
    where flatten = foldr f []
          f (Seq stmnts) acc = stmnts ++ acc
          f x            acc = x : acc
-- All cases must have been checked by now.
toStrictStat' _ = error $ "Not all cases were considered when transforming " ++
                          "extended Goto to strict Goto!"

-- Helper to make the toStrictStat' as close as possible to the Loop version
data Loop = Loop AExp Stat

loopToWhile :: Loop -> State [VarIdent] Stat
loopToWhile (Loop aexp stat) = do
    c <- getUnusedVar
    toStrictStat' $ Seq
        [ Assign c aexp
        , While (RelOp "!=" (Var c) (Const 0)) $ Seq
              [ Assign c (AOp "-" (Var c) (Const 1))
              , stat
              ]
        ]


-- * Transformation to Goto
--   ======================

-- | Transform a While AST to a Goto AST.
toGoto :: Stat -> GotoAS.Stat
toGoto ast = evalState (
                 evalStateT (toGoto' ast) (getStrictUnusedLabels [])
             ) []

-- The reason for the use of the state monad is that the transformation of
-- a While loop to a "Goto loop" needs unused labels and the list of unusued
-- labels is carried along in the state.
toGoto' :: Stat -> TransformState GotoAS.Stat
toGoto' (Assign v aexp) = return $ GotoAS.Assign v aexp
toGoto' (If bexp statThen statElse) = do
    gStatThen <- toGoto' statThen
    gStatElse <- case statElse of 
                   Just s  -> do s' <- toGoto' s
                                 return $ Just s'
                   Nothing -> return Nothing
    return $ GotoAS.If bexp gStatThen gStatElse
toGoto' (While bexp stat) = do
    m0 <- getUnusedLabel
    m1 <- getUnusedLabel
    s <- toGoto' stat
    return $ GotoAS.Seq $
        [ GotoAS.Label m0 $ GotoAS.If (BNegOp bexp) (GotoAS.Goto m1) Nothing
        ] ++ flatten s ++ 
        [ GotoAS.Goto m0
        , GotoAS.Label m1 $ GotoAS.Assign "x0" (GotoAS.Var "x0")
        ]
  where flatten (GotoAS.Seq xs)              = xs
        flatten x                            = [x]
toGoto' (Seq stats) = liftM GotoAS.Seq $ mapM toGoto' stats
