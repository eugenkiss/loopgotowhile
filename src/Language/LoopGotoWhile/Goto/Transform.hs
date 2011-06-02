module Language.LoopGotoWhile.Goto.Transform
    ( toExtended
    , toStrict
    , toWhile
    -- Needed by While.Transform
    , TransformState
    , getUnusedLabel
    , getStrictUnusedLabels
    ) where

import Control.Monad
import Control.Monad.State
import Control.Monad.ST
import Data.STRef
import Data.Array.ST
import Data.Char (isDigit)
import Data.List (partition, nub, (\\), union)

import Text.ParserCombinators.Parsec hiding (State)
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language (javaStyle)
import qualified Text.ParserCombinators.Parsec.Token as P

import Language.LoopGotoWhile.Goto.ExtendedAS
import qualified Language.LoopGotoWhile.Common.Transform as T
import qualified Language.LoopGotoWhile.Goto.StrictAS as StrictAS
import qualified Language.LoopGotoWhile.While.ExtendedAS as WhileAS


-- * Transformation from strict Goto to extended Goto
--   ================================================

-- | Transform a strict syntax tree into an extended one without changing the
-- semantics of the program.
toExtended :: StrictAS.Stat -> Stat
toExtended (StrictAS.Assign l i j StrictAS.Plus c) = Label (indexToLabel l) $ 
    Assign ('x' : show i) (AOp "+" (Var ('x' : show j)) (Const c))
toExtended (StrictAS.Assign l i j StrictAS.Minus c) = Label (indexToLabel l) $
    Assign ('x' : show i) (AOp "-" (Var ('x' : show j)) (Const c))
toExtended (StrictAS.IfGoto l1 i c l2) = Label (indexToLabel l1) $
    If (RelOp "=" (Var ('x' : show i)) (Const c)) (Goto (indexToLabel l2)) Nothing
toExtended (StrictAS.Goto l1 l2) = 
    Label (indexToLabel l1) $ Goto (indexToLabel l2)
toExtended (StrictAS.Halt l) = Label (indexToLabel l) $ Halt
toExtended (StrictAS.Seq stats) = Seq $ map toExtended stats

indexToLabel :: StrictAS.LIndex -> LIdent
indexToLabel i = 'M' : show i


-- * Transformation from extended Goto to strict Goto
--   ================================================

-- | Transform an extended syntax tree into a strict one without changing the
-- semantics of the program.
toStrict :: Stat -> StrictAS.Stat
toStrict = toStrictGoto 
         . flatten
         . addHalt 
         . strictifyUndefLabels
         . toStrictLabels 
         . toStrictStat 
         . toStrictVars

-- | Transform an extended syntax tree into a strict one. Assume that the AST
-- is given in a directly translatable form.
toStrictGoto :: Stat -> StrictAS.Stat
toStrictGoto (Label l (Assign (_:i) (AOp "+" (Var (_:j)) (Const c)))) =
    StrictAS.Assign (labelToIndex l) (read i) (read j) StrictAS.Plus c
toStrictGoto (Label l (Assign (_:i) (AOp "-" (Var (_:j)) (Const c)))) =
    StrictAS.Assign (labelToIndex l) (read i) (read j) StrictAS.Minus c
toStrictGoto (Label l1 (If (RelOp "=" (Var (_:i)) (Const c)) (Goto l2) Nothing)) =
    StrictAS.IfGoto (labelToIndex l1) (read i) c (labelToIndex l2)
toStrictGoto (Label l1 (Goto l2)) = StrictAS.Goto (labelToIndex l1) (labelToIndex l2)
toStrictGoto (Label l (Halt)) = StrictAS.Halt (labelToIndex l)
toStrictGoto (Seq stats) = StrictAS.Seq (map toStrictGoto stats)
toStrictGoto ast         = error $ "Extended AST is not in strict form: "
                                 ++ show ast

labelToIndex :: LIdent -> StrictAS.LIndex
labelToIndex (_:i) = read i

-- | Unwrap the singleton sequence.
flatten :: Stat -> Stat
flatten (Seq [x]) = x
flatten x         = x

-- | Add a Halt statement to the end if needed.
addHalt :: Stat -> Stat
addHalt (Seq stats) = case lastStat of
    Label _ (Goto _) -> Seq stats
    Label _ (Halt)   -> Seq stats
    _                -> Seq $ stats ++ [Label ('M' : show (l + 1)) Halt]
  where l        = length stats
        lastStat = last stats


-- ** Renaming of Labels
--    ------------------

-- | Transform GOTOs with an undefined label to "M1".
strictifyUndefLabels :: Stat -> Stat
strictifyUndefLabels ast = case ast of
    Goto l               -> Goto (r l)
    If bexp s Nothing    -> If bexp (sUL s) Nothing
    If bexp s1 (Just s2) -> If bexp (sUL s1) (Just (sUL s2))
    Label l stat         -> Label l (sUL stat)
    Seq stats            -> Seq (map sUL stats)
    x                    -> x
  where sUL = strictifyUndefLabels
        r l | isStrictLab l = l | otherwise = "M1"
        isStrictLab (x:xs) | not (null xs) = x == 'M' && all isDigit xs
        isStrictLab _      = False

-- TODO: This is one ugly ******.

-- | Rename and reorder the labels in such a way that they are successive and
-- the first label is "M1". Assume that the AST is already flat.
toStrictLabels :: Stat -> Stat
toStrictLabels (Seq stats) = Seq $ runST $ do
    arr <- newListArray (1, length stats) stats :: ST s (STArray s Int Stat)
    marked <- newSTRef [] 
    forM_ [1..length stats] $ \i -> do
        e <- readArray arr i
        let newLabel = 'M' : show i
        case e of 
          Label l stat -> do 
              writeArray arr i (Label newLabel stat) 
              marked' <- readSTRef marked
              forM_ ([1..length stats] \\ marked') $ \j -> do
                  s <- readArray arr j
                  let renamed = renameGotoLabel l newLabel s  
                  writeArray arr j (renameGotoLabel l newLabel s) 
                  if s /= renamed
                     then modifySTRef marked ((:) j)
                     else return ()
          other        -> do
              writeArray arr i (Label newLabel other) 
    getElems arr
toStrictLabels stat = toStrictLabels $ Seq [stat]
                           
-- | Rename all occurences of 'from' as a label name to 'to' in the given AST.
renameGotoLabel :: LIdent -> LIdent -> Stat -> Stat
renameGotoLabel from to stat = case stat of
    Goto l               -> Goto (r l)
    If bexp s Nothing    -> If bexp (rLab s) Nothing
    If bexp s1 (Just s2) -> If bexp (rLab s1) (Just (rLab s2))
    Label l stat         -> Label l (rLab stat)
    x                    -> x
  where rLab = renameGotoLabel from to
        r l | l == from = to | otherwise = l

-- | Return an unused label name and remove it from the list of unused label
-- names (the state).
getUnusedLabel :: TransformState LIdent
getUnusedLabel = do
   unusedLabels <- get 
   put $ drop 1 unusedLabels
   return $ head unusedLabels

-- | Create an infinite list of strict, unused label names. A list of already
-- used label names is given whose elements will not be inside the created
-- list.
--
-- The purpose of this function is to provide an unlimited supply of strict
-- labels *without* any strict label that is already used in the program.
getStrictUnusedLabels :: [LIdent] -> [LIdent]
getStrictUnusedLabels used = labelStream \\ used
  where labelStream = iterate (\l -> 'M' : (succ' (tail l))) "M1"
        succ' s     = show $ toInteger (read s) + 1

-- | Analyze the AST and return a list without duplicates of all used label
-- names.
getLabelNames :: Stat -> [LIdent]
getLabelNames = nub . f
  where f (If bexp s Nothing)    = f s
        f (If bexp s1 (Just s2)) = f s1 ++ f s2
        f (Goto l)               = [l] 
        f (Label l stat)         = l : f stat
        f (Seq stats)            = concatMap f stats
        f x                      = []


-- ** Renaming of Variables
--    ---------------------

-- | Change all occurences of unstrict variable names into strict variable
-- names without altering the semantics of the AST.
toStrictVars :: Stat -> Stat
toStrictVars ast = foldr (uncurry renameVar) ast renameMappings
  where renameMappings     = zip unstrict unused
        unused             = T.getStrictUnusedVars strict
        (strict, unstrict) = partitionVars $ getVarNames ast

-- | Rename all occurences of 'from' as a variable identifier to 'to' in the
-- given AST.
renameVar :: VarIdent -> VarIdent -> Stat -> Stat
renameVar from to ast = case ast of
    Assign var aexp      -> Assign (r var) (rAExp aexp)
    If bexp s1 Nothing   -> If (rBExp bexp) (rVar s1) Nothing
    If bexp s1 (Just s2) -> If (rBExp bexp) (rVar s1) (Just (rVar s2))
    Label l stat         -> Label l (rVar stat)
    Seq stats            -> Seq (map rVar stats)
    x                    -> x
  where rVar                 = renameVar from to
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
        f (If bexp s1 Nothing)   = f'' bexp ++ f s1
        f (If bexp s1 (Just s2)) = f'' bexp ++ f s1 ++ f s2
        f (Label _ stat)         = f stat
        f (Seq stats)            = concatMap f stats
        f _                      = []
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

-- | Transform an extended Goto AST into a strict Goto AST. Assume that all
-- variables are already strict.
toStrictStat :: Stat -> Stat
toStrictStat ast =
    evalState (
      evalStateT (toStrictStat' ast) (getStrictUnusedLabels (getLabelNames ast))
    ) (T.getStrictUnusedVars (getVarNames ast))

-- Since sometimes an unused variable/label is needed in order to correctly
-- transform a statement two infinite lists of unused variable names and unused
-- label names are carried along as state that gets updated whenever an unused
-- variable resp. label name is requested.
type TransformState = StateT [LIdent] (State [VarIdent])

-- Lift getUnusedVar because of Monad Transformation madness!
getUnusedVar = lift T.getUnusedVar

toStrictStat' :: Stat -> TransformState Stat
-- GOTO Mx. Keep unchanged!
toStrictStat' stat@(Goto _) = return stat
-- HALT. Keep unchanged!
toStrictStat' Halt = return Halt
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
    "+" -> do ltw <- loopToGoto $ Loop (Var v2) (Assign v0 (AOp "+" (Var v0) (Const 1)))
              toStrictStat' $ Seq
                  [ Assign v0 (Var v1)
                  , ltw
                  ]
    "-" -> do ltw <- loopToGoto $ Loop (Var v2) (Assign v0 (AOp "-" (Var v0) (Const 1)))
              toStrictStat' $ Seq
                  [ Assign v0 (Var v1)
                  , ltw
                  ]
    "*" -> do u <- getUnusedVar
              ltw <- loopToGoto $ Loop (Var v2) (Assign u (AOp "+" (Var u) (Var v1)))
              toStrictStat' $ Seq
                  [ Assign u (Const 0)
                  , ltw
                  , Assign v0 (Var u)
                  ]
    "^" -> do u <- getUnusedVar
              ltw <- loopToGoto $ Loop (Var v2) (Assign u (AOp "*" (Var u) (Var v1)))
              toStrictStat' $ Seq
                  [ Assign u (Const 1)
                  , ltw
                  , Assign v0 (Var u)
                  ]
    "/" -> do c <- getUnusedVar
              ltw <- loopToGoto $ Loop (Var v0) (If (RelOp ">=" (Var v0) (Var v2)) (Seq
                         [ Assign c  (AOp "+" (Var c) (Const 1))
                         , Assign v0 (AOp "-" (Var v0) (Var v2))
                         ]) Nothing)
              toStrictStat' $ Seq $
                  [ Assign c (Const 0)
                  , Assign v0 (Var v1)
                  , ltw
                  , Assign v0 (Var c)
                  ]
    "%" -> do ltw <- loopToGoto $ Loop (Var v0) (If (RelOp ">=" (Var v0) (Var v2))
                         (Assign v0 (AOp "-" (Var v0) (Var v2))) Nothing)
              toStrictStat' $ Seq $
                  [ Assign v0 (Var v1)
                  , ltw
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
-- IF xi = c THEN GOTO Mx END -- Keep that unchanged!
toStrictStat' stat@(If (RelOp "=" (Var _) (Const _)) (Goto _) Nothing) = 
    return stat
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
               ltw1 <- loopToGoto $ Loop (Var u1) body
               ltw2 <- loopToGoto $ Loop (Var u2) stat1
               t' <- case (stat2, u3) of
                          (Just s, Just u) -> do ltw3 <- loopToGoto $ Loop (Var u) s
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
  where f aexp = do
            u1 <- getUnusedVar
            u2 <- getUnusedVar
            let t = Assign u2 (Const 1)
            (body, u3) <- case stat2 of
                            Nothing -> return (t, Nothing)
                            Just _  -> do u3 <- getUnusedVar
                                          return (Seq [t, Assign u3 (Const 0)], Just u3)
            ltw1 <- loopToGoto $ Loop (Var u1) body
            ltw2 <- loopToGoto $ Loop (Var u2) stat1
            t' <- case (stat2, u3) of
                       (Just s, Just u) -> do ltw3 <- loopToGoto $ Loop (Var u) s
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
    ltw <- loopToGoto $ Loop (Var u) stat2
    toStrictStat' $ Seq $
        [ Assign u (Const 1)
        , If a (If b (Seq [Assign u (Const 0), stat1]) Nothing) Nothing
        , ltw
        ]
toStrictStat' (If (BOp "||" a b) stat1 Nothing) = do
    u <- getUnusedVar
    ltw <- loopToGoto $ Loop (Var u) stat1
    toStrictStat' $ Seq $
        [ Assign u (Const 0)
        , If a (Assign u (Const 1)) Nothing
        , If b (Assign u (Const 1)) Nothing
        , ltw
        ]
toStrictStat' (If (BOp "||" a b) stat1 (Just stat2)) = do
    u1 <- getUnusedVar
    u2 <- getUnusedVar
    ltw1 <- loopToGoto $ Loop (Var u1) stat1
    ltw2 <- loopToGoto $ Loop (Var u2) stat2
    toStrictStat' $ Seq $
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
-- Mx: P
toStrictStat' (Label l stat) = liftM (Label l) (toStrictStat' stat)
-- P1; P2;...
toStrictStat' (Seq stats) = liftM (Seq . flatten) $ mapM toStrictStat' stats
    where flatten = foldr f []
          f (Seq stmnts) acc = stmnts ++ acc
          f x            acc = x : acc
-- All cases must have been checked by now.
toStrictStat' _ = error $ "Not all cases were considered when transforming " ++
                          "extended Goto to strict Goto!"

-- Helper to make the toStrictStat' as close as possible to the Loop version.
data Loop = Loop AExp Stat

loopToGoto :: Loop -> TransformState Stat
loopToGoto (Loop aexp stat) = do
    c <- getUnusedVar
    my <- getUnusedLabel
    mx <- getUnusedLabel
    toStrictStat' $ Seq
        [ Assign c aexp
        , Label my $ If (RelOp "=" (Var c) (Const 0)) (Goto mx) Nothing
        , Assign c (AOp "-" (Var c) (Const 1))
        , stat
        , Goto my
        , Label mx $ Assign c (Var c) -- NOOP
        ]


-- * Transformation to While
--   =======================

-- | Transform a Goto AST to a While AST.
toWhile :: Program -> WhileAS.Stat
toWhile ast = 
    evalState (toWhile' ast') (T.getStrictUnusedVars (getVarNames (toExtended ast')))
  where ast' = toStrict ast

-- The reason for the use of the state monad is that the transformation of
-- a goto program to a while program needs a previously unused variable and the
-- list of unusued variables is carried along in the state. 
toWhile' :: StrictAS.Program -> State [VarIdent] WhileAS.Stat
toWhile' ast = do
    unused <- T.getUnusedVar
    let body = toWhile'' unused ast
    return $ WhileAS.Seq
                 [ WhileAS.Assign unused (WhileAS.Const 1)
                 , WhileAS.While (WhileAS.RelOp "!=" (WhileAS.Var unused) (WhileAS.Const 0)) body
                 ]

-- The AST is first made strict and only then transformed to While because it
-- is much easier to transform a strict Goto program than an extended.
toWhile'' :: VarIdent -> StrictAS.Program -> WhileAS.Stat 
toWhile'' x (StrictAS.Assign l i j op c) = 
    WhileAS.If (WhileAS.RelOp "=" (WhileAS.Var x) (WhileAS.Const l)) (WhileAS.Seq
        [ WhileAS.Assign ('x':show i) (WhileAS.AOp (show op) (WhileAS.Var ('x':show j)) (WhileAS.Const c))
        , WhileAS.Assign x (WhileAS.AOp "+" (WhileAS.Var x) (WhileAS.Const 1))
        ]) Nothing
toWhile'' x (StrictAS.Goto l1 l2) = 
    WhileAS.If (WhileAS.RelOp "=" (WhileAS.Var x) (WhileAS.Const l1)) ( 
        WhileAS.Assign x (WhileAS.Const l2)
        ) Nothing
toWhile'' x (StrictAS.Halt l) = 
    WhileAS.If (WhileAS.RelOp "=" (WhileAS.Var x) (WhileAS.Const l)) ( 
        WhileAS.Assign x (WhileAS.Const 0)
        ) Nothing
toWhile'' x (StrictAS.IfGoto l1 i c l2) = 
    WhileAS.If (WhileAS.RelOp "=" (WhileAS.Var x) (WhileAS.Const l1)) (
        WhileAS.If (WhileAS.RelOp "=" (WhileAS.Var ('x':show i)) (WhileAS.Const c))
              (WhileAS.Assign x (WhileAS.Const l2))
              (Just (WhileAS.Assign x (WhileAS.AOp "+" (WhileAS.Var x) (WhileAS.Const 1))))
        ) Nothing
toWhile'' x (StrictAS.Seq stats) = WhileAS.Seq $ map (toWhile'' x) stats
