-- | Functions for transforming Goto into While or into a semantically
-- equivalent strict subset.
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
import Data.Char (isDigit)
import Data.List ((\\), nub)

import qualified Language.LoopGotoWhile.While.ExtendedAS as While
import qualified Language.LoopGotoWhile.Goto.StrictAS as Strict
import qualified Language.LoopGotoWhile.Shared.Transform as T
import Language.LoopGotoWhile.Goto.ExtendedAS
import Language.LoopGotoWhile.Shared.Transform hiding (getUnusedVar)


-- * Transformation from strict Goto to extended Goto
--   ================================================

-- | Transform a strict syntax tree into an extended one without changing the
-- semantics of the program.
-- 
-- Since the strict syntax is a subset of the extended syntax this functions
-- merely translates the strict data constructors to the extended ones in
-- a very direct manner. This function is only needed to make some
-- transformations / function compositions easier.
toExtended :: Strict.Stat -> Stat
toExtended (Strict.Assign l i j Strict.Plus c) = Label (indexToLabel l) $ 
    Assign ('x' : show i) (AOp "+" (Var ('x' : show j)) (Const c))
toExtended (Strict.Assign l i j Strict.Minus c) = Label (indexToLabel l) $
    Assign ('x' : show i) (AOp "-" (Var ('x' : show j)) (Const c))
toExtended (Strict.IfGoto l1 i c l2) = Label (indexToLabel l1) $
    If (RelOp "=" (Var ('x' : show i)) (Const c)) (Goto (indexToLabel l2)) Nothing
toExtended (Strict.Goto l1 l2) = 
    Label (indexToLabel l1) $ Goto (indexToLabel l2)
toExtended (Strict.Halt l) = Label (indexToLabel l) Halt
toExtended (Strict.Seq stats) = Seq $ map toExtended stats

indexToLabel :: Strict.LIndex -> LIdent
indexToLabel i = 'M' : show i


-- * Transformation from extended Goto to strict Goto
--   ================================================

-- | Transform an extended syntax tree into a strict one without changing the
-- semantics of the program and trying to reduce redundant statements.
toStrict :: Stat -> Strict.Stat
toStrict = toStrictGoto 
         . flatten
         . strictifyUndefLabels
         . toStrictLabels
         . removeRedundancy
         -- It is important that addHalt comes before removeRedundancy due to
         -- several special cases, e.g. a program which ends with a redundant
         -- statement like "x0 := x0 + 0", which would be removed and GOTOs
         -- pointing to that label would point into nothing (undefined). With
         -- a HALT statement this case cannot happen. 
         . addHalt
         . toStrictLabels
         . toStrictStat 
         . toStrictVars

-- | Transform an extended syntax tree into a strict one. Assume that the AST
-- is given in a directly translatable form.
toStrictGoto :: Stat -> Strict.Stat
toStrictGoto (Label l (Assign (_:i) (AOp "+" (Var (_:j)) (Const c)))) =
    Strict.Assign (labelToIndex l) (read i) (read j) Strict.Plus c
toStrictGoto (Label l (Assign (_:i) (AOp "-" (Var (_:j)) (Const c)))) =
    Strict.Assign (labelToIndex l) (read i) (read j) Strict.Minus c
toStrictGoto (Label l1 (If (RelOp "=" (Var (_:i)) (Const c)) (Goto l2) Nothing)) =
    Strict.IfGoto (labelToIndex l1) (read i) c (labelToIndex l2)
toStrictGoto (Label l1 (Goto l2)) = Strict.Goto (labelToIndex l1) (labelToIndex l2)
toStrictGoto (Label l Halt) = Strict.Halt (labelToIndex l)
toStrictGoto (Seq stats) = Strict.Seq (map toStrictGoto stats)
toStrictGoto ast = error $ "Extended AST is not in strict form: " ++ show ast

labelToIndex :: LIdent -> Strict.LIndex
labelToIndex (_:i) = read i
labelToIndex _     = error "Impossible"

-- | Unwrap the singleton sequence.
flatten :: Stat -> Stat
flatten (Seq [x]) = x
flatten x         = x

-- | Add a Halt statement to the end if needed.
addHalt :: Stat -> Stat
addHalt (Seq stats) 
  | null stats = Seq [haltStmnt]
  | otherwise  = case lastStat of
        Label _ (Goto _) -> Seq stats
        Label _ Halt     -> Seq stats
        _                -> Seq $ stats ++ [haltStmnt]
  where l         = length stats
        lastStat  = last stats
        haltStmnt = Label ('M' : show (l + 1)) Halt
addHalt stat = addHalt $ Seq [stat]
{-addHalt _ = error "Impossible! Must be a sequence!"-}


-- ** Renaming of Labels
--    ------------------

-- | Transform GOTOs with an undefined label to "M1".
strictifyUndefLabels :: Stat -> Stat
strictifyUndefLabels ast = case ast of
    Assign var aexp      -> Assign var aexp
    Halt                 -> Halt
    Goto l               -> Goto (r l)
    If bexp s Nothing    -> If bexp (sUL s) Nothing
    If bexp s1 (Just s2) -> If bexp (sUL s1) (Just (sUL s2))
    Label l stat         -> Label l (sUL stat)
    Seq stats            -> Seq (map sUL stats)
  where sUL = strictifyUndefLabels
        r l | isStrictLab l = l | otherwise = "M1"
        isStrictLab (x:xs) | not (null xs) = x == 'M' && all isDigit xs
        isStrictLab _      = False

-- | Rename and reorder the labels in such a way that they are successive and
-- the first label is "M1". Assume that the AST is already flat.
toStrictLabels :: Stat -> Stat
toStrictLabels (Seq stats) =
    let (stats', mappings) = foldr step ([], []) $ zip stats labelStream
    in  renameGotoLabels mappings (Seq stats')
  where step (stat@(Label l s), l') (ss, mappings) 
            | l == l'   = (stat        :ss,         mappings)
            | otherwise = ((Label l' s):ss, (l, l'):mappings)
        step (s, l') (ss, mappings) 
                        = ((Label l' s):ss,         mappings)
toStrictLabels stat = toStrictLabels (Seq [stat])

-- | Given a list of disjunctive rename mappings in the form [...,(from_i,
-- to_i),...] rename all occurences of 'from_i' as a label name to 'to_i' in
-- the given AST. Do not rename the labels denoting the statement, however.
renameGotoLabels :: [(LIdent, LIdent)] -> Stat -> Stat
renameGotoLabels mappings (Seq stats) = Seq $ map (rLab mappings) stats
  where rLab as@((from,to):rs) stat = case stat of
            Goto l               -> if l == from 
                                       then Goto to
                                       else rLab rs $ Goto l
            If bexp s Nothing    -> If bexp (rLab as s) Nothing
            If bexp s1 (Just s2) -> If bexp (rLab as s1) (Just (rLab as s2))
            Label l s            -> Label l (rLab as s)
            Seq _                -> error "Impossible" 
            x                    -> x
        rLab [] stat = stat
renameGotoLabels rs x = renameGotoLabels rs $ Seq [x]

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

-- | Create an infinite list of consecutive, strict labels beginning with 'M1'.
labelStream :: [LIdent]
labelStream = iterate (\l -> 'M' : succ' (tail l)) "M1"
  where succ' s = show $ stoi s + 1
        stoi :: String -> Integer
        stoi = read

-- | Analyze the AST and return a list without duplicates of all used label
-- names.
getLabelNames :: Stat -> [LIdent]
getLabelNames = nub . f
  where f (Assign _ _)        = []
        f (Halt)              = []
        f (If _ s Nothing)    = f s
        f (If _ s1 (Just s2)) = f s1 ++ f s2
        f (Goto l)            = [l] 
        f (Label l stat)      = l : f stat
        f (Seq stats)         = concatMap f stats


-- ** Remove Redundancy
--    -----------------

-- | During transformation some "NOPs" (like "x0 := x0 + 0") are used here and
-- there to keep the transformation and thus the code simpler. However, as
-- these statements are redundant they should be removed so as to unclutter the
-- transformed code. Assume that the AST is in strict form and that it is
-- a sequence of statements if only the singleton sequence.
removeRedundancy :: Stat -> Stat
removeRedundancy ast = removeRedundantStats . renameGotoLabels mappings $ ast
  where mappings = map (\(i,j) -> ('M':show i, 'M':show j)) 
                 . relabelMappings 0 1 . reverse . naiveRelabelMappings $ ast
        -- relabelMappings is needed for the case of several successive "NOPs"
        -- like "M3: x0 := x0 + 0; M4: x0 := x0 + 0; M5 := x0 := x0 + 0" (here
        -- n would be 3 when the recursion reaches M3).
        relabelMappings _    _ []              = []
        relabelMappings lastIndex n ((i,j):xs) = 
            if lastIndex - j == 1
               then (i, j+n) : relabelMappings j (n+1) xs
               else (i, j)   : relabelMappings j 1     xs
        naiveRelabelMappings = map (\l -> (l, succ l)) . getRedundantLabelIndices

removeRedundantStats :: Stat -> Stat
removeRedundantStats (Seq stats) = Seq $ filter (not . isRedundant) $ stats
removeRedundantStats _ = error $ "This should be impossible"

getRedundantLabelIndices :: Stat -> [Integer]
getRedundantLabelIndices (Seq stats) = 
    map extractIndex . getLabelNames . Seq . filter isRedundant $ stats
  where extractIndex (_:i) = (read i :: Integer)
        extractIndex _     = error $ "This should be impossible"
getRedundantLabelIndices _ = error $ "This should be impossible"

isRedundant :: Stat -> Bool
isRedundant (Label _ (Assign v0 (AOp _ (Var v1) (Const 0)))) 
    | v0 == v1  = True
    | otherwise = False
isRedundant _ = False


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
    Halt                 -> Halt
    Goto l               -> Goto l
    Assign var aexp      -> Assign (r var) (rAExp aexp)
    If bexp s1 Nothing   -> If (rBExp bexp) (rVar s1) Nothing
    If bexp s1 (Just s2) -> If (rBExp bexp) (rVar s1) (Just (rVar s2))
    Label l stat         -> Label l (rVar stat)
    Seq stats            -> Seq (map rVar stats)
  where rVar  = renameVar from to
        rAExp = renameVarInAExp from to
        rBExp = renameVarInBExp from to
        r var | var == from = to | otherwise = var
     
-- | Analyze the AST and return a list without duplicates of all used variable
-- names.
getVarNames :: Stat -> [VarIdent]
getVarNames = nub . f
  where f (Halt)                 = []
        f (Goto _)               = []
        f (Assign var aexp)      = var : getVarNamesInAExp aexp
        f (If bexp s1 Nothing)   = getVarNamesInBExp bexp ++ f s1
        f (If bexp s1 (Just s2)) = getVarNamesInBExp bexp ++ f s1 ++ f s2
        f (Label _ stat)         = f stat
        f (Seq stats)            = concatMap f stats


-- ** Statements
--    ----------

-- | Transform an extended Goto AST into a strict Goto AST. Assume that all
-- variables are already strict.
toStrictStat :: Stat -> Stat
toStrictStat ast =
    evalState (
      evalStateT (toStrictStat' $ ast) (getStrictUnusedLabels (getLabelNames ast))
    ) (T.getStrictUnusedVars (getVarNames ast))

-- Since sometimes an unused variable/label is needed in order to correctly
-- transform a statement two infinite lists of unused variable names and unused
-- label names are carried along as state that gets updated whenever an unused
-- variable resp. label name is requested.
type TransformState = StateT [LIdent] (State [VarIdent])

-- Lift getUnusedVar because of Monad Transformation Madness!
getUnusedVar :: MonadTrans t => t (State [VarIdent]) VarIdent
getUnusedVar = lift T.getUnusedVar

toStrictStat' :: Stat -> TransformState Stat
-- GOTO Mx. Keep unchanged!
toStrictStat' stat@(Goto _) = return stat
-- HALT. Keep unchanged!
toStrictStat' Halt = return Halt
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
              toStrictStat' $ Seq 
                  [ Assign c (Const 0)
                  , Assign v0 (Var v1)
                  , ltw
                  , Assign v0 (Var c)
                  ]
    "%" -> do ltw <- loopToGoto $ Loop (Var v0) (If (RelOp ">=" (Var v0) (Var v2))
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
    _    -> error "Impossible! Wrong operator!"
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
    toStrictStat' $ Seq
        [ Assign u (Const 1)
        , If a (If b (Seq [Assign u (Const 0), stat1]) Nothing) Nothing
        , ltw
        ]
toStrictStat' (If (BOp "||" a b) stat1 Nothing) = do
    u <- getUnusedVar
    ltw <- loopToGoto $ Loop (Var u) stat1
    toStrictStat' $ Seq
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
-- Mx: P
toStrictStat' s@(Label _ (Assign "x0" (AOp "+" (Var "x0") (Const 0)))) = 
    return s
toStrictStat' (Label l stat) = toStrictStat' $ Seq
    [ Label l $ Assign "x0" $ AOp "+" (Var "x0") (Const 0) -- NOP 
    , stat
    ]
-- P1; P2;...
toStrictStat' (Seq stats) = liftM (Seq . flatten') $ mapM toStrictStat' stats
    where flatten' = foldr f []
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
        , Label mx $ Assign c (Var c) -- NOP
        ]


-- * Transformation to While
--   =======================

-- | Transform a Goto AST to a While AST.
toWhile :: Program -> While.Stat
toWhile ast = 
    evalState (toWhile' ast') (T.getStrictUnusedVars (getVarNames (toExtended ast')))
  where ast' = toStrict ast

-- The reason for the use of the state monad is that the transformation of
-- a goto program to a while program needs a previously unused variable and the
-- list of unusued variables is carried along in the state. 
toWhile' :: Strict.Program -> State [VarIdent] While.Stat
toWhile' ast = do
    unused <- T.getUnusedVar
    let body = toWhile'' unused ast
    return $ While.Seq
                 [ While.Assign unused (While.Const 1)
                 , While.While (While.RelOp "!=" (While.Var unused) (While.Const 0)) body
                 ]

-- The AST is first made strict and only then transformed to While because it
-- is much easier to transform a strict Goto program than an extended.
toWhile'' :: VarIdent -> Strict.Program -> While.Stat 
toWhile'' x (Strict.Assign l i j op c) = 
    While.If (While.RelOp "=" (While.Var x) (While.Const l)) (While.Seq
        [ While.Assign ('x':show i) (While.AOp (show op) (While.Var ('x':show j)) (While.Const c))
        , While.Assign x (While.AOp "+" (While.Var x) (While.Const 1))
        ]) Nothing
toWhile'' x (Strict.Goto l1 l2) = 
    While.If (While.RelOp "=" (While.Var x) (While.Const l1)) ( 
        While.Assign x (While.Const l2)
        ) Nothing
toWhile'' x (Strict.Halt l) = 
    While.If (While.RelOp "=" (While.Var x) (While.Const l)) ( 
        While.Assign x (While.Const 0)
        ) Nothing
toWhile'' x (Strict.IfGoto l1 i c l2) = 
    While.If (While.RelOp "=" (While.Var x) (While.Const l1)) (
        While.If (While.RelOp "=" (While.Var ('x':show i)) (While.Const c))
              (While.Assign x (While.Const l2))
              (Just (While.Assign x (While.AOp "+" (While.Var x) (While.Const 1))))
        ) Nothing
toWhile'' x (Strict.Seq stats) = While.Seq $ map (toWhile'' x) stats
