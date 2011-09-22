-- | Data constructors (Abstract Syntax) and pretty printer for strict Goto.
module Language.LoopGotoWhile.Goto.StrictAS 
    ( Stat (..)
    , Op (..)
    , Program
    , Const
    , VIndex
    , LIndex
    , prettyPrint
    ) where

import Data.Monoid
import Data.List (intercalate)


type Program = Stat

type VIndex = Integer -- Variable index

type LIndex = Integer -- Label index

type Const = Integer

data Op = Plus 
        | Minus
        deriving Eq

data Stat
    = Assign LIndex VIndex VIndex Op Const
    | IfGoto LIndex VIndex Const LIndex
    | Goto LIndex LIndex
    | Halt LIndex
    | Seq [Stat]
    deriving Eq


instance Monoid Stat where
    mempty = Seq []
    (Seq stats1) `mappend` (Seq stats2) = Seq $ stats1 ++ stats2
    (Seq stats)  `mappend` stat         = Seq $ stats ++ [stat]
    stat         `mappend` (Seq stats)  = Seq $ stat : stats
    stat1        `mappend` stat2        = Seq $ [stat1, stat2]

instance Show Stat where
    show = prettyPrint

instance Show Op where
    show Plus  = "+"
    show Minus = "-"


-- | Return a standard string representation of a strict Goto AST.
prettyPrint :: Program -> String
prettyPrint (Assign l i j op c) = 
    "M" ++ show l ++ ": " ++ 
    "x" ++ show i ++ " := " ++ 
    "x" ++ show j ++ " " ++ show op ++ " " ++ show c
prettyPrint (IfGoto l1 i c l2) = 
    "M" ++ show l1 ++ ": " ++ 
    "IF x" ++ show i ++ " = " ++ show c ++ " THEN GOTO M" ++ show l2
prettyPrint (Goto l1 l2) = "M" ++ show l1 ++ ": " ++ "GOTO M" ++ show l2 
prettyPrint (Halt l) = "M" ++ show l ++ ": HALT" 
prettyPrint (Seq stats) = intercalate ";\n" . map prettyPrint $ stats 
