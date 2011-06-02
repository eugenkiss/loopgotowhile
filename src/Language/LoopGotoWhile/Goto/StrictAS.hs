module Language.LoopGotoWhile.Goto.StrictAS 
    ( Stat (..)
    , Op (..)
    , Program
    , Const
    , VIndex
    , LIndex
    , prettyPrint
    ) where

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


-- | Return a standard string representation of a strict Goto AST.
prettyPrint :: Program -> String
prettyPrint = prettyPrint' ""
  where prettyPrint' s (Assign l i j op c) = s ++ "M" ++ show l ++ ": " ++ 
            "x" ++ show i ++ " := " ++ 
            "x" ++ show j ++ " " ++ show op ++ " " ++ show c
        prettyPrint' s (IfGoto l1 i c l2) = s ++ "M" ++ show l1 ++ ": " ++ 
            "IF x" ++ show i ++ " = " ++ show c ++ " THEN GOTO M" ++ show l2 ++ " END"
        prettyPrint' s (Goto l1 l2) = s ++ "M" ++ show l1 ++ ": " ++ 
            "GOTO M" ++ show l2 
        prettyPrint' s (Halt l) = s ++ "M" ++ show l ++ ": HALT" 
        prettyPrint' s (Seq stats) = 
            intercalate ";\n" . map (prettyPrint' "") $ stats 
             
instance Show Stat where
    show = prettyPrint

instance Show Op where
    show Plus  = "+"
    show Minus = "-"
