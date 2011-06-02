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
prettyPrint (Assign l i j op c) = 
    "M" ++ show l ++ ": " ++ 
    "x" ++ show i ++ " := " ++ 
    "x" ++ show j ++ " " ++ show op ++ " " ++ show c
prettyPrint (IfGoto l1 i c l2) = 
    "M" ++ show l1 ++ ": " ++ 
    "IF x" ++ show i ++ " = " ++ show c ++ " THEN GOTO M" ++ show l2 ++ " END"
prettyPrint (Goto l1 l2) = "M" ++ show l1 ++ ": " ++ "GOTO M" ++ show l2 
prettyPrint (Halt l) = "M" ++ show l ++ ": HALT" 
prettyPrint (Seq stats) = intercalate ";\n" . map prettyPrint $ stats 
             
instance Show Stat where
    show = prettyPrint

instance Show Op where
    show Plus  = "+"
    show Minus = "-"
