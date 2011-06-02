-- | Data constructors (Abstract Syntax) and pretty printer for strict Loop.
module Language.LoopGotoWhile.Loop.StrictAS 
    ( Stat (..)
    , Op (..)
    , Program
    , Const
    , Index
    , prettyPrint
    ) where

import Data.List (intercalate)

type Program = Stat

type Index = Integer

type Const = Integer

data Op = Plus 
        | Minus
        deriving Eq

data Stat
    = Assign Index Index Op Const
    | Loop Index Stat
    | Seq [Stat]
    deriving Eq


prettyPrint :: Program -> String
prettyPrint = prettyPrint' 0 
  where prettyPrint' indentSize (Assign i j op c) = 
            indent indentSize ++ "x" ++ show i ++ " := " ++ 
            "x" ++ show j ++ " " ++ show op ++ " " ++ show c
        prettyPrint' indentSize (Loop i stat) =  
            indent indentSize ++ "LOOP x" ++ show i ++ " DO\n" ++ 
            prettyPrint' (indentSize + tabSize) stat ++ "\n" ++
            indent indentSize ++ "END"
        prettyPrint' indentSize (Seq stats) = 
            intercalate ";\n" . map (prettyPrint' indentSize) $ stats
        indent size = replicate size ' '
        tabSize     = 2

instance Show Stat where
    show = prettyPrint

instance Show Op where
    show Plus  = "+"
    show Minus = "-"
