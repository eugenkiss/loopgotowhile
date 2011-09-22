-- | Data constructors (Abstract Syntax) and pretty printer for extended Goto.
module Language.LoopGotoWhile.Goto.ExtendedAS
    ( Program
    , VarIdent
    , LIdent
    , Stat (..)
    , AExp (..)
    , BExp (..)
    , prettyPrint
    ) where

import Data.Monoid
import Data.List (intercalate)

import Language.LoopGotoWhile.Shared.ExtendedAS (VarIdent, AExp(..), BExp(..))


type Program = Stat

type LIdent = String

data Stat
    = Assign VarIdent AExp
    | If BExp Stat (Maybe Stat)
    | Goto LIdent
    | Halt
    | Label LIdent Stat
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


-- | Return a standard string representation of an extended While AST.
prettyPrint :: Program -> String
prettyPrint (Assign v a) = v ++ " := " ++ show a
prettyPrint (If b statThen Nothing) =
    "IF " ++ show b ++ " THEN\n" ++
    prettyPrint statThen ++ "\n" ++
    "END"
prettyPrint (If b statThen (Just statElse)) = 
    "IF " ++ show b ++ " THEN\n" ++
    prettyPrint statThen ++ "\n" ++
    "ELSE\n" ++
    prettyPrint statElse ++ "\n" ++
    "END"
prettyPrint (Goto l)       = "GOTO " ++ l 
prettyPrint (Halt)         = "HALT" 
prettyPrint (Label l stat) = l ++ ": " ++ prettyPrint stat
prettyPrint (Seq stats)    = intercalate ";\n" . map prettyPrint $ stats
