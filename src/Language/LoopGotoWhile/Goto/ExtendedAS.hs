module Language.LoopGotoWhile.Goto.ExtendedAS
    ( Program
    , VarIdent
    , LIdent
    , Stat (..)
    , AExp (..)
    , BExp (..)
    , prettyPrint
    ) where

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


-- | Return a standard string representation of an extended While AST.
prettyPrint :: Program -> String
prettyPrint = prettyPrint' ""
  where prettyPrint' s (Assign v a) = s ++
            v ++ " := " ++ show a
        prettyPrint' s (If b statThen Nothing) = s ++
            "IF " ++ show b ++ " THEN\n" ++
            prettyPrint' "" statThen ++ "\n" ++
            "END"
        prettyPrint' s (If b statThen (Just statElse)) = s ++
            "IF " ++ show b ++ " THEN\n" ++
            prettyPrint' "" statThen ++ "\n" ++
            "ELSE\n" ++
            prettyPrint' "" statElse ++ "\n" ++
            "END"
        prettyPrint' s (Goto l) = s ++ "GOTO " ++ l 
        prettyPrint' s Halt = s ++ "HALT" 
        prettyPrint' s (Label l stat) = s ++
            l ++ ": " ++ prettyPrint' "" stat
        prettyPrint' s (Seq stats) = s ++
            (intercalate ";\n" . map (prettyPrint' "") $ stats)
             
instance Show Stat where
    show = prettyPrint
