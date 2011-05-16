module Language.LoopGotoWhile.While.ExtendedAS
    ( Program
    , VarIdent
    , Stat (..)
    , AExp (..) 
    , BExp (..)
    , prettyPrint
    ) where

import Data.List (intercalate)

import Language.LoopGotoWhile.Common.ExtendedAS (AExp(..), BExp(..))


type Program = Stat

type VarIdent = String

-- TODO: Function definition

data Stat
    = Assign VarIdent AExp
    | While BExp Stat
    | If BExp Stat (Maybe Stat)
    | Seq [Stat]
    deriving Eq


-- | Return a standard string representation of an extended While AST.
prettyPrint :: Program -> String
prettyPrint = prettyPrint' 0 ""
  where prettyPrint' indentSize s (Assign v a) = s ++ 
            indent indentSize ++ v ++ " := " ++ show a
        prettyPrint' indentSize s (While b stat) = s ++ 
            indent indentSize ++ "WHILE " ++ show b ++ " DO\n" ++ 
            prettyPrint' (indentSize + tabSize) "" stat ++ "\n" ++
            indent indentSize ++ "END"
        prettyPrint' indentSize s (If b statThen Nothing) = s ++ 
            indent indentSize ++ "IF " ++ show b ++ " THEN\n" ++ 
            prettyPrint' (indentSize + tabSize) "" statThen ++ "\n" ++
            indent indentSize ++ "END"
        prettyPrint' indentSize s (If b statThen (Just statElse)) = s ++ 
            indent indentSize ++ "IF " ++ show b ++ " THEN\n" ++ 
            prettyPrint' (indentSize + tabSize) "" statThen ++ "\n" ++
            indent indentSize ++ "ELSE\n" ++
            prettyPrint' (indentSize + tabSize) "" statElse ++ "\n" ++
            indent indentSize ++ "END"
        prettyPrint' indentSize s (Seq stats) = s ++
            (intercalate ";\n" . map (prettyPrint' indentSize "") $ stats) 

        indent size = replicate size ' '
        tabSize     = 2
             
instance Show Stat where
    show = prettyPrint
