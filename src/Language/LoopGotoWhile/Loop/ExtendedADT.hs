module Language.LoopGotoWhile.Loop.ExtendedADT
    ( Program
    , VarIdent
    , AExp (..) -- TODO: Move to core
    , BExp (..) -- TODO: Move to core
    , Stat (..)
    ) where


type Program = Stat

type VarIdent = String

-- TODO: Function definition

data AExp 
    = Var VarIdent
    | Const Integer
    | AOp String AExp AExp
    deriving (Show, Eq)

data BExp
    = BNegOp BExp
    | BOp String BExp BExp
    | RelOp String AExp AExp
    deriving (Show, Eq)

data Stat
    = Assign VarIdent AExp
    | Loop AExp Stat
    | If BExp Stat (Maybe Stat)
    | Seq [Stat]
    deriving (Show, Eq)
