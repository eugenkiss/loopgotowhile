-- | Shared data constructors (abstract syntax) like boolean & arithmetic
-- expressions.
module Language.LoopGotoWhile.Shared.ExtendedAS
    ( VarIdent
    , AExp (..) 
    , BExp (..) 
    ) where


type VarIdent = String

data AExp 
    = Var VarIdent
    | Const Integer
    | AOp String AExp AExp
    deriving Eq

data BExp
    = BNegOp BExp
    | BOp String BExp BExp
    | RelOp String AExp AExp
    deriving Eq


instance Show AExp where
    show (Var v)       = v
    show (Const i)     = show i
    show (AOp s a1 a2) = "(" ++ show a1 ++ " " ++ s ++ " " ++ show a2 ++ ")"

instance Show BExp where
    show (BNegOp b)      = "!(" ++ show b ++ ")"
    show (BOp s b1 b2)   = "(" ++ show b1 ++ " " ++ s ++ " " ++ show b2 ++ ")"
    show (RelOp s a1 a2) = "(" ++ show a1 ++ " " ++ s ++ " " ++ show a2 ++ ")"
