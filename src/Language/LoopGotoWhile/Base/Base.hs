module Language.LoopGotoWhile.Base.Base
    ( Constant
    , Index
    , Variable (..)
    , Operator (..)
    , Assignment (..)
    , Env
    , parser
    , evaluator
    , parseConst
    , parseVar
    , parseOp
    , parseAssignment
    , parseAssignments
    , evalAssignment
    , evalAssignments
    , nullEnv
    , getVar
    , setVar
    ) where

import Control.Monad
import Control.Monad.ST
import Data.STRef
import Data.Maybe (fromJust)
import Text.ParserCombinators.Parsec

import Language.LoopGotoWhile.Util (makeStdParser)


parser :: String -> Either String [Assignment]
parser = makeStdParser parseAssignments

evaluator :: [Assignment] -> [Integer] -> Integer
evaluator ast args = runST $ do
    envRef <- nullEnv
    forM_ [1..length args] $ \i ->
        setVar envRef (toInteger i) (args !! (i-1))
    evalAssignments envRef ast
    getVar envRef 0


type Constant = Integer

type Index = Integer

data Variable = Variable Index

data Operator = Plus | Minus

data Assignment = Assignment Variable Variable Operator Constant

parseConst :: Parser Constant
parseConst = liftM read (many1 digit <?> "constant")

parseVar :: Parser Variable
parseVar = liftM (Variable . read) (char 'x' >> many1 (digit <?> "") <?> "variable")

parseOp :: Parser Operator
parseOp = do
    op <- oneOf "+-"
    case op of
      '+' -> return Plus
      '-' -> return Minus
      _   -> fail "Wrong operator"

parseAssignment :: Parser Assignment
parseAssignment = do
    spaces
    x <- parseVar
    spaces
    _ <- string ":="
    spaces
    y <- parseVar
    spaces
    o <- parseOp
    spaces
    c <- parseConst
    spaces
    return $ Assignment x y o c

parseAssignments :: Parser [Assignment]
parseAssignments = parseAssignment `sepBy` string ";"


showAssignment :: Assignment -> String
showAssignment (Assignment (Variable i) (Variable j) (Plus) (c)) =
    showAssignment' i j "+" c
showAssignment (Assignment (Variable i) (Variable j) (Minus) (c)) =
    showAssignment' i j "-" c

showAssignment' :: Index -> Index -> String -> Integer -> String
showAssignment' i j op c = 
    "x" ++ show i ++ " := " ++ "x" ++ show j ++ " " ++ op ++ " " ++ show c

instance Show Assignment where show = showAssignment


evalAssignment :: Env s -> Assignment -> ST s ()
evalAssignment env (Assignment (Variable i) (Variable j) (Plus) (c)) = do
    xj <- getVar env j
    setVar env i (xj + c)
evalAssignment env (Assignment (Variable i) (Variable j) (Minus) (c)) = do
    xj <- getVar env j
    setVar env i (max (xj - c) 0)

evalAssignments :: Env s -> [Assignment] -> ST s ()
evalAssignments env assignments = mapM_ (evalAssignment env) assignments


-- TODO: If possible move the following code to Util

type Env s = STRef s [(Index, STRef s Integer)]

-- TODO: Is there a better way to achieve this?
nullEnv :: ST s (Env s)
nullEnv = newSTRef =<< return . zip [0..] =<< lazyRefs
  where lazyRefs = do
          x  <- newSTRef 0
          xs <- unsafeInterleaveST lazyRefs
          return (x:xs)

getVar :: Env s -> Index -> ST s Integer
getVar envRef i = readSTRef envRef >>= readSTRef . fromJust . lookup i

setVar :: Env s -> Index -> Integer -> ST s ()
setVar envRef i v = readSTRef envRef >>= flip writeSTRef v . fromJust . lookup i 
