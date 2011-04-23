module Language.LoopGotoWhile.Base.Base
    ( Constant
    , Index
    , Variable (..)
    , Operator (..)
    , Assignment (..)
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

import System.IO.Unsafe (unsafeInterleaveIO)
import Control.Monad
import Data.IORef
import Data.Maybe (fromJust)
import Text.ParserCombinators.Parsec


parser :: String -> Either String [Assignment]
parser code = case parse parseAssignments "" code of
    Left err  -> Left $ show err
    Right val -> Right val

evaluator :: [Assignment] -> [Integer] -> IO Integer
evaluator ast args = do
    envRef <- nullEnv
    forM_ [1..length args] (\i ->
        setVar envRef (toInteger i) (args !! (i-1)))
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


evalAssignment :: Env -> Assignment -> IO ()
evalAssignment env (Assignment (Variable i) (Variable j) (Plus) (c)) = do
    xj <- getVar env j
    setVar env i (xj + c)
evalAssignment env (Assignment (Variable i) (Variable j) (Minus) (c)) = do
    xj <- getVar env j
    setVar env i (max (xj - c) 0)

evalAssignments :: Env -> [Assignment] -> IO ()
evalAssignments env assignments = mapM_ (evalAssignment env) assignments


type Env = IORef [(Index, IORef Integer)]

-- TODO: Is there a better way to achieve this?
nullEnv :: IO Env
nullEnv = newIORef =<< return . zip [0..] =<< lazyRefs
  where lazyRefs = do
          x  <- newIORef 0
          xs <- unsafeInterleaveIO lazyRefs
          return (x:xs)

getVar :: Env -> Index -> IO Integer
getVar envRef i = readIORef envRef >>= readIORef . fromJust . lookup i

setVar :: Env -> Index -> Integer -> IO ()
setVar envRef i v = readIORef envRef >>= flip writeIORef v . fromJust . lookup i 
