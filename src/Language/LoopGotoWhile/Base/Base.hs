module Language.LoopGotoWhile.Base.Base
    ( Constant
    , Index
    , Variable (..)
    , Operator (..)
    , Statement (..)
    , Env
    , runStatements
    , runProgram
    , parseConst
    , parseVar
    , parseOp
    , parseStatement
    , parseProgram
    , eval
    , nullEnv
    , getVar
    , setVar
    ) where

import System
import System.IO.Unsafe (unsafeInterleaveIO)
import Control.Monad
import Data.IORef
import Data.List (intercalate)
import Data.Maybe (fromJust)
import Text.ParserCombinators.Parsec


runStatements :: Statement -> [Integer] -> IO Integer
runStatements stmnts args = do
    envRef <- nullEnv
    forM_ [1..length args] (\i ->
        setVar envRef (toInteger i) (args !! (i-1)))
    eval envRef stmnts
    getVar envRef 0

runProgram :: String -> [Integer] -> IO (Either String Integer)
runProgram code args = case parse parseProgram "base" code of
    Left err  -> return $ Left $ show err
    Right val -> liftM Right $ runStatements val args


type Constant = Integer

type Index = Integer

data Variable = Variable Index

data Operator = Plus | Minus

data Statement = Assignment Variable Variable Operator Constant
               | Sequence [Statement]

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

parseStatement :: Parser Statement
parseStatement = do
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

parseProgram :: Parser Statement
parseProgram = liftM Sequence $ parseStatement `sepBy` string ";"


showStatement :: Statement -> String
showStatement (Assignment (Variable i) (Variable j) (Plus) (c)) =
    showStatement' i j "+" c
showStatement (Assignment (Variable i) (Variable j) (Minus) (c)) =
    showStatement' i j "-" c
showStatement (Sequence stmnts) = 
    intercalate "; " $ map showStatement stmnts

showStatement' :: Index -> Index -> String -> Integer -> String
showStatement' i j op c = 
    "x" ++ show i ++ " := " ++ "x" ++ show j ++ " " ++ op ++ " " ++ show c

instance Show Statement where show = showStatement


eval :: Env -> Statement -> IO ()
eval env (Assignment (Variable i) (Variable j) (Plus) (c)) = do
    xj <- getVar env j
    setVar env i (xj + c)
eval env (Assignment (Variable i) (Variable j) (Minus) (c)) = do
    xj <- getVar env j
    setVar env i (max (xj - c) 0)
eval env (Sequence stmnts) = mapM_ (eval env) stmnts 


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
