-- | Parsing and evaluation of strict While.
module Language.LoopGotoWhile.While.Strict 
    ( run
    , eval
    , parse
    , prettyPrint
    ) where

import Control.Monad
import Control.Monad.ST

import Text.ParserCombinators.Parsec hiding (parse)

import Language.LoopGotoWhile.Shared.Util (mkStdParser, mkStdRunner)
import Language.LoopGotoWhile.Shared.Evaluation (Env, nullEnv, getVar, setVar)
import Language.LoopGotoWhile.While.StrictAS


-- * Main Functions
--   ==============

-- | Given a string representation of a strict While program and a list of
-- arguments parse & evaluate the program and return either an error string or
-- the value of 'x0'.
run :: String -> [Integer] -> Either String Integer
run = mkStdRunner parse eval

-- | Given a strict While AST and a list of arguments evaluate the program
-- and return the value of 'x0'.
eval :: Program -> [Integer] -> Integer
eval ast args = runST $ do
    envRef <- nullEnv
    forM_ [1..length args] $ \i ->
        setVar envRef (toInteger i) (args !! (i-1))
    eval' envRef ast
    getVar envRef 0

-- | Given a string representation of a strict While program parse it and
-- return either an error string or the AST.
parse :: String -> Either String Program
parse = mkStdParser parseStats () spaces


-- * Evaluation
--   ==========

eval' :: Env s -> Stat -> ST s ()
eval' env (Assign i j Plus c) = do
    xj <- getVar env j
    setVar env i $! (xj + c)
eval' env (Assign i j Minus c) = do
    xj <- getVar env j
    setVar env i $! (max (xj - c) 0)
eval' env w@(While n stat) = do
    xn <- getVar env n
    unless (xn == 0) $ eval' env stat >> eval' env w
eval' env (Seq stats) = mapM_ (eval' env) stats


-- * Parsing
--   =======

parseStats :: Parser Program
parseStats = do
    stats <- parseStat `sepBy1` (string ";" >> spaces)
    return $ case stats of
               [x] -> x
               x   -> Seq x
  where parseStat = parseAssign <|> parseWhile

parseWhile :: Parser Stat
parseWhile = do
    _ <- string "WHILE"
    spaces
    x <- parseVar
    spaces
    _ <- string "!="
    spaces
    _ <- string "0"
    spaces
    _ <- string "DO"
    spaces
    body <- parseStats
    spaces
    _ <- string "END"
    return $ While x body

parseAssign :: Parser Stat
parseAssign = do
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
    return $ Assign x y o c

parseOp :: Parser Op
parseOp = do
    op <- oneOf "+-"
    case op of
      '+' -> return Plus
      '-' -> return Minus
      _   -> fail "Wrong operator"

parseConst :: Parser Const
parseConst = liftM read (many1 digit <?> "constant")

parseVar :: Parser Index
parseVar = liftM read (char 'x' >> many1 (digit <?> "") <?> "variable")
