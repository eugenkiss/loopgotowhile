-- | Parsing and evaluation of strict Loop.
module Language.LoopGotoWhile.Loop.Strict
    ( run
    , eval
    , parse
    , prettyPrint
    ) where

import Control.Monad

import Text.ParserCombinators.Parsec hiding (parse)

import Language.LoopGotoWhile.Shared.Util (mkStdParser, mkStdRunner)
import Language.LoopGotoWhile.Loop.StrictAS
import Language.LoopGotoWhile.Loop.Transform (toExtended, toWhile)
import qualified Language.LoopGotoWhile.While.Strict as WhileS
import qualified Language.LoopGotoWhile.While.Transform as WhileT


-- * Main Functions
--   ==============

-- | Given a string representation of a strict Loop program and a list of
-- arguments parse & evaluate the program and return either an error string or
-- the value of 'x0'.
run :: String -> [Integer] -> Either String Integer
run = mkStdRunner parse eval

-- | Given a strict Loop AST and a list of arguments evaluate the program
-- and return the value of 'x0'.
eval :: Program -> [Integer] -> Integer
eval ast = WhileS.eval (WhileT.toStrict . toWhile . toExtended $ ast)

-- | Given a string representation of a strict Loop program parse it and
-- return either an error string or the AST.
parse :: String -> Either String Program
parse = mkStdParser parseStats () spaces


-- * Parsing
--   =======

parseStats :: Parser Program
parseStats = do
    stats <- parseStat `sepBy1` (string ";" >> spaces)
    return $ case stats of
               [x] -> x
               x   -> Seq x
  where parseStat = parseAssign <|> parseLoop

parseLoop :: Parser Stat
parseLoop = do
    _ <- string "LOOP"
    spaces
    x <- parseVar
    spaces
    _ <- string "DO"
    spaces
    body <- parseStats
    spaces
    _ <- string "END"
    return $ Loop x body

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

parseVar :: Parser Index
parseVar = liftM read (char 'x' >> many1 (digit <?> "") <?> "variable")

parseConst :: Parser Const
parseConst = liftM read (many1 digit <?> "constant")
