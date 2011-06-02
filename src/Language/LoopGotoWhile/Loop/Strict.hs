module Language.LoopGotoWhile.Loop.Strict
    ( eval
    , parse
    , prettyPrint
    ) where

import Control.Monad
import Control.Monad.ST
import Data.STRef
import Data.Maybe (fromJust)
import Text.ParserCombinators.Parsec hiding (parse)

import Language.LoopGotoWhile.Util (mkStdParser)
import Language.LoopGotoWhile.Loop.StrictAS
import Language.LoopGotoWhile.Loop.Transform (toExtended, toWhile)
import qualified Language.LoopGotoWhile.While.Strict as WhileS
import qualified Language.LoopGotoWhile.While.Transform as WhileT


-- * Main Functions
--   ==============

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

parseConst :: Parser Const
parseConst = liftM read (many1 digit <?> "constant")

parseVar :: Parser Index
parseVar = liftM read (char 'x' >> many1 (digit <?> "") <?> "variable")

parseOp :: Parser Op
parseOp = do
    op <- oneOf "+-"
    case op of
      '+' -> return Plus
      '-' -> return Minus
      _   -> fail "Wrong operator"

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

parseStats :: Parser Program
parseStats = do
    stats <- parseStat `sepBy` (string ";" >> spaces)
    return $ case stats of
               [x] -> x
               x   -> Seq x
  where parseStat = parseAssign <|> parseLoop
