module Language.LoopGotoWhile.Goto.Strict 
    ( eval
    , parse
    , prettyPrint
    ) where

import Control.Monad
import Control.Monad.ST
import Data.STRef
import Data.Maybe (fromJust)
import Text.ParserCombinators.Parsec hiding (parse)

import Language.LoopGotoWhile.Shared.Util (mkStdParser)
import Language.LoopGotoWhile.Goto.StrictAS
import Language.LoopGotoWhile.Goto.Transform (toExtended, toWhile)
import qualified Language.LoopGotoWhile.While.Strict as WhileS
import qualified Language.LoopGotoWhile.While.Transform as WhileT


-- * Main Functions
--   ==============

-- | Given a strict Goto AST and a list of arguments evaluate the program
-- and return the value of 'x0'.
eval :: Program -> [Integer] -> Integer
eval ast = WhileS.eval (WhileT.toStrict . toWhile . toExtended $ ast)

-- | Given a string representation of a strict Goto program parse it and
-- return either an error string or the AST.
parse :: String -> Either String Program
parse = mkStdParser parseStats (1, False) spaces


-- * Parsing
--   =======

-- In order to check if labels have successive indices starting at 1 and to
-- check if the last statement is either 'HALT' or 'GOTO Mx' state must be
-- carried along. In this case the integer represents the currenct and correct
-- index. If the index of a label is not equal to this integer an error is
-- thrown. Likewise, the bool value is set to true if the current statement is
-- either 'HALT' or 'GOTO Mx' otherwise it is set to false. If the bool value
-- is not true when the last statement has been reached an error is thrown.
type GotoParser a = GenParser Char (Integer, Bool) a

parseConst :: GotoParser Const
parseConst = liftM read (many1 digit <?> "constant")

parseVar :: GotoParser VIndex
parseVar = liftM read (char 'x' >> many1 (digit <?> "") <?> "variable")

parseLabel' :: GotoParser LIndex
parseLabel' = do
    _ <- char 'M'
    x <- many1 (digit <?> "") <?> "label"
    spaces
    return $ read x

parseLabel :: GotoParser LIndex
parseLabel = do
    x <- parseLabel'
    (l,_) <- getState
    when (x /= l) $ fail "label numbers not successive"
    updateState (\(i,j) -> (i + 1, j)) >> spaces >> char ':' >> spaces >> return x

parseOp :: GotoParser Op
parseOp = do
    op <- oneOf "+-"
    case op of
      '+' -> return Plus
      '-' -> return Minus
      _   -> fail "Wrong operator"

parseAssign :: GotoParser Stat
parseAssign = do
    l <- parseLabel
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
    updateState $ \(l,_) -> (l,False)
    return $ Assign l x y o c

parseHalt :: GotoParser Stat
parseHalt = do
    l <- parseLabel
    spaces
    _ <- string "HALT"
    spaces
    updateState $ \(l,_) -> (l,True)
    return $ Halt l

parseGoto :: GotoParser Stat
parseGoto = do
    l1 <- parseLabel
    spaces
    _ <- string "GOTO"
    spaces
    l2 <- parseLabel'
    spaces
    updateState $ \(l,_) -> (l,True)
    return $ Goto l1 l2

parseIfGoto :: GotoParser Stat
parseIfGoto = do
    l1 <- parseLabel
    spaces
    _ <- string "IF"
    spaces
    x <- parseVar
    spaces
    _ <- string "="
    spaces
    c <- parseConst
    spaces
    _ <- string "THEN"
    spaces
    _ <- string "GOTO"
    spaces
    l2 <- parseLabel'
    spaces
    _ <- string "END"
    spaces
    updateState $ \(l,_) -> (l,False)
    return $ IfGoto l1 x c l2

parseStats :: GotoParser Program
parseStats = do
    stats <- parseStat `sepBy` (string ";" >> spaces)
    (_,b) <- getState
    case b of
      False -> fail "last statement is neither HALT nor GOTO"
      True  -> return $ case stats of
                 [x] -> x
                 x   -> Seq x
  where parseStat = try parseAssign 
                <|> try parseGoto 
                <|> try parseIfGoto 
                <|> parseHalt
