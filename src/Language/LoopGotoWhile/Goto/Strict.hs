-- | Parsing and evaluation of strict Goto.
module Language.LoopGotoWhile.Goto.Strict 
    ( run
    , eval
    , parse
    , prettyPrint
    ) where

import Control.Monad
import Control.Monad.ST
import Data.STRef
import qualified Data.Map as M
import Data.Array.ST hiding (index)
import Data.List (genericLength)

import Text.ParserCombinators.Parsec hiding (parse, label)

import Language.LoopGotoWhile.Shared.Util (mkStdParser, mkStdRunner)
import Language.LoopGotoWhile.Goto.StrictAS


-- * Main Functions
--   ==============
 
-- | Given a string representation of a strict Goto program and a list of
-- arguments parse & evaluate the program and return either an error string or
-- the value of 'x0'.
run :: String -> [Integer] -> Either String Integer
run = mkStdRunner parse eval

-- | Given a strict Goto AST and a list of arguments evaluate the program
-- and return the value of 'x0'.
eval :: Program -> [Integer] -> Integer
eval ast args = runST $ do
    envRef <- nullEnv
    let (Seq stats) = case ast of
                        Seq ss -> Seq ss
                        other  -> Seq [other]
    statsArr <- newListArray (1, genericLength stats) stats :: ST s (STArray s Integer Stat)
    forM_ [1..length args] $ \i ->
        setVar envRef (toInteger i) (args !! (i-1))
    eval' envRef statsArr 1
    getVar envRef 0

-- | Given a string representation of a strict Goto program parse it and
-- return either an error string or the AST.
parse :: String -> Either String Program
parse = mkStdParser parseStats (1, False) spaces


-- * Evaluation
--   ==========

type Env s      = STRef s (M.Map VIndex (STRef s Integer))
type StatsArr s = STArray s Integer Stat

eval' :: Env s -> StatsArr s -> Integer -> ST s ()
eval' env arr index = do
    stat <- readArray arr index
    case stat of
      Assign l i j Plus c -> do
          xj <- getVar env j
          setVar env i $! (xj + c)
          eval' env arr $ succ l
      Assign l i j Minus c -> do
          xj <- getVar env j
          setVar env i $! (max (xj - c) 0)
          eval' env arr $ succ l
      IfGoto l1 i c l2 -> do
          xi <- getVar env i
          if xi == c 
             then eval' env arr l2
             else eval' env arr $ succ l1
      Goto _ l -> eval' env arr l
      Halt _   -> return ()
      Seq  _   -> error "Impossible! Seq must not appear here!"

nullEnv :: ST s (Env s)
nullEnv = newSTRef M.empty

getVar :: Env s -> VIndex -> ST s Integer
getVar envRef i = do
    env <- readSTRef envRef
    case M.lookup i env of
      Just varRef -> readSTRef varRef
      Nothing     -> do x <- newSTRef 0
                        writeSTRef envRef (M.insert i x env) 
                        return $ fromInteger 0

setVar :: Env s -> VIndex -> Integer -> ST s ()
setVar envRef i v = do
    env <- readSTRef envRef
    case M.lookup i env of
      Just varRef -> writeSTRef varRef v
      Nothing     -> do x <- newSTRef v
                        writeSTRef envRef (M.insert i x env) 


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
    lab <- parseLabel
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
    return $ Assign lab x y o c

parseHalt :: GotoParser Stat
parseHalt = do
    lab <- parseLabel
    spaces
    _ <- string "HALT"
    spaces
    updateState $ \(l,_) -> (l,True)
    return $ Halt lab

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
    updateState $ \(l,_) -> (l,False)
    return $ IfGoto l1 x c l2

parseStats :: GotoParser Program
parseStats = do
    stats <- parseStat `sepBy` (string ";" >> spaces)
    (_,b) <- getState
    if b then return $ case stats of
                         [x] -> x
                         x   -> Seq x
       else fail "last statement is neither HALT nor GOTO"
  where parseStat = try parseAssign 
                <|> try parseGoto 
                <|> try parseIfGoto 
                <|> parseHalt
