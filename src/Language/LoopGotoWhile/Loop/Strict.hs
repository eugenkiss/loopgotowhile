module Language.LoopGotoWhile.Loop.Strict where

import Control.Monad
import Control.Monad.ST
import Data.STRef
import Data.Maybe (fromJust)
import Data.List (intercalate)
import Text.ParserCombinators.Parsec

import Language.LoopGotoWhile.Util (mkStdParser)


parser :: String -> Either String Program
parser = mkStdParser parseStatements spaces

evaluator :: Program -> [Integer] -> Integer
evaluator ast args = runST $ do
    envRef <- nullEnv
    forM_ [1..length args] $ \i ->
        setVar envRef (toInteger i) (args !! (i-1))
    eval envRef ast
    getVar envRef 0

prettyPrint :: Program -> String
prettyPrint = prettyPrint' 0 ""
  where prettyPrint' indentSize s (Assignment i j op c) = s ++ 
            indent indentSize ++ "x" ++ show i ++ " := " ++ 
            "x" ++ show j ++ " " ++ show op ++ " " ++ show c
        prettyPrint' indentSize s (Loop i stat) = s ++ 
            indent indentSize ++ "LOOP x" ++ show i ++ " DO\n" ++ 
            prettyPrint' (indentSize + tabSize) "" stat ++ "\n" ++
            indent indentSize ++ "END"
        prettyPrint' indentSize s (Sequence stats) =
            intercalate ";\n" . map (prettyPrint' indentSize "") $ stats 
        indent size = replicate size ' '
        tabSize     = 2
             

type Program = Statement

type VarIndex = Integer

type Constant = Integer

data Operator = Plus | Minus deriving Eq

instance Show Operator where
    show Plus  = "+"
    show Minus = "-"

data Statement
    = Assignment VarIndex VarIndex Operator Constant
    | Loop VarIndex Statement
    | Sequence [Statement]
    deriving Eq

instance Show Statement where
    show = prettyPrint


parseConst :: Parser Constant
parseConst = liftM read (many1 digit <?> "constant")

parseVar :: Parser VarIndex
parseVar = liftM read (char 'x' >> many1 (digit <?> "") <?> "variable")

parseOp :: Parser Operator
parseOp = do
    op <- oneOf "+-"
    case op of
      '+' -> return Plus
      '-' -> return Minus
      _   -> fail "Wrong operator"

parseAssignment :: Parser Statement
parseAssignment = do
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

parseLoop :: Parser Statement
parseLoop = do
    _ <- string "LOOP"
    spaces
    x <- parseVar
    spaces
    _ <- string "DO"
    spaces
    body <- parseStatements
    spaces
    _ <- string "END"
    return $ Loop x body

parseStatements :: Parser Program
parseStatements = do
    stats <- parseStatement `sepBy` (string ";" >> spaces)
    return $ case stats of
               [x] -> x
               x   -> Sequence x
  where parseStatement = parseAssignment <|> parseLoop


eval :: Env s -> Statement -> ST s ()
eval env (Assignment i j Plus c) = do
    xj <- getVar env j
    setVar env i (xj + c)
eval env (Assignment i j Minus c) = do
    xj <- getVar env j
    setVar env i (max (xj - c) 0)
eval env (Loop n statement) = do
    xn <- getVar env n
    loop xn
  where loop 0  = return ()
        loop xn = eval env statement >> loop (xn-1)
eval env (Sequence statements) = mapM_ (eval env) statements


type Env s = STRef s [(VarIndex, STRef s Integer)]

-- TODO: Is there a better way to achieve this?
nullEnv :: ST s (Env s)
nullEnv = newSTRef =<< return . zip [0..] =<< lazyRefs
  where lazyRefs = do
          x  <- newSTRef 0
          xs <- unsafeInterleaveST lazyRefs
          return (x:xs)

getVar :: Env s -> VarIndex -> ST s Integer
getVar envRef i = readSTRef envRef >>= readSTRef . fromJust . lookup i

setVar :: Env s -> VarIndex -> Integer -> ST s ()
setVar envRef i v = readSTRef envRef >>= flip writeSTRef v . fromJust . lookup i 
