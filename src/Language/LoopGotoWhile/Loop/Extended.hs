module Language.LoopGotoWhile.Loop.Extended
    ( eval
    , parse
    , prettyPrint
    ) where

import Control.Monad
import Control.Monad.State
import Data.Char (isDigit)
import Data.List (partition, nub, (\\), union)

import Text.ParserCombinators.Parsec hiding (State, parse)
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language (javaStyle)

import Language.LoopGotoWhile.Shared.Util (mkStdParser)
import Language.LoopGotoWhile.Loop.ExtendedAS
import qualified Language.LoopGotoWhile.Loop.Strict as Strict
import Language.LoopGotoWhile.Loop.Transform (toStrict)


-- * Main Functions
--   ==============

-- | Given an extended Loop AST and a list of arguments evaluate the program
-- and return the value of 'x0'.
eval :: Program -> [Integer] -> Integer
eval ast args = Strict.eval (toStrict ast) args 

-- | Given a string representation of an extended Loop program parse it and
-- return either an error string or the AST.
parse :: String -> Either String Program
parse = mkStdParser parseProgram () whiteSpace


-- * Parsing
--   =======

parseProgram :: Parser Stat
parseProgram = do
    stats <- semiSep1 parseStat
    return $ if length stats < 2 then head stats else Seq stats

parseStat :: Parser Stat
parseStat = choice 
          [ try parseIfElseStat
          , parseIfStat
          , parseLoopStat
          , try parseAssignStat
          ]

parseAssignStat :: Parser Stat
parseAssignStat = do
    ident <- identifier
    reservedOp ":="
    s <- parseAritExp
    return $ Assign ident s

parseIfStat :: Parser Stat
parseIfStat = do 
    reserved "IF"
    cond <- parseBoolExp
    reserved "THEN"
    thenpart <- parseProgram
    reserved "END"
    return $ If cond thenpart Nothing

parseIfElseStat :: Parser Stat
parseIfElseStat = do 
    reserved "IF"
    cond <- parseBoolExp
    reserved "THEN"
    thenpart <- parseProgram
    reserved "ELSE"
    elsepart <- parseProgram
    reserved "END"
    return $ If cond thenpart (Just elsepart)
             
parseLoopStat :: Parser Stat
parseLoopStat = do
   reserved "LOOP"
   a <- parseAritExp
   reserved "DO"
   body <- parseProgram
   reserved "END"
   return $ Loop a body

parseBoolExp :: Parser BExp
parseBoolExp = buildExpressionParser boolOperators parseSimpleBool

boolOperators =
    [ [ op "&&" AssocRight ]
    , [ op "||" AssocRight ] 
    ]
  where op name assoc = Infix (do reservedOp name
                                  return $ \x y -> BOp name x y 
                                  ) assoc

parseSimpleBool = choice 
                [ try parseRelExp
                , parens parseBoolExp
                , parseNegBool
                ]

parseNegBool :: Parser BExp
parseNegBool = do
    reservedOp "!"
    whiteSpace
    string "("
    whiteSpace
    bexp <- parseBoolExp
    whiteSpace
    string ")"
    whiteSpace
    return $ BNegOp bexp

parseRelExp :: Parser BExp
parseRelExp = do 
    arg1 <- parseAritExp
    op <- choice [ symbol "="
                 , try (symbol "!=")
                 , try (symbol "<=")
                 , symbol "<"
                 , try (symbol ">=")
                 , symbol ">"
                 ]
    arg2 <- parseAritExp
    return $ RelOp op arg1 arg2
    
parseAritExp :: Parser AExp
parseAritExp = buildExpressionParser aritOperators parseSimpleArit

aritOperators =
    [ [ op "^" AssocRight ]
    , [ op "*" AssocLeft, op "/" AssocLeft ]
    , [ op "+" AssocLeft, op "-" AssocLeft ]
    , [ op "%" AssocRight ] 
    ]
  where op name assoc = Infix (do reservedOp name
                                  return $ \x y -> AOp name x y 
                                  ) assoc

parseSimpleArit :: Parser AExp
parseSimpleArit = choice 
                [ parseConst
                , parens parseAritExp
                , parseVar
                ]

parseConst :: Parser AExp
parseConst = liftM Const (integer <?> "constant")

parseVar :: Parser AExp
parseVar = liftM Var (identifier <?> "identifier")


-- ** Lexing
--    ------

-- TODO: Type annotations

lexer     = P.makeTokenParser loopDef

loopDef   = javaStyle
          { P.reservedNames   = [ "LOOP", "DO", "END", "IF", "THEN", "ELSE" ]
          , P.reservedOpNames = [ ":=" 
                                , "+", "-", "*", "/", "^", "%"
                                , "=", "!=", "<", "<=", ">", ">=" 
                                , "!", "&&", "||"
                                ]
          , P.opLetter        = oneOf (concat (P.reservedOpNames loopDef))
          , P.caseSensitive   = True
          }

parens     = P.parens lexer    
semiSep1   = P.semiSep1 lexer    
whiteSpace = P.whiteSpace lexer    
symbol     = P.symbol lexer    
identifier = P.identifier lexer    
reserved   = P.reserved lexer    
reservedOp = P.reservedOp lexer
integer    = P.integer lexer    
