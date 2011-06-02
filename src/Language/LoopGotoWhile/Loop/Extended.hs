-- | Parsing and evaluation of extended Loop.
module Language.LoopGotoWhile.Loop.Extended
    ( eval
    , parse
    , prettyPrint
    ) where

import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec hiding (State, parse)
import Text.ParserCombinators.Parsec.Language (javaStyle)

import qualified Language.LoopGotoWhile.Loop.Strict as Strict
import Language.LoopGotoWhile.Loop.ExtendedAS
import Language.LoopGotoWhile.Loop.Transform (toStrict)
import Language.LoopGotoWhile.Shared.Util (mkStdParser)
import Language.LoopGotoWhile.Shared.Extended


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


-- * Lexing
--   ======

-- TODO: Type annotations

lexer      = P.makeTokenParser loopDef

loopDef    = javaStyle
           { P.reservedNames   = [ "LOOP", "DO", "END", "IF", "THEN", "ELSE" ]
           , P.reservedOpNames = [ ":=" 
                                 , "+", "-", "*", "/", "^", "%"
                                 , "=", "!=", "<", "<=", ">", ">=" 
                                 , "!", "&&", "||"
                                 ]
           , P.opLetter        = oneOf (concat (P.reservedOpNames loopDef))
           , P.caseSensitive   = True
           }

semiSep1   = P.semiSep1 lexer    
whiteSpace = P.whiteSpace lexer    
symbol     = P.symbol lexer    
identifier = P.identifier lexer    
reserved   = P.reserved lexer    
reservedOp = P.reservedOp lexer
