{-# LANGUAGE RankNTypes #-}
-- | Shared parsing functionality to reduce code duplication.
module Language.LoopGotoWhile.Shared.Extended
    ( parseBoolExp
    , parseAritExp
    ) where

import Control.Monad

import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec hiding (Parser)
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language (javaStyle)

import Language.LoopGotoWhile.Shared.ExtendedAS


-- * Parsing
--   =======

-- The reason for using RankNTypes is that Goto needs to keep other state for
-- 'st' than either While or Loop and using RankNTypes is the only way to reuse
-- the parsers for boolean & arithmetic expressions.
type Parser a = forall st. GenParser Char st a

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


-- * Lexing
--   ======

-- TODO: Type annotations

lexer      = P.makeTokenParser sharedDef

sharedDef  = javaStyle
           { P.reservedOpNames = [ "+", "-", "*", "/", "^", "%"
                                 , "=", "!=", "<", "<=", ">", ">=" 
                                 , "!", "&&", "||"
                                 ]
           , P.opLetter        = oneOf (concat (P.reservedOpNames sharedDef))
           , P.caseSensitive   = True
           }

parens     = P.parens lexer    
whiteSpace = P.whiteSpace lexer    
symbol     = P.symbol lexer    
identifier = P.identifier lexer    
reservedOp = P.reservedOp lexer
integer    = P.integer lexer    
