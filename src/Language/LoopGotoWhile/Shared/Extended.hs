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

boolOperators :: forall st. [[Operator Char st BExp]]
boolOperators =
    [ [ op "&&" AssocRight ]
    , [ op "||" AssocRight ] 
    ]
  where op name = Infix $ do reservedOp name
                             return $ \x y -> BOp name x y 

parseSimpleBool :: forall st. GenParser Char st BExp
parseSimpleBool = choice 
                [ try parseRelExp
                , parens parseBoolExp
                , parseNegBool
                ]

parseNegBool :: Parser BExp
parseNegBool = do
    reservedOp "!"
    whiteSpace
    _ <- string "("
    whiteSpace
    bexp <- parseBoolExp
    whiteSpace
    _ <- string ")"
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

aritOperators :: forall st. [[Operator Char st AExp]]
aritOperators =
    [ [ op "^" AssocRight ]
    , [ op "*" AssocLeft, op "/" AssocLeft ]
    , [ op "+" AssocLeft, op "-" AssocLeft ]
    , [ op "%" AssocRight ] 
    ]
  where op name = Infix $ do reservedOp name
                             return $ \x y -> AOp name x y 

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

lexer      :: forall st. P.TokenParser st
lexer      = P.makeTokenParser sharedDef

sharedDef  :: forall st. P.LanguageDef st
sharedDef  = javaStyle
           { P.reservedOpNames = [ "+", "-", "*", "/", "^", "%"
                                 , "=", "!=", "<", "<=", ">", ">=" 
                                 , "!", "&&", "||"
                                 ]
           , P.opLetter        = oneOf (concat (P.reservedOpNames sharedDef))
           , P.caseSensitive   = True
           }

parens     :: forall st a. CharParser st a -> CharParser st a
parens     = P.parens lexer    
whiteSpace :: forall st. CharParser st ()
whiteSpace = P.whiteSpace lexer    
symbol     :: forall st. String -> CharParser st String
symbol     = P.symbol lexer    
identifier :: forall st. CharParser st String
identifier = P.identifier lexer    
reservedOp :: forall st. String -> CharParser st ()
reservedOp = P.reservedOp lexer
integer    :: forall st. CharParser st Integer
integer    = P.integer lexer    
