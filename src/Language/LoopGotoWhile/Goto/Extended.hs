{-# LANGUAGE RankNTypes #-}
-- | Parsing and evaluation of extended Goto.
module Language.LoopGotoWhile.Goto.Extended
    ( run
    , eval
    , parse
    , prettyPrint
    ) where

import Control.Monad

import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec hiding (State, labels, parse)
import Text.ParserCombinators.Parsec.Language (javaStyle)

import qualified Language.LoopGotoWhile.Goto.Strict as Strict
import Language.LoopGotoWhile.Goto.ExtendedAS
import Language.LoopGotoWhile.Goto.Transform (toStrict, toExtended)
import Language.LoopGotoWhile.Shared.Util (mkStdParser, mkStdRunner)
import Language.LoopGotoWhile.Shared.Extended 


-- * Main Functions
--   ==============

-- | Given a string representation of an extended Goto program and a list of
-- arguments parse & evaluate the program and return either an error string or
-- the value of 'x0'.
run :: String -> [Integer] -> Either String Integer
run = mkStdRunner parse eval

-- | Given an extended Goto AST and a list of arguments evaluate the program
-- and return the value of 'x0'.
eval :: Program -> [Integer] -> Integer
eval ast = Strict.eval (toStrict ast)

-- | Given a string representation of an extended Goto program parse it and
-- return either an error string or the AST. If given a valid strict Goto
-- program parse it as well.
parse :: String -> Either String Program
parse s = case Strict.parse s of 
            Right strictP -> Right $ toExtended strictP
            Left  _       -> mkStdParser parseProgram [] whiteSpace s


-- * Parsing
--   =======

-- | Carry along a list of labels to check if there aren't any duplicates.
type GotoParser a = GenParser Char [LIdent] a

parseProgram :: GotoParser Stat
parseProgram = do
    stats <- semiSep1 parseStat
    return $ if length stats < 2 then head stats else Seq stats

parseStat :: GotoParser Stat
parseStat = choice
          [ try $ parseMaybeWithLabel parseIfElseStat
          , parseMaybeWithLabel parseIfStat
          , try $ parseMaybeWithLabel parseAssignStat
          , parseMaybeWithLabel parseGoto
          , parseMaybeWithLabel parseHalt
          ]

parseHalt :: GotoParser Stat
parseHalt = do
    reserved "HALT"
    return Halt

parseGoto :: GotoParser Stat
parseGoto = do
    reserved "GOTO"
    l <- identifier
    return $ Goto l

parseAssignStat :: GotoParser Stat
parseAssignStat = do
    ident <- identifier
    reservedOp ":="
    s <- parseAritExp
    return $ Assign ident s

parseIfStat :: GotoParser Stat
parseIfStat = do
    reserved "IF"
    cond <- parseBoolExp
    reserved "THEN"
    thenpart <- parseProgram
    reserved "END"
    return $ If cond thenpart Nothing

parseIfElseStat :: GotoParser Stat
parseIfElseStat = do
    reserved "IF"
    cond <- parseBoolExp
    reserved "THEN"
    thenpart <- parseProgram
    reserved "ELSE"
    elsepart <- parseProgram
    reserved "END"
    return $ If cond thenpart (Just elsepart)
             
-- | Parse a statement that may or may not be preceded by a label.
parseMaybeWithLabel :: GotoParser Stat -> GotoParser Stat
parseMaybeWithLabel p = try (parseWithLabel p) <|> p

-- | Parse a statement that is preceded by a label.
parseWithLabel :: GotoParser Stat -> GotoParser Stat
parseWithLabel p = do
    l <- identifier
    labels <- getState
    when (l `elem` labels) $ 
       fail $ "Use of duplicate labels: " ++ l ++ " is already used!"
    updateState ((:) l)
    _ <- symbol ":"
    s <- p
    return $ Label l s


-- * Lexing
--   ======

lexer      :: forall st. P.TokenParser st
lexer      = P.makeTokenParser gotoDef

gotoDef    :: forall st. P.LanguageDef st
gotoDef    = javaStyle
            { P.reservedNames   = [ "GOTO", "HALT", "END", "IF", "THEN", "ELSE" ]
            , P.reservedOpNames = [ ":="
                                  , "+", "-", "*", "/", "^", "%"
                                  , "=", "!=", "<", "<=", ">", ">="
                                  , "!", "&&", "||"
                                  ]
            , P.opLetter        = oneOf (concat (P.reservedOpNames gotoDef))
            , P.caseSensitive   = True
            }

semiSep1   :: forall st a. CharParser st a -> CharParser st [a]
semiSep1   = P.semiSep1 lexer    
whiteSpace :: forall st. CharParser st ()
whiteSpace = P.whiteSpace lexer    
identifier :: forall st. CharParser st String
identifier = P.identifier lexer    
reserved   :: forall st. String -> CharParser st ()
reserved   = P.reserved lexer    
reservedOp :: forall st. String -> CharParser st ()
reservedOp = P.reservedOp lexer
symbol     :: forall st. String -> CharParser st String
symbol     = P.symbol lexer   
