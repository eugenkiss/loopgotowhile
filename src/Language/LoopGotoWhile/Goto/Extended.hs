-- | Parsing and evaluation of extended Goto.
module Language.LoopGotoWhile.Goto.Extended
    ( eval
    , parse
    , prettyPrint
    ) where

import Control.Monad

import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec hiding (State, parse)
import Text.ParserCombinators.Parsec.Language (javaStyle)

import qualified Language.LoopGotoWhile.Goto.Strict as Strict
import Language.LoopGotoWhile.Goto.ExtendedAS
import Language.LoopGotoWhile.Goto.Transform (toStrict)
import Language.LoopGotoWhile.Shared.Util (mkStdParser)
import Language.LoopGotoWhile.Shared.Extended 


-- * Main Functions
--   ==============

-- | Given an extended While AST and a list of arguments evaluate the program
-- and return the value of 'x0'.
eval :: Program -> [Integer] -> Integer
eval ast args = Strict.eval (toStrict ast) args

-- | Given a string representation of an extended While program parse it and
-- return either an error string or the AST.
parse :: String -> Either String Program
parse = mkStdParser parseProgram [] whiteSpace


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
    return $ Halt

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
    when (l `elem` labels) $ do
       fail $ "Use of duplicate labels: " ++ l ++ " is already used!"
    updateState ((:) l)
    symbol ":"
    s <- p
    return $ Label l s


-- * Lexing
--   ======

-- TODO: Type annotations

lexer      = P.makeTokenParser gotoDef

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

semiSep1   = P.semiSep1 lexer   
whiteSpace = P.whiteSpace lexer   
symbol     = P.symbol lexer   
identifier = P.identifier lexer   
reserved   = P.reserved lexer   
reservedOp = P.reservedOp lexer
