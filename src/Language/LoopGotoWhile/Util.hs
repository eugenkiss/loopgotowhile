module Language.LoopGotoWhile.Util
    ( evalProgram
    , evalProgram'
    , mkStdParser
    ) where

import Text.ParserCombinators.Parsec hiding (Parser)

type Parser a = String -> Either String a

type Evaluator a = a -> [Integer] -> Integer

evalProgram :: Parser a -> Evaluator a -> String -> [Integer] -> Either String Integer
evalProgram parser evaluator code args = 
    case parser code of
      Left err  -> Left err
      Right ast -> Right $ evaluator ast args
      
evalProgram' :: Parser a -> Evaluator a -> String -> [Integer] -> Integer
evalProgram' parser evaluator code args = 
    case evalProgram parser evaluator code args of
      Left _    -> -1
      Right res -> res

-- | Given a parser 'p' of program 'a' and an associated "whiteSpace"-parser
-- 'ws', i.e. a parser that skips whitespace, newlines, comments etc., make
-- a "standard" parser that receives a string of a program and returns either
-- an AST of type 'a' or an error message. The parser skips whitespace at the
-- beginning of the input and consumes input according to 'p' until the end of
-- the file/string.
mkStdParser :: GenParser Char () a -> GenParser Char () () -> String -> Either String a
mkStdParser p ws = \code -> 
    case parse p' "" code of
      Left err  -> Left $ show err
      Right val -> Right val 
  where p' = do ws 
                x <- p
                eof
                return x

-- TODO: If possible, create mkStdEvaluator instead of evalProgram
