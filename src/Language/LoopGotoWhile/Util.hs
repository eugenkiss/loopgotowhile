module Language.LoopGotoWhile.Util
    ( evalProgram
    , evalProgram'
    , makeStdParser
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
      Left err  -> -1
      Right res -> res

makeStdParser :: GenParser Char () a -> (String -> Either String a)
makeStdParser parser code = 
    case parse parser "" code of
      Left err  -> Left $ show err
      Right val -> Right val 

--TODO: If possible create 'makeStdEvaluator'
