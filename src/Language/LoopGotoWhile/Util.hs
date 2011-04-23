module Language.LoopGotoWhile.Util
    ( evalProgram
    , evalProgram'
    ) where

type Parser a = String -> Either String a

type Evaluator a = a -> [Integer] -> IO Integer

evalProgram :: Parser a -> Evaluator a -> String -> [Integer] -> Either String (IO Integer)
evalProgram parser evaluator code args = 
    case parser code of
      Left err  -> Left err
      Right ast -> Right $ evaluator ast args
      
evalProgram' :: Parser a -> Evaluator a -> String -> [Integer] -> IO Integer
evalProgram' parser evaluator code args = 
    case evalProgram parser evaluator code args of
      Left err  -> return (-1)
      Right ioa -> ioa
