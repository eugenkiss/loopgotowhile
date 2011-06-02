import Language.LoopGotoWhile.Util
{-import Language.LoopGotoWhile.Base.Base-}
{-import Language.LoopGotoWhile.Loop.Loop-}
import qualified Language.LoopGotoWhile.Loop.Strict as Strict
import Language.LoopGotoWhile.Loop.Extended
import Language.LoopGotoWhile.Loop.Transform
{-import qualified Language.LoopGotoWhile.Goto.Strict as Strict-}
{-import Language.LoopGotoWhile.Goto.Extended-}
{-import Language.LoopGotoWhile.Goto.Transform-}

main :: IO ()
{-main = putStrLn $ show $ evalProgram' parser evaluator "x0 := x1 + 1" [1]-}
{-main = putStrLn $ show $ evalProgram' parser evaluator "LOOP x1 DO x0 := x0 + 0; x1 := x2 + 1 END" [10]-}
{-main = putStrLn $ show $ evalProgram' parser evaluator " sdf" [10]-}
{-main = case parser program of -}
         {-Left  err -> putStrLn err-}
         {-Right ast -> do putStrLn $ prettyPrint (makeStrictVars 30 ast)-}
                         {-putStrLn $ show $ getVarNames ast-}
                         {-putStrLn $ show $ (getVarNames . makeStrictVars 30) ast-}
                         {-[>putStrLn $ evalProgram' parser evaluator program []<]-}
  {-where program = "v := y + z; j := v + y; LOOP j DO x33 := x0 + 0 END"-}
main = do 
    print $ toGoto $ parseE p
    {-print $ parseS s-}
  where p = "LOOP x3 / 2 DO x0 := x0 END"

parseE code = case parse code of
    Left  err -> error err
    Right ast -> ast

parseS code = case Strict.parse code of
    Left  err -> error err
    Right ast -> ast
