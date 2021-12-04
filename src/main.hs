{-# OPTIONS_GHC -Wall -Wno-name-shadowing -dynamic #-}
import System.Environment
import Lexer
import Parser
import Ast
import Vm

getInst :: String -> Inst
getInst = generate . parse . getTokens

run :: String -> [StackData] 
run = startvm . getInst

compileAndSave :: String -> String -> IO()
compileAndSave path = (save path) . getInst

mapOutput :: StackData -> String
mapOutput (DataNum n) = show n
mapOutput (Cons (Cell (DataNum x) (DataNum y))) =
    "(" ++ show x ++ "." ++ show y ++ ")"
mapOutput (Cons (Cell (DataNum x) (Cons y))) =
    "(" ++ show x ++ " " ++ mapInner  (Cons y) ++ ")"
mapOutput (Cons (Cell x y)) =
    "(" ++ mapOutput x ++ " " ++ mapOutput y ++ ")"
mapOutput (Cons Empty) = "()"

mapInner :: StackData -> String
mapInner (DataNum n) = show n
mapInner (Cons (Cell (DataNum x) (DataNum y))) =
    "(" ++ show x ++ "." ++ show y ++ ")"
mapInner (Cons (Cell (DataNum x) (Cons Empty))) =
    show x
mapInner (Cons (Cell (DataNum x) (Cons y))) =
    show x ++ " " ++ mapInner (Cons y)
mapInner (Cons (Cell x y)) =
    "(" ++ mapOutput x ++ " " ++ mapOutput y ++ ")"
mapInner (Cons Empty) = ""


main :: IO()
main = do
    args <- getArgs
    str <- readFile $ head args
    --putStr $ foldl (\x y -> x ++ y ++ "\n") "" (map mapOutput (run str))
    compileAndSave (args !! 1) str
