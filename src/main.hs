{-# OPTIONS_GHC -Wall -Wno-name-shadowing -dynamic #-}
import System.Environment
import Lexer
import Parser
import Ast
import Vm

getInst :: String -> Inst
getInst = generate . parse . getTokens

run :: String -> [Integer] 
run = startvm . getInst

main :: IO()
main = do
    args <- getArgs
    str <- readFile $ head args
    putStrLn . show $ run str
