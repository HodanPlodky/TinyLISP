{-# OPTIONS_GHC -Wall -Wno-name-shadowing -dynamic #-}
import System.Environment
import Lexer
import Parser
import Ast

run :: String -> Inst
run = generate . parse . getTokens

main :: IO()
main = do
    putStrLn "Hi"
    args <- getArgs
    str <- readFile $ head args
    putStrLn . show $ run str
