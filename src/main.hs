{-# OPTIONS_GHC -Wall -Wno-name-shadowing -dynamic #-}
import System.Environment
import Lexer

main :: IO()
main = do
    putStrLn "Hi"
    args <- getArgs
    str <- readFile $ head args
    putStrLn . show $ getTokens str
