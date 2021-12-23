{-# OPTIONS_GHC -Wall -Wno-name-shadowing -dynamic #-}
import System.Environment
import Lexer
import Parser
import Ast
import Vm

getInst :: String -> Inst
getInst = generate . parse . getTokens

compileAndSave :: String -> String -> IO()
compileAndSave path = (save path) . getInst

main :: IO()
main = do
    args <- getArgs
    str <- readFile $ head args
    --putStr . show $ (parse . getTokens) str
    compileAndSave (args !! 1) str
