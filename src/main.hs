{-# OPTIONS_GHC -Wall -Wno-name-shadowing -dynamic #-}
import System.Environment
import Lexer
import Parser
import Ast

getInst :: String -> Maybe Inst
getInst str = 
    let parsed = (parse . getTokens) str in 
    case parsed of
        EError -> Nothing
        p -> Just $ generate p [[]]

compileAndSave :: String -> String -> IO()
compileAndSave path str =
    case getInst str of
        Nothing -> error "Error"
        Just inst -> do 
            save path inst
            putStrLn "Done"

main :: IO()
main = do
    args <- getArgs
    str <- readFile $ head args
    compileAndSave (args !! 1) str
