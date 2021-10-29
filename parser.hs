{-# OPTIONS_GHC -Wall -Wno-name-shadowing -dynamic #-}
import Data.List
import Data.Char
import System.IO
import System.Environment
import Lexer

data Expr
    = NumVal Integer
    | IdentVal String
    | Add Expr Expr
    | Sub Expr Expr
    | Mul Expr Expr
    | Div Expr Expr
    | Empty
    deriving Show

getExpr :: [Token] -> [Token]
getExpr tokens = getExprPrime $ getTerm tokens 

getExprPrime :: [Token] -> [Token]
getExprPrime [] = []
getExprPrime (tok:rest)
    | tok == Lexer.Add = getExprPrime $ getTerm rest
    | tok == Lexer.Sub = getExprPrime $ getTerm rest
    | otherwise = tok : rest

getTerm :: [Token] -> [Token]
getTerm tokens = getTermPrime $ getFactor tokens

getTermPrime :: [Token] -> [Token]
getTermPrime [] = []
getTermPrime (tok:rest)
    | tok == Lexer.Mul = getTermPrime $ getFactor rest
    | tok == Lexer.Div = getTermPrime $ getFactor rest
    | otherwise = tok : rest

getFactor :: [Token] -> [Token]
getFactor (Number n : rest) = rest
getFactor (Ident i : rest) = rest
getFactor (LeftBraces : rest) =
    case getExpr rest of
        (RightBraces : rest) -> rest
        n -> n

main :: IO ()
main = do
    args <- getArgs
    putStrLn $ "Lexing " ++ head args
    str <- readFile $ head args
    print $ getTokens str
    print $ getExpr (getTokens str)
