{-# OPTIONS_GHC -Wall -Wno-name-shadowing -dynamic #-}
import System.Environment
import Lexer

data Expr
    = ENumVal Integer
    | EIdentVal String
    | EAdd Expr Expr
    | ESub Expr Expr
    | EMul Expr Expr
    | EDiv Expr Expr
    | EError
    deriving Show

getExpr :: [Token] -> ([Token], Expr)
getExpr tokens = 
    getExprPrime toks expr
    where (toks, expr) = getTerm tokens 


getExprPrime :: [Token] -> Expr -> ([Token], Expr)
getExprPrime [] i = ([], i)
getExprPrime (tok:rest) i
    | tok == Lexer.Add = getExprPrime toks (EAdd i expr)
    | tok == Lexer.Sub = getExprPrime toks (ESub i expr)
    | otherwise = (tok : rest, i)
    where (toks, expr) = getTerm rest

getTerm :: [Token] -> ([Token], Expr)
getTerm tokens =
    getTermPrime toks expr
    where (toks, expr)= getFactor tokens

getTermPrime :: [Token] -> Expr -> ([Token], Expr)
getTermPrime [] i = ([], i)
getTermPrime (tok:rest) i
    | tok == Lexer.Mul =  getTermPrime toks (EMul i expr)
    | tok == Lexer.Div = getTermPrime toks (EDiv i expr)
    | otherwise = (tok : rest, i)
    where (toks, expr) = getFactor rest

getFactor :: [Token] -> ([Token], Expr)
getFactor (Number n : rest) = (rest, ENumVal n)
getFactor (Ident i : rest) = (rest, EIdentVal i)
getFactor (LeftBraces : rest) =
    case getExpr rest of
        (RightBraces : rest, expr) -> (rest, expr)
        ([Eof], _) -> ([Eof], EError)
        n -> n
getFactor other = (other, EError)

mevalExpr :: Expr -> Expr
mevalExpr (ENumVal n) = ENumVal n
mevalExpr (EIdentVal i) = EIdentVal i
mevalExpr (EAdd l r) =
    let el = mevalExpr l
        er = mevalExpr r
    in
    case (el, er) of
        (ENumVal a, ENumVal b) -> ENumVal (a + b)
        _ -> EAdd el er
mevalExpr (ESub l r) =
    let el = mevalExpr l
        er = mevalExpr r
    in
    case (el, er) of
        (ENumVal a, ENumVal b) -> ENumVal (a - b)
        _ -> ESub el er
mevalExpr (EMul l r) =
    let el = mevalExpr l
        er = mevalExpr r
    in
    case (el, er) of
        (ENumVal a, ENumVal b) -> ENumVal (a * b)
        _ -> EMul el er
mevalExpr (EDiv l r) =
    let el = mevalExpr l
        er = mevalExpr r
    in
    case (el, er) of
        (ENumVal a, ENumVal b) -> ENumVal (div a b)
        _ -> EDiv el er
mevalExpr EError = EError

showExpr :: Expr -> String
showExpr (ENumVal n) = show n
showExpr (EIdentVal i ) = i
showExpr (EAdd l r) = "(" ++ showExpr l ++ " + " ++ showExpr r ++ ")"
showExpr (ESub l r) = "(" ++ showExpr l ++ " - " ++ showExpr r ++ ")"
showExpr (EMul l r) = showExpr l ++ " * " ++ showExpr r
showExpr (EDiv l r) = showExpr l ++ " / " ++ showExpr r
showExpr EError = "<ERROR>"

main :: IO ()
main = do
    args <- getArgs
    str <- readFile $ head args
    putStrLn $ 
        let (_, expr) = getExpr (getTokens str) in
        (showExpr expr) ++ " = " ++ (showExpr $ mevalExpr expr)
