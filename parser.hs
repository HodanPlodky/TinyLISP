{-# OPTIONS_GHC -Wall -Wno-name-shadowing -dynamic #-}
import System.Environment
import Lexer

data Expr
    = ENumVal Integer
    | EIdentVal String
    | EOp (Integer -> Integer -> Integer) String Expr Expr
    | EError

getExpr :: [Token] -> ([Token], Expr)
getExpr tokens = 
    getExprPrime toks expr
    where (toks, expr) = getTerm tokens 

getExprPrime :: [Token] -> Expr -> ([Token], Expr)
getExprPrime [] i = ([], i)
getExprPrime (tok:rest) i
    | tok == Lexer.Add = getExprPrime toks (EOp (+) "+" i expr)
    | tok == Lexer.Sub = getExprPrime toks (EOp (-) "-" i expr)
    | otherwise = (tok : rest, i)
    where (toks, expr) = getTerm rest

getTerm :: [Token] -> ([Token], Expr)
getTerm tokens =
    getTermPrime toks expr
    where (toks, expr)= getFactor tokens

getTermPrime :: [Token] -> Expr -> ([Token], Expr)
getTermPrime [] i = ([], i)
getTermPrime (tok:rest) i
    | tok == Lexer.Mul =  getTermPrime toks (EOp (*) "*" i expr)
    | tok == Lexer.Div = getTermPrime toks (EOp div "/" i expr)
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
mevalExpr EError = EError
mevalExpr (EOp op s l r) =
    let el = mevalExpr l
        er = mevalExpr r
    in
    case (el, er) of
        (ENumVal a, ENumVal b) -> ENumVal (op a b)
        _ -> EOp op s el er

showExpr :: Expr -> String
showExpr (ENumVal n) = show n
showExpr (EIdentVal i ) = i
showExpr (EOp _ s l r) = "(" ++ showExpr l ++ " " ++ s ++ " " ++ showExpr r ++ ")"
showExpr EError = "<ERROR>"

main :: IO ()
main = do
    args <- getArgs
    str <- readFile $ head args
    putStrLn $ 
        let (_, expr) = getExpr (getTokens str) in
        (showExpr expr) ++ " = " ++ (showExpr $ mevalExpr expr)
