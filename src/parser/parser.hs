{-# OPTIONS_GHC -Wall -Wno-name-shadowing -dynamic #-}
module Parser (parse) where

import Lexer
import Ast

parse :: [Token] -> Expr
parse toks =
    case parseImp [] toks of
        (e, []) -> e
        _ -> EError

parseImp :: [Expr] -> [Token] -> (Expr, [Token])
parseImp acc [] = (EList acc, [])
parseImp acc [TEof] = (EList acc, [])
parseImp acc t = 
    let (e, toks) = expression t in
    case e of
        EError -> (EError, toks)
        _ -> parseImp (acc ++ [e]) toks

expression :: [Token] -> (Expr, [Token])
-- build in binary operations
expression (TLBrac:TAdd:rest) = expressionBin rest OAdd
expression (TLBrac:TSub:rest) = expressionBin rest OSub
expression (TLBrac:TMul:rest) = expressionBin rest OMul
expression (TLBrac:TDiv:rest) = expressionBin rest ODiv
expression (TLBrac:TLt:rest) = expressionBin rest OLt
expression (TLBrac:TGt:rest) = expressionBin rest OGt
expression (TLBrac:TKw FCons:rest) = expressionBin rest OCons
expression (TLBrac:TKw Eq:rest) = expressionBin rest OEq

-- build in unary operations
expression (TLBrac:TKw Car:rest) = expressionUnary rest UCar
expression (TLBrac:TKw Cdr:rest) = expressionUnary rest UCdr

expression (TLBrac:TKw Null:rest) = (ENull, rest) 

--functions
expression (TLBrac:TKw Lambda:rest) = lambdaexpr rest
expression (TLBrac:TKw Letrec:rest) = letrec rest

-- null keyword
expression (TKw Null : rest) = (ENull, rest)

-- parsing if-else expression
expression (TLBrac:TKw If:rest) =
    let (cond, t1) = factor rest
        (thenB, t2) = factor t1
        (elseB, toks) = factor t2
    in
    case toks of
        (TRBrac:rest) -> (EIf cond thenB elseB, rest)
        _ -> (EError, toks)

-- parsing function applications
expression (TLBrac : tok : rest) =
    let (callable, t1) = factor (tok : rest)
        (args, t2) = factors t1 []
    in
    case (callable, t2) of
        (EError, _) -> (EError, t1)
        (_, TRBrac:x) -> (ECall callable args, x)
        (_, _) -> (EError, tok:rest)

expression (TTick : rest) = dataexpr rest

expression (_:rest) = (EError, rest)
expression [] = (EError, [])

-- parsing of letrec expression
letrec :: [Token] -> (Expr, [Token])
letrec (TLBrac : TIdent name : TRBrac : TLBrac : rest) = 
    let (reclamb, t1) = factor rest in
    case (reclamb, t1) of
        (ELambda _ _, TRBrac : t2) -> 
            case factor t2 of
                (EError, _) -> (EError, rest)
                (expr, TRBrac : t3) ->(ELetrec name reclamb expr, t3)
                (_, _) -> (EError, rest)
        (_, _) -> (EError, rest)

letrec toks = (EError, toks)

-- parsing of lambda
lambdaexpr :: [Token] -> (Expr, [Token])
lambdaexpr (TLBrac : rest) = 
    let (args, t1) = paramexpr rest
        (body, t2) = expression t1
    in
    case (args, body, t2) of
        (Just a, _, (TRBrac : toks)) -> (ELambda a body, toks)
        (_, _, _) -> (EError, rest)
lambdaexpr toks = (EError, toks)

paramexpr :: [Token] -> (Maybe [String], [Token])
paramexpr (TRBrac : rest) = (Just [], rest)
paramexpr (TIdent s : rest) =
    let (others, toks) = paramexpr rest in
    case others of
        Just o -> (Just $ s : o, toks)
        Nothing -> (Nothing, rest)
paramexpr toks = (Nothing, toks)

-- parsing of build in binary operation
expressionBin :: [Token] -> BinOp -> (Expr, [Token])
expressionBin t op = 
    let (l, toksl) = factor t
        (r, toks) = factor toksl
    in
    case toks of
      (TRBrac:rest) -> (EBinOp op l r, rest)
      _ -> (EError, toks)

-- parsing of build in unary operation
expressionUnary :: [Token] -> UnOp -> (Expr, [Token])
expressionUnary t op =
    let (e, toks) = factor t in
    case toks of
        (TRBrac : rest) -> (EUnaryOp op e, rest)
        _ -> (EError, toks)
    
-- parsing of just data (aka anything starting with ')
dataexpr :: [Token] -> (Expr, [Token])
dataexpr (TKw Null : rest) = (ENull, rest)
dataexpr (TNumber n : rest) = (ENum n, rest)
dataexpr (TLBrac : rest) = listdata rest
dataexpr toks = (EError, toks)

listdata :: [Token] -> (Expr, [Token])
listdata (TRBrac : rest) = (ENull, rest)
listdata toks =
    let (car, t1) = dataexpr toks
        (cdr, t2) = listdata t1
    in
    case (car, cdr) of
        (EError, _) -> (EError, toks)
        (_, EError) -> (EError, toks)
        (_, _) -> (EBinOp OCons car cdr, t2)

-- basic things (aka numbers, data and idents)
factor :: [Token] -> (Expr, [Token])
factor (TTick : rest) = dataexpr rest
factor (TNumber n : rest) = (ENum n, rest)
factor (TIdent s : rest) = (EIdent s, rest)
factor (TKw Null : rest) = (ENull, rest)
factor (TLBrac:rest) = expression (TLBrac : rest)
factor (_ : rest) = (EError, rest)
factor [] = (EError, [])

-- list of factors
factors :: [Token] -> [Expr] -> ([Expr], [Token])
factors toks acc =
    let (f, t) = factor toks in 
    case f of
        EError -> (acc, toks)
        fact -> factors t (acc ++ [fact])