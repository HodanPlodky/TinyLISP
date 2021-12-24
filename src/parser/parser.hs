{-# OPTIONS_GHC -Wall -Wno-name-shadowing -dynamic #-}
module Parser (parse, parseImp) where

import Lexer
import Ast
import Debug.Trace

parse :: [Token] -> Expr
parse toks =
    case parseImp [] toks of
        (e, []) -> e
        _ -> EError

parseImp :: [Expr] -> [Token] -> (Expr, [Token])
--parseImp t e | trace ("parseImp " ++ show t ++ " " ++ show e) False = undefined
parseImp acc [] = (EList acc, [])
parseImp acc [TEof] = (EList acc, [])
parseImp acc t = 
    let (e, toks) = expression t in
    case e of
        EError -> (EError, toks)
        _ -> parseImp (acc ++ [e]) toks

expression :: [Token] -> (Expr, [Token])
--expression t | trace ("expression " ++ show t) False = undefined
expression (TLBrac:TAdd:rest) = expressionBin rest OAdd
expression (TLBrac:TSub:rest) = expressionBin rest OSub
expression (TLBrac:TMul:rest) = expressionBin rest OMul
expression (TLBrac:TDiv:rest) = expressionBin rest ODiv
expression (TLBrac:TLt:rest) = expressionBin rest OLt
expression (TLBrac:TGt:rest) = expressionBin rest OGt
expression (TLBrac:TKw FCons:rest) = expressionBin rest OCons
expression (TLBrac:TKw Eq:rest) = expressionBin rest OEq
expression (TLBrac:TKw Null:rest) = (ENull, rest) 
expression (TLBrac:TKw Lambda:rest) = lambdaexpr rest
expression (TLBrac:TKw Letrec:rest) = letrec rest
expression (TKw Null : rest) = (ENull, rest)
expression (TLBrac:TKw If:rest) =
    let (cond, t1) = factor rest
        (thenB, t2) = factor t1
        (elseB, toks) = factor t2
    in
    case toks of
        (TRBrac:rest) -> (EIf cond thenB elseB, rest)
        _ -> (EError, toks)

expression (TLBrac : tok : rest) =
    let (callable, t1) = factor (tok : rest)
        (args, t2) = factors t1 []
    in
    case (callable, t2) of
        (EError, _) -> (EError, t1)
        (_, TRBrac:x) -> (ECall callable args, x)
        (_, _) -> (EError, tok:rest)
    
expression (_:rest) = (EError, rest)
expression [] = (EError, [])

letrec :: [Token] -> (Expr, [Token])
letrec (TLBrac : TIdent name : TRBrac : TLBrac : rest) = 
    let (reclamb, t1) = factor rest in
    case (reclamb, t1) of
        (ELambda args body, TRBrac : t2) -> 
            case factor t2 of
                (EError, _) -> (EError, rest)
                (expr, TRBrac : t3) ->(ELetrec name reclamb expr, t3)
                (_, _) -> (EError, rest)
        (_, _) -> (EError, rest)

letrec toks = (EError, toks)

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

expressionBin :: [Token] -> BinOp -> (Expr, [Token])
--expressionBin t e | trace ("expressionBin " ++ show t ++ " " ++ show e) False = undefined
expressionBin t op = 
    let (l, toksl) = factor t
        (r, toks) = factor toksl
    in
    case toks of
      (TRBrac:rest) -> (EBinOp op l r, rest)
      _ -> (EError, toks)

factor :: [Token] -> (Expr, [Token])
--factor t | trace ("factor " ++ show t) False = undefined
factor (TNumber n : rest) = (ENum n, rest)
factor (TIdent s : rest) = (EIdent s, rest)
factor (TKw Null : rest) = (ENull, rest)
factor (TLBrac:rest) = expression (TLBrac : rest)
factor (_ : rest) = (EError, rest)
factor [] = (EError, [])

factors :: [Token] -> [Expr] -> ([Expr], [Token])
--factors t e | trace ("factors " ++ show t ++ " " ++ show e) False = undefined
factors toks acc =
    let (f, t) = factor toks in 
    case f of
        EError -> (acc, toks)
        fact -> factors t (acc ++ [fact])