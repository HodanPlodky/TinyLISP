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
expression (TLBrac:TAdd:rest) = expressionBin rest OAdd
expression (TLBrac:TSub:rest) = expressionBin rest OSub
expression (TLBrac:TMul:rest) = expressionBin rest OMul
expression (TLBrac:TDiv:rest) = expressionBin rest ODiv
expression (TLBrac:TLt:rest) = expressionBin rest OLt
expression (TLBrac:TGt:rest) = expressionBin rest OGt
expression (TLBrac:TKw FCons:rest) = expressionBin rest OCons
expression (TLBrac:TKw Eq:rest) = expressionBin rest OEq
expression (TLBrac:TKw Null:rest) = (ENull, rest) 
expression (TKw Null : rest) = (ENull, rest)
expression (TLBrac:TKw If:rest) =
    let (cond, t1) = factor rest
        (thenB, t2) = factor t1
        (elseB, toks) = factor t2
    in
    case toks of
        (TRBrac:rest) -> (EIf cond thenB elseB, rest)
        _ -> (EError, toks)
expression (_:rest) = (EError, rest)
expression [] = (EError, [])

expressionBin :: [Token] -> BinOp -> (Expr, [Token])
expressionBin t op = 
    let (l, toksl) = factor t
        (r, toks) = factor toksl
    in
    case toks of
      (TRBrac:rest) -> (EBinOp op l r, rest)
      _ -> (EError, toks)

factor :: [Token] -> (Expr, [Token])
factor (TNumber n : rest) = (ENum n, rest)
factor (TLBrac:rest) = expression (TLBrac : rest)
factor (_ : rest) = (EError, rest)