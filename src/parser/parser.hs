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
    let (e, toks) = expression t
    in
    parseImp (acc ++ [e]) toks

expression :: [Token] -> (Expr, [Token])
expression (TNumber n : rest) = (ENum n, rest)
expression (TLBrac:TAdd:rest) = expressionBin rest OAdd
expression (TLBrac:TSub:rest) = expressionBin rest OSub
expression (TLBrac:TMul:rest) = expressionBin rest OMul
expression (TLBrac:TDiv:rest) = expressionBin rest ODiv
expression (TLBrac:TLt:rest) = expressionBin rest OLt
expression (TLBrac:TGt:rest) = expressionBin rest OGt
expression (TLBrac:TKw FCons:rest) = expressionBin rest OCons
expression (TLBrac:TKw Eq:rest) = expressionBin rest OEq
expression (TLBrac:TKw Null:rest) = (ENull, rest) 
expression (TLBrac:TKw If:rest) =
    let (cond, t1) = expression rest
        (thenB, t2) = expression t1
        (elseB, toks) = expression t2
    in
    case toks of
        (TRBrac:rest) -> (EIf cond thenB elseB, rest)
        _ -> (EError, toks)
expression (_:rest) = (EError, rest)
expression [] = (EError, [])

expressionBin :: [Token] -> BinOp -> (Expr, [Token])
expressionBin t op = 
    let (l, toksl) = expression t
        (r, toks) = expression toksl
    in
    case toks of
      (TRBrac:rest) -> (EBinOp op l r, rest)
      _ -> (EError, [])
