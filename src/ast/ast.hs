{-# OPTIONS_GHC -Wall -Wno-name-shadowing -dynamic #-}
module Ast (Expr(..)) where

import Data.List

data Expr
    = ENum Integer
    | ECons Expr Expr
    | EFunc
    | EAdd Expr Expr
    | ESub Expr Expr
    | EMul Expr Expr
    | EDiv Expr Expr
    | EIf   { cond :: Expr
            , thenB :: Expr
            , elseb :: Expr
            }

data Inst
    = InstList [Inst]
    | LDC Integer
    | NIL
    | ADD
    | SUB
    | MUL
    | DIV
    | CONS
    | CAR
    | CDR
    | CONSP
    | SEL
    | JOIN

appendInst :: Inst -> Inst -> Inst
appendInst (InstList l) i = InstList (l ++ [i])
appendInst i1 i2 = InstList [i1, i2]

generate :: Expr -> Inst
generate (ENum n) = LDC n
generate (EAdd x y) = InstList [generate x, generate y, ADD]
generate (ESub x y) = InstList [generate x, generate y, SUB]
generate (EMul x y) = InstList [generate x, generate y, MUL]
generate (EDiv x y) = InstList [generate x, generate y, DIV]
generate (EIf cond thenB elseB) = 
    InstList    [ generate cond, SEL
                , appendInst (generate thenB) JOIN
                , appendInst (generate elseB) JOIN
                ]
