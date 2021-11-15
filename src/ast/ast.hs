{-# OPTIONS_GHC -Wall -Wno-name-shadowing -dynamic #-}
module Ast (Inst(..), Expr(..), BinOp(..), appendExpr, generate) where

data BinOp
    = OAdd
    | OSub
    | OMul
    | ODiv
    | OLt
    | OGt
    | OEq
    deriving Show

data Expr
    = ENum Integer
    | ECons Expr Expr
    | EFunc
    | EBinOp BinOp Expr Expr
    | EIf   { cond :: Expr
            , thenB :: Expr
            , elseb :: Expr
            }
    | EList [Expr]
    | EError
    deriving Show

appendExpr :: Expr -> Expr -> Expr
appendExpr (EList l) e = EList (l ++ [e])
appendExpr e1 e2 = EList [e1, e2]

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
    deriving Show

appendInst :: Inst -> Inst -> Inst
appendInst (InstList l) i = InstList (l ++ [i])
appendInst i1 i2 = InstList [i1, i2]

generate :: Expr -> Inst
generate (ENum n) = LDC n
generate (EBinOp OAdd x y) = InstList [generate x, generate y, ADD]
generate (EBinOp OSub x y) = InstList [generate x, generate y, SUB]
generate (EBinOp OEq x y) = 
    InstList [
                InstList [generate x, generate y, SUB],
                SEL,
                InstList [LDC 0, JOIN],
                InstList [LDC 1, JOIN]
             ]
generate (EBinOp OMul x y) = InstList [generate x, generate y, MUL]
generate (EBinOp ODiv x y) = InstList [generate x, generate y, DIV]
generate (EIf cond thenB elseB) = 
    InstList    [ generate cond, SEL
                , appendInst (generate thenB) JOIN
                , appendInst (generate elseB) JOIN
                ]
generate (EList exprs) = InstList $ map generate  exprs
generate (EError) = NIL
generate _ = NIL
