{-# OPTIONS_GHC -Wall -Wno-name-shadowing -dynamic #-}
module Ast (Inst(..), Expr(..), BinOp(..), UnOp(..), appendExpr, generate, save) where

import qualified Data.ByteString.Lazy as BIN
import Data.Binary.Put
import Data.Binary
import System.IO
import Data.Map


--
-- Ast data types
--

data BinOp
    = OAdd
    | OSub
    | OMul
    | ODiv
    | OLt
    | OGt
    | OEq
    | OCons
    deriving Show

data UnOp
    = UCar
    | UCdr
    deriving Show

data Expr
    = ENum Int
    | EIdent String
    | ECons Expr Expr
    | ENull
    | EFunc
    | EBinOp BinOp Expr Expr
    | EUnaryOp UnOp Expr
    | EIf   { cond :: Expr
            , thenB :: Expr
            , elseb :: Expr
            }
    | ELambda [String] Expr --arguments and body
    | ECall Expr [Expr] -- callable expression and arguments
    | EList [Expr]
    | ELetrec String Expr Expr --name, recursive-lambda body
    | EError
    deriving Show

appendExpr :: Expr -> Expr -> Expr
appendExpr (EList l) e = EList (l ++ [e])
appendExpr e1 e2 = EList [e1, e2]


--
-- SECD nstructions
--

data Inst
    = InstList [Inst]
    | ERR
    | LDC Int
    | NIL
    | ADD
    | SUB
    | MUL
    | DIV
    | EQ
    | GT
    | LT
    | CONS
    | CAR
    | CDR
    | CONSP
    | SEL
    | JOIN
    | LD Int Int
    | LDF Inst
    | AP
    | RTN
    | DUM
    | RAP
    | PRT
    | READ
    deriving (Show, Eq, Ord)

appendInst :: Inst -> Inst -> Inst
appendInst (InstList l1) (InstList l2) = InstList (l1 ++ l2)
appendInst (InstList l) i = InstList (l ++ [i])
appendInst i1 i2 = InstList [i1, i2]


--
-- Generating secd instruction from ast
--

generate :: Expr -> [[String]] -> Inst
-- basic data types
generate (ENull) _ = InstList [NIL]
generate (ENum n) _ = LDC n
generate (EIdent s) names =
    let lx = Prelude.filter (\i -> elem s (names !! i)) [0..] in
    case lx of
        [] -> ERR
        x -> case Prelude.filter (\i -> s == (names !! head x) !! i) [0..] of
            [] -> ERR
            y -> LD (head x) (head y)

-- build in unary operations
generate (EUnaryOp UCar x) names =
    InstList [generate x names, CAR]
generate (EUnaryOp UCdr x) names =
    InstList [generate x names, CDR]

-- build in binary operations
generate (EBinOp OAdd x y) names = 
    InstList [generate x names , generate y names, ADD]
generate (EBinOp OSub x y) names = 
    InstList [generate x names, generate y names, SUB]
generate (EBinOp OEq x y) names = 
    InstList [generate x names, generate y names, Ast.EQ]
generate (EBinOp OGt x y) names = 
    InstList [generate x names, generate y names, Ast.GT]
generate (EBinOp OLt x y) names =
    InstList [generate x names, generate y names, Ast.LT]
generate (EBinOp OMul x y) names = 
    InstList [generate x names, generate y names, MUL]
generate (EBinOp ODiv x y) names = 
    InstList [generate x names, generate y names, DIV]
generate (EBinOp OCons x y) names = 
    InstList [generate y names, generate x names, CONS]

-- flow control
generate (EIf cond thenB elseB) names = 
    InstList    [ generate cond names, SEL
                , appendInst (generate thenB names) JOIN
                , appendInst (generate elseB names) JOIN
                ]

-- lists
generate (EList exprs) names = 
    InstList $ Prelude.map (\x ->generate x names)  exprs

-- functions and their applications
generate (ELambda args body) names =
    LDF $ appendInst (generate body (args : names)) RTN
generate (ECall callable args) names =
    appendInst
        (appendInst
            (InstList $ 
                Prelude.foldr (\x acc -> acc ++ [generate x names, CONS]) [NIL] args) 
            (generate callable names))
        AP
generate (ELetrec name reclamb body) names =
    let nnames = [name] : names in
    appendInst 
        (appendInst (InstList [DUM, NIL]) (generate reclamb nnames)) 
        (appendInst (InstList [CONS]) 
            (appendInst 
                (generate (ELambda [name] body) names)
                RAP))

-- errors
generate (EError) _ = ERR
generate _ _ = NIL


--
-- This part of code handles saving 
-- generated instuction to file
--

save :: String -> Inst -> IO ()
save path (InstList insts) = do
    h_out <- openFile path WriteMode
    saveImpl insts h_out
    hClose h_out
save path inst = do
    h_out <- openFile path WriteMode
    saveImpl [inst] h_out
    hClose h_out

-- code for instruction without any intresting going on
simpleSaves :: Map Inst Word64
simpleSaves = fromList
    [ (ADD, 0x01)
    , (NIL, 0x03)
    , (SUB, 0x04)
    , (MUL, 0x05)
    , (DIV, 0x06)
    , (CONS, 0x07)
    , (CAR, 0x08)
    , (CDR, 0x09)
    , (CONSP, 0x0a)
    , (SEL, 0x0b)
    , (JOIN, 0x0c)
    , (AP, 0x0f)
    , (RTN, 0x10)
    , (Ast.EQ, 0x11)
    , (Ast.GT, 0x12)
    , (Ast.LT, 0x13)
    , (DUM, 0x14)
    , (RAP, 0x15)
    , (PRT, 0x16)
    , (READ, 0x17)
    , (ERR, 0xfe)
    ]

saveImpl :: [Inst] -> Handle -> IO ()
-- intresting (aka must do more then just write out code) instrustions
saveImpl (InstList insts : rest) outfile = do
    BIN.hPut outfile $ runPut (putWord64be 0x00)
    saveImpl insts outfile
    BIN.hPut outfile $ runPut (putWord64be 0xff)
    saveImpl rest outfile

saveImpl (LDC num : rest) outfile = do
    BIN.hPut outfile $ runPut (putWord64be 0x02)
    BIN.hPut outfile $ runPut (put num)
    saveImpl rest outfile


saveImpl (LD x y : rest) outfile = do
    BIN.hPut outfile $ runPut (putWord64be 0x0d)
    BIN.hPut outfile $ runPut (put x)
    BIN.hPut outfile $ runPut (put y)
    saveImpl rest outfile

saveImpl (LDF (InstList insts) : rest) outfile = do
    BIN.hPut outfile $ runPut (putWord64be 0x0e)
    saveImpl ([InstList insts]) outfile
    saveImpl rest outfile

saveImpl (LDF insts : rest) outfile = do
    BIN.hPut outfile $ runPut (putWord64be 0x0e)
    saveImpl ([InstList [insts]]) outfile
    saveImpl rest outfile

-- not intresting instructions
saveImpl (x : rest) outfile = 
    let word = case Data.Map.lookup x simpleSaves of
            Just w -> w
            Nothing -> 0xff
    in
    do
    BIN.hPut outfile $ runPut (putWord64be word)
    saveImpl rest outfile

-- end of instructions
saveImpl [] _ = return () 