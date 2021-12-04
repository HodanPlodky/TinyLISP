{-# OPTIONS_GHC -Wall -Wno-name-shadowing -dynamic #-}
module Ast (Inst(..), Expr(..), BinOp(..), appendExpr, generate, save) where

import qualified Data.ByteString.Lazy as BIN
import Data.Binary.Put
import Data.Binary
import System.IO
--import Data.Bits.Extras


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

data Expr
    = ENum Int
    | ECons Expr Expr
    | ENull
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
    | LDC Int
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
    | LD Int Int
    | LDF Inst
    | AP
    | RTN
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
generate (EBinOp OCons x y) = InstList [generate y, generate x, CONS]
generate (EIf cond thenB elseB) = 
    InstList    [ generate cond, SEL
                , appendInst (generate thenB) JOIN
                , appendInst (generate elseB) JOIN
                ]
generate (EList exprs) = InstList $ map generate  exprs
generate (ENull) = InstList [NIL]
generate (EError) = NIL
generate _ = NIL

save :: String -> Inst -> IO ()
save path (InstList insts) = do
    h_out <- openFile path WriteMode
    saveImpl insts h_out
    hClose h_out
save path inst = do
    h_out <- openFile path WriteMode
    saveImpl [inst] h_out
    hClose h_out

saveImpl :: [Inst] -> Handle -> IO ()
saveImpl (InstList insts : rest) outfile = do
    BIN.hPut outfile $ runPut (putWord64be 0x00)
    saveImpl insts outfile
    BIN.hPut outfile $ runPut (putWord64be 0xff)
    saveImpl rest outfile

saveImpl (ADD : rest) outfile = do
    BIN.hPut outfile $ runPut (putWord64be 0x01)
    saveImpl rest outfile

saveImpl (LDC num : rest) outfile = do
    BIN.hPut outfile $ runPut (putWord64be 0x02)
    BIN.hPut outfile $ runPut (put num)
    saveImpl rest outfile

saveImpl (NIL : rest) outfile = do
    BIN.hPut outfile $ runPut (putWord64be 0x03)
    saveImpl rest outfile

saveImpl (SUB : rest) outfile = do
    BIN.hPut outfile $ runPut (putWord64be 0x04)
    saveImpl rest outfile

saveImpl (MUL : rest) outfile = do
    BIN.hPut outfile $ runPut (putWord64be 0x05)
    saveImpl rest outfile

saveImpl (DIV : rest) outfile = do
    BIN.hPut outfile $ runPut (putWord64be 0x06)
    saveImpl rest outfile

saveImpl (CONS : rest) outfile = do
    BIN.hPut outfile $ runPut (putWord64be 0x07)
    saveImpl rest outfile

saveImpl (CAR : rest) outfile = do
    BIN.hPut outfile $ runPut (putWord64be 0x08)
    saveImpl rest outfile

saveImpl (CDR : rest) outfile = do
    BIN.hPut outfile $ runPut (putWord64be 0x09)
    saveImpl rest outfile

saveImpl (CONSP : rest) outfile = do
    BIN.hPut outfile $ runPut (putWord64be 0x0a)
    saveImpl rest outfile

saveImpl (SEL : rest) outfile = do
    BIN.hPut outfile $ runPut (putWord64be 0x0b)
    saveImpl rest outfile

saveImpl (JOIN : rest) outfile = do
    BIN.hPut outfile $ runPut (putWord64be 0x0c)
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

saveImpl (AP : rest) outfile = do
    BIN.hPut outfile $ runPut (putWord64be 0x0f)
    saveImpl rest outfile

saveImpl (RTN : rest) outfile = do
    BIN.hPut outfile $ runPut (putWord64be 0x10)
    saveImpl rest outfile

saveImpl [] _ = putStrLn "Finnish" 