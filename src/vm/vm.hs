{-# OPTIONS_GHC -Wall -Wno-name-shadowing -dynamic #-}
module Vm (startvm, StackData(..), ConsCell(..)) where

import Ast
--import Debug.Trace

data ConsCell
    = Empty
    | Cell StackData StackData
    deriving Show

data StackData
    = DataNum Int
    | Cons ConsCell
    | Code Inst
    deriving Show

data State = State
    { code :: [Inst]
    , stack :: [StackData]
    , dump :: [Inst]
    , enviroment :: [[StackData]]
    }
    deriving Show

startvm :: Inst -> [StackData]
startvm inst = 
    let result = runvm $ State [inst] [] [] []
    in
    (reverse . stack) result

runvm :: State -> State 
--runvm s | trace ("runvm " ++ show s) False = undefined
runvm (State (InstList l : rest) stack dump env) =
    runvm $ State (l ++ rest) stack dump env

runvm (State (LDC x : rest) stack dump env) =
    runvm $ State rest (DataNum x:stack) dump env

runvm (State (ADD : inst) (DataNum x: DataNum y:stack) dump env) =
    runvm $ State inst (DataNum (x+y):stack) dump env

runvm (State (SUB : inst) (DataNum x: DataNum y:stack) dump env) =
    runvm $ State inst (DataNum (y-x):stack) dump env

runvm (State (MUL : inst) (DataNum x: DataNum y:stack) dump env) =
    runvm $ State inst (DataNum (x*y):stack) dump env

runvm (State (DIV : inst) (DataNum x: DataNum y:stack) dump env) =
    runvm $ State inst (DataNum (div y x):stack) dump env

runvm (State (SEL:x:y:inst)(DataNum c:stack) dump env) =
    let ninst = if (c /= 0) then [x] else [y]
        ndump = InstList inst : dump
    in
    runvm $ State ninst stack ndump env

runvm (State (JOIN:inst) stack (x:dump) env) =
    runvm $ State (x:inst) stack dump env

runvm (State (CONS:inst) (x:y:stack) dump env) =
    runvm $ State inst (Cons (Cell x y) : stack) dump env

runvm (State (NIL:inst) stack dump env) =
    runvm $ State inst (Cons Empty : stack) dump env

runvm (State [] stack dump env) =
    State [] stack dump env

runvm s = s
