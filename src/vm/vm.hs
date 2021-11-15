{-# OPTIONS_GHC -Wall -Wno-name-shadowing -dynamic #-}
module Vm (startvm, StackData(..), ConsCell(..)) where

import Ast
--import Debug.Trace

data ConsCell
    = Empty
    | Cell StackData StackData
    deriving Show

data StackData
    = DataNum Integer
    | Cons ConsCell
    deriving Show

data State = State
    { code :: [Inst]
    , stack :: [StackData]
    , dump :: [Inst]
    }
    deriving Show

startvm :: Inst -> [StackData]
startvm inst = 
    let result = runvm $ State [inst] [] []
    in
    (reverse . stack) result

runvm :: State -> State 
--runvm s | trace ("runvm " ++ show s) False = undefined
runvm (State (InstList l : rest) stack dump) =
    runvm $ State (l ++ rest) stack dump

runvm (State (LDC x : rest) stack dump) =
    runvm $ State rest (DataNum x:stack) dump

runvm (State (ADD : inst) (DataNum x: DataNum y:stack) dump) =
    runvm $ State inst (DataNum (x+y):stack) dump

runvm (State (SUB : inst) (DataNum x: DataNum y:stack) dump) =
    runvm $ State inst (DataNum (y-x):stack) dump

runvm (State (MUL : inst) (DataNum x: DataNum y:stack) dump) =
    runvm $ State inst (DataNum (x*y):stack) dump

runvm (State (DIV : inst) (DataNum x: DataNum y:stack) dump) =
    runvm $ State inst (DataNum (div y x):stack) dump

runvm (State (SEL:x:y:inst)(DataNum c:stack) dump) =
    let ninst = if (c /= 0) then [x] else [y]
        ndump = InstList inst : dump
    in
    runvm $ State ninst stack ndump

runvm (State (JOIN:inst) stack (x:dump)) =
    runvm $ State (x:inst) stack dump

runvm (State (CONS:inst) (x:y:stack) dump) =
    runvm $ State inst (Cons (Cell x y) : stack) dump

runvm (State (NIL:inst) stack dump) =
    runvm $ State inst (Cons Empty : stack) dump

runvm (State [] stack dump) =
    State [] stack dump

runvm s = s
