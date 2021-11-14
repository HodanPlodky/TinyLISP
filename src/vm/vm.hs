{-# OPTIONS_GHC -Wall -Wno-name-shadowing -dynamic #-}
module Vm (startvm) where

import Ast
--import Debug.Trace

data State = State
    { code :: [Inst]
    , stack :: [Integer]
    , dump :: [Inst]
    }
    deriving Show

startvm :: Inst -> [Integer]
startvm inst = 
    let result = runvm $ State [inst] [] []
    in
    (reverse . stack) result

runvm :: State -> State 
--runvm s | trace ("runvm " ++ show s) False = undefined
runvm (State (InstList l : rest) stack dump) =
    runvm $ State (l ++ rest) stack dump

runvm (State (LDC x : rest) stack dump) =
    runvm $ State rest (x:stack) dump

runvm (State (ADD : inst) (x:y:stack) dump) =
    runvm $ State inst ((x+y):stack) dump

runvm (State (SUB : inst) (x:y:stack) dump) =
    runvm $ State inst ((y-x):stack) dump

runvm (State (MUL : inst) (x:y:stack) dump) =
    runvm $ State inst ((x*y):stack) dump

runvm (State (DIV : inst) (x:y:stack) dump) =
    runvm $ State inst ((div y x):stack) dump

runvm (State [] stack dump) =
    State [] stack dump

runvm s = s
