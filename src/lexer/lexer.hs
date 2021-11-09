{-# OPTIONS_GHC -Wall -Wno-name-shadowing -dynamic #-}
module Lexer (Token (..) , getTokens) where
import Data.Char
{-
data Position = Position
    {
        line :: Integer,
        col :: Integer,
        file :: String
    }
--}

data Keyword
    = Defun
    | If
    deriving Show

checkIdent :: String -> Token
checkIdent name =
    case name of
        "if" -> Kw If
        "defun" -> Kw Defun
        n -> Ident n
        

data Token
    = Eof
    | Error String
    | Ident String
    | Kw Keyword
    | Number Integer
    | RNumber Double
    | Add
    | Sub
    | Mul
    | Div
    | LBrac
    | RBrac
    | Dot
    | Tick
    deriving Show

alfa :: [Char]
alfa = ['a'..'z'] ++ ['A'..'Z']

nums :: [Char]
nums = ['0'..'9']

whiteSpace :: [Char]
whiteSpace = ['\n', '\r', '\r', ' ']

match :: a -> [(a -> Bool, b)] -> Maybe b
match item [] = Nothing
match item (test:rest) =
    if ((fst test) item)
       then Just $ snd test
       else (match item rest)

elemCh :: (Foldable t, Eq a) => t a -> a -> Bool
elemCh l e = elem e l

getTokens :: String -> [Token]
getTokens "" = [Eof]
getTokens (c:rest) =
    let toks = 
            match c 
            [
            (elemCh whiteSpace, getTokens rest),
            (elemCh alfa, stateIdent (c:rest) ""),
            (elemCh nums, stateStartNum (c:rest)),
            (\_ -> True, [Eof])
            ]
    in
    case toks of
      Just val -> val
      Nothing -> [Eof]

stateIdent :: String -> String -> [Token]
stateIdent "" acc = [checkIdent acc]
stateIdent (c:rest) acc =
    if (elem c alfa)
       then stateIdent rest (acc ++ [c])
       else checkIdent acc : getTokens (c:rest)

stateStartNum :: String -> [Token]
stateStartNum ('0':'x':c:rest) = stateHex (c:rest) 0
stateStartNum ('0':'X':c:rest) = stateHex (c:rest) 0
stateStartNum ('0':c:rest) = stateNum (c:rest) 0 8
stateStartNum (c:rest) = stateNum (c:rest) 0 10
stateStartNum _ = [Error "Not a number"]

checkNum :: Char -> Integer -> Maybe Integer
checkNum c base =
    let n = toInteger $ ord c - ord '0' 
    in
    if (elem n [0..(base - 1)])
       then Just n
       else Nothing

stateNum :: String -> Integer -> Integer -> [Token]
stateNum "" acc _ = [Number acc]
stateNum (c:rest) acc base =
    case n of
        Just n -> stateNum rest (acc * base + n) base
        Nothing -> Number acc : getTokens (c:rest)
    where n = checkNum c base

checkHex :: Char -> Maybe Integer
checkHex c
    | elem c nums = checkNum c 16
    | elem c ['A'..'F'] = Just . toInteger $ (ord c - ord 'A') + 10
    | otherwise = Nothing

stateHex :: String -> Integer -> [Token]
stateHex  "" acc = [Number acc]
stateHex (c:rest) acc =
    case n of
        Just n -> stateHex rest (acc * 16 + n)
        Nothing -> Number acc : getTokens (c:rest)
    where n = checkHex c

