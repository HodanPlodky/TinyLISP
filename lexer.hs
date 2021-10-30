{-# OPTIONS_GHC -Wall -Wno-name-shadowing -dynamic #-}
module Lexer (Token (..) , getTokens) where

import Data.List
import Data.Char

data Token
    = Eof
    | Eps
    | Ident String
    | Number Integer
    | Add
    | Sub
    | Mul
    | Div
    | LeftBraces
    | RightBraces
    deriving (Show, Eq)

alfa :: [Char]
alfa = ['a'..'z'] ++ ['A'..'Z']

nums :: [Char]
nums = ['0'..'9']

whiteSpaces :: [Char]
whiteSpaces = ['\n', ' ', '\t', '\r']

getTokens :: String -> [Token]
getTokens "" = [Eof]
getTokens (x:xs)
    | elem x nums = stateNum 0 (x:xs)
    | elem x whiteSpaces = getTokens xs
    | elem x alfa = stateIdent [x] (xs)
    | otherwise = 
        (case x of
            '+' -> Add
            '-' -> Sub
            '*' -> Mul
            '/' -> Div
            '(' -> LeftBraces
            ')' -> RightBraces
            _ -> Eps
        ) : getTokens xs

stateNum :: Integer -> String -> [Token]
stateNum n "" = [Number n]
stateNum n (x:xs)
    | elem x nums = stateNum (10*n + nn) xs
    | otherwise = Number n : getTokens (x:xs)
    where nn = toInteger $ ord x - ord '0'

stateIdent :: String -> String -> [Token]
stateIdent text "" = [Ident text]
stateIdent text (x:xs)
    | elem x (nums ++ alfa) = stateIdent (text ++ [x]) xs
    | otherwise = Ident text : getTokens (x:xs)
