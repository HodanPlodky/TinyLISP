{-# OPTIONS_GHC -Wall -Wno-name-shadowing -dynamic #-}
module Lexer (Token (..), Keyword(..), getTokens) where
import Data.Char

data Keyword
    = Defun
    | If
    | Eq
    | FCons
    | Null
    | Define
    deriving Show

checkIdent :: String -> Token
checkIdent name =
    case name of
        "if" -> TKw If
        "defun" -> TKw Defun
        "eq?" -> TKw Eq
        "cons" -> TKw FCons
        "null" -> TKw Null
        "define" -> TKw Define
        n -> TIdent n

data Token
    = TEof
    | TError String
    | TIdent String
    | TKw Keyword
    | TNumber Integer
    | TRNumber Double
    | TAdd
    | TSub
    | TMul
    | TDiv
    | TLt
    | TGt
    | TLBrac
    | TRBrac
    | TDot
    | TTick
    deriving Show

alfa :: [Char]
alfa = ['a'..'z'] ++ ['A'..'Z']

identChars :: [Char]
identChars = alfa ++ ['_', '?']

nums :: [Char]
nums = ['0'..'9']

whiteSpace :: [Char]
whiteSpace = ['\n', '\r', '\r', ' ']

match :: a -> [(a -> Bool, b)] -> Maybe b
match _ [] = Nothing
match item (test:rest) =
    if ((fst test) item)
       then Just $ snd test
       else (match item rest)

elemCh :: (Foldable t, Eq a) => t a -> a -> Bool
elemCh l e = elem e l

getTokens :: String -> [Token]
getTokens "" = [TEof]
getTokens (c:rest) =
    let toks = 
            match c 
            [
            (elemCh whiteSpace, getTokens rest),
            (elemCh alfa, stateIdent (rest) [c]),
            (elemCh nums, stateStartNum (c:rest)),
            (\_ -> True,
                (case c of
                    '(' -> TLBrac
                    ')' -> TRBrac
                    '.' -> TDot
                    '\'' -> TTick
                    '+' -> TAdd
                    '-' -> TSub
                    '*' -> TMul
                    '/' -> TDiv
                    '<' -> TLt
                    '>' -> TGt
                    _ -> TError "Undefined token") : getTokens rest
            )
            ]
    in
    case toks of
      Just val -> val
      Nothing -> [TError "Cannot lex"]

stateIdent :: String -> String -> [Token]
stateIdent "" acc = [checkIdent acc]
stateIdent (c:rest) acc =
    if (elem c identChars)
       then stateIdent rest (acc ++ [c])
       else checkIdent acc : getTokens (c:rest)

stateStartNum :: String -> [Token]
stateStartNum ('0':'x':c:rest) = stateHex (c:rest) 0
stateStartNum ('0':'X':c:rest) = stateHex (c:rest) 0
stateStartNum ('0':c:rest) = stateNum (c:rest) 0 8
stateStartNum (c:rest) = stateNum (c:rest) 0 10
stateStartNum _ = [TError "Not a number"]

checkNum :: Char -> Integer -> Maybe Integer
checkNum c base =
    let n = toInteger $ ord c - ord '0' 
    in
    if (elem n [0..(base - 1)])
       then Just n
       else Nothing

stateNum :: String -> Integer -> Integer -> [Token]
stateNum "" acc _ = [TNumber acc]
stateNum (c:rest) acc base =
    case n of
        Just n -> stateNum rest (acc * base + n) base
        Nothing -> TNumber acc : getTokens (c:rest)
    where n = checkNum c base

checkHex :: Char -> Maybe Integer
checkHex c
    | elem c nums = checkNum c 16
    | elem c ['A'..'F'] = Just . toInteger $ (ord c - ord 'A') + 10
    | otherwise = Nothing

stateHex :: String -> Integer -> [Token]
stateHex  "" acc = [TNumber acc]
stateHex (c:rest) acc =
    case n of
        Just n -> stateHex rest (acc * 16 + n)
        Nothing -> TNumber acc : getTokens (c:rest)
    where n = checkHex c

