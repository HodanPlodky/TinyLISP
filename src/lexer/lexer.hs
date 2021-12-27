{-# OPTIONS_GHC -Wall -Wno-name-shadowing -dynamic #-}
module Lexer (Token (..), Keyword(..), getTokens) where
import Data.Char

-- Build in keywords datatype
data Keyword
    = Defun
    | If
    | Eq
    | FCons
    | Null
    | Define
    | Lambda
    | Letrec
    | Car
    | Cdr
    | KwRead
    | KwPrint
    deriving Show

-- check if ident is not keyword
checkIdent :: String -> Token
checkIdent name =
    case name of
        "if" -> TKw If
        "defun" -> TKw Defun
        "eq?" -> TKw Eq
        "cons" -> TKw FCons
        "null" -> TKw Null
        "define" -> TKw Define
        "lambda" -> TKw Lambda
        "letrec" -> TKw Letrec
        "car" -> TKw Car
        "cdr" -> TKw Cdr
        "read" -> TKw KwRead
        "print" -> TKw KwPrint
        n -> TIdent n

-- tokens
data Token
    = TEof
    | TError String
    | TIdent String
    | TKw Keyword
    | TNumber Int
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

-- definition of sum handy set of characters
alfa :: [Char]
alfa = ['a'..'z'] ++ ['A'..'Z']

identChars :: [Char]
identChars = alfa ++ ['_', '?']

nums :: [Char]
nums = ['0'..'9']

whiteSpace :: [Char]
whiteSpace = ['\n', '\r', '\r', ' ']

-- funny helper funtions
-- match is just function witch similar functionality like case 
-- but I found it bit more usefull in some cases then the case
match :: a -> [(a -> Bool, b)] -> Maybe b
match _ [] = Nothing
match item (test:rest) =
    if ((fst test) item)
       then Just $ snd test
       else (match item rest)

elemCh :: (Foldable t, Eq a) => t a -> a -> Bool
elemCh l e = elem e l

-- main function of lexer
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
                    '=' -> TKw Eq
                    _ -> TError "Undefined token") : getTokens rest
            )
            ]
    in
    case toks of
      Just val -> val
      Nothing -> [TError "Cannot lex"]

--
-- helper states in lexer
--


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

-- check validity of number depending on base
checkNum :: Char -> Int -> Maybe Int
checkNum c base =
    let n = ord c - ord '0' 
    in
    if (elem n [0..(base - 1)])
       then Just n
       else Nothing

stateNum :: String -> Int -> Int -> [Token]
stateNum "" acc _ = [TNumber acc]
stateNum (c:rest) acc base =
    case n of
        Just n -> stateNum rest (acc * base + n) base
        Nothing -> TNumber acc : getTokens (c:rest)
    where n = checkNum c base

-- check validity of hex char and parses
checkHex :: Char -> Maybe Int
checkHex c
    | elem c nums = checkNum c 16
    | elem c ['A'..'F'] = Just $ ((ord c - ord 'A') + 10)
    | otherwise = Nothing

stateHex :: String -> Int -> [Token]
stateHex  "" acc = [TNumber acc]
stateHex (c:rest) acc =
    case n of
        Just n -> stateHex rest (acc * 16 + n)
        Nothing -> TNumber acc : getTokens (c:rest)
    where n = checkHex c

