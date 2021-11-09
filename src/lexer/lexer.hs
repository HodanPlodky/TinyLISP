{-# OPTIONS_GHC -Wall -Wno-name-shadowing -dynamic #-}
module Lexer (Token (..) , getTokens) where
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
            (elemCh nums, stateNum (c:rest) 0 10),
            (\_ -> True, [Eof])
            ]
    in
    case toks of
      Just val -> val
      Nothing -> [Eof]

stateIdent :: String -> String -> [Token]
stateIdent "" acc = [Ident acc]
stateIdent (c:rest) acc =
    if (elem c alfa)
       then stateIdent rest (acc ++ [c])
       else Ident acc : getTokens rest

--checkIdent :: String -> Token
--checkIdent name =

stateNum :: String -> Integer -> Integer -> [Token]
stateNum "" acc _ = [Number acc]
stateNum (c:rest) _ acc = [Eof]
