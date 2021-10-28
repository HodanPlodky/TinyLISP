import Data.List
import System.IO

maxInt = maxBound :: Int

summing :: Integer -> Integer
summing n = sum [1..n]

myPrepend l x = x : l

squaresTo n = [x * x | x <- [1..n], x * x <= n]

divisibleBy to a b = [x | x <- [1..to], mod x a == 0, mod x b == 0]

addLists l1 l2 = zipWith (+) l1 l2

mulTable a b = [[x * y | y <- [1..b]] | x <- [1..a]]

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

getGrade :: Int -> String
getGrade score
    | score >= 90 = "A"
    | score >= 80 = "B"
    | otherwise = "F"

mySum :: [Int] -> Int
mySum [] = 0
mySum (x:xs) = x + mySum xs

myStrEq :: [Char] -> [Char] -> Bool
myStrEq [] [] = True
myStrEq (x:xs) (y:ys) = x == y && myStrEq xs ys
myStrEq _ _ = False

doubleEvens n =
    if odd n 
        then n
        else  n * 2

-- Enumarating Types
data Tokens = Ident
            | Number Int

data Shape = Circ Float Float Float
           | Rect Float Float Float Float
           deriving Show

area :: Shape -> Float
area (Circ _ _ r) = pi * r ^ 2
area (Rect x1 y1 x2 y2) = abs (x1 - x2) * abs (y1 - y2)

-- holy fuck
fib = 0 : 1 : [a + b | (a, b) <- zip fib (tail fib)]

writeToFile = do
    file <- openFile "test.txt" WriteMode
    hPutStrLn file "Hi"
    hClose file

readFromFile = do
    file <- openFile "test.txt" ReadMode
    contents <- hGetContents file
    putStr contents
    hClose file

main :: IO ()
main = do
    putStrLn "Enter name : "
    name <- getLine
    putStrLn ("Hello " ++ name)
