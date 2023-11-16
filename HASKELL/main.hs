-- Functions Definition

doubleNum :: Integer -> Integer
-- This is defining the type of the function

doubleNum x = x*2

-- To load this function to ghci use :l filename.hs

canDrink :: Integer -> Bool
canDrink x = if x >= 18 then True else False

sayNum :: Int->String
sayNum x
    | lowerL<=x && x <= upperL = show x ++ " " ++ "chosen!"
    | otherwise      = "Not in between " ++ show lowerL ++ " and " ++ show upperL 
    where upperL = 2
          lowerL = 1

factorec :: (Integral a) => a -> a
factorec n
    | n == 0    = 1
    | otherwise = n * factorec (n-1) 

revS :: String -> String
revS [] = ""
revS (x:xs) = revS (xs) ++ [x]

len :: (Num a) => [b] -> a 
len []     = 0 
len (_:xs) = 1 + len(xs)

sayFive :: (Integral a) => a -> String
sayFive x
    | x == 5 = "Five"
    | x == 4 || x == 3 = "Close.."
    | otherwise = "Not close"

findFib :: Integer -> Integer
findFib n = case n of 0 -> 0
                      1 -> 1
                      n -> findFib (n-1) + findFib (n-2)
               
fibNums :: [Integer]
fibNums = [findFib x | x <- [0..]]
 
replicate' :: (Integral a) => a -> a -> [a]
replicate' 1 b = [b]
replicate' a b = b : replicate' (a-1) b 

repTen :: (Integral a) => a -> [a]
repTen = replicate' 10

take' :: (Integral a) => a -> [b] -> [b]
take' 1 (x:_) = [x]
take' a (x:xs) = x : take' (a-1) xs

collatzChain :: (Ord a, Integral a) => a -> [a]
collatzChain 1 = [1]
collatzChain n
    | n `mod` 2 == 0 = n : collatzChain(n `div` 2)
    | otherwise  = n : collatzChain(3*n + 1)

flip' :: (a -> b -> c) -> (b-> a -> c)
flip' f x y = f y x 

folder :: (Num a) => (a -> a -> a) -> a -> [a] -> a
folder f acc (x:xs) = folder (f) (f acc x) (xs)  
folder f acc [] = acc 

mapper :: (a -> b) -> [a] -> [b]
mapper _ [] = []
mapper f (x:xs) = f x : mapper f xs

-- Practice 1
rev :: [a] -> [a]
rev l = foldr (:) [] l

-- Practice 2
prefixes :: [a] -> [[a]]
prefixes = foldl (\x y -> if null x then [[y]] else x ++ [last x ++ [y]]) [] 

data Time = Date Int Int Int deriving (Show)

printTime :: Time -> String
printTime (Date x y z) = "The day is " ++ show x ++ ". The month is " ++ show y ++ ". The year is " ++ show z ++ "."

data Tree = Empty | Node Int (Tree) (Tree) deriving (Show)

addToTree :: Int -> Tree -> Tree
addToTree n Empty = Node n Empty Empty
addToTree n (Node x a b)
    | n > x = Node x a (addToTree n b)
    | n < x = Node x (addToTree n a) b
    | otherwise = (Node x a b)

listToTree :: [Int] -> Tree
listToTree x = foldr addToTree Empty (reverse x)
