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
 
