-- Functions Definition

doubleNum :: Integer -> Integer
-- This is defining the type of the function

doubleNum x = x*2

-- To load this function to ghci use :l filename.hs

canDrink :: Integer -> Bool
canDrink x = if x >= 18 then True else False

sayNum :: Int->String
sayNum x
    | 1<=x && x <= 5 = show x ++ " " ++ "chosen!"
    | otherwise      = "Not in between 1 and 5" 

factorec :: (Integral a) => a -> a
factorec n
    | n == 0    = 1
    | otherwise = n * factorec (n-1) 

revS :: String -> String
revS [] = ""
revS (x:xs) = revS (xs) ++ [x]
