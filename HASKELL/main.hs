-- Functions Definition

doubleNum :: Integer -> Integer
-- This is defining the type of the function

doubleNum x = x*2

-- To load this function to ghci use :l filename.hs

canDrink :: Integer -> Boo
canDrink x = if x >= 18 then True else False
