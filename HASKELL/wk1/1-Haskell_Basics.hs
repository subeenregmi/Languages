------------------------- Exercise 1
square :: Int -> Int
square x = x*x 

pythagoras :: Int -> Int -> Int -> Bool
pythagoras a b c = if (square a) + (square b) == (square c) then True else False


------------------------- Exercise 2

factorial :: Int -> Int
factorial x
    | x <= 1    = 1
    | otherwise = x * factorial (x-1)

power :: Int -> Int -> Int
--power x y 
--    | y < 0     = error "Negative exponent"
--    | y == 0    = 1
--    | otherwise = x * power x (y-1)

power x y 
    | y < 0  = error "Negative exponent"
    | y == 0 = 1 
    | even y = power (x*x) (y `div` 2)
    | odd y  = x * power (x*x) ((y-1) `div` 2)

euclid :: Int -> Int -> Int
euclid x y
    | x <= 0 || y <= 0 = error "Zero or negative inputs"
    | x == y           = x
    | x > y            = euclid y (x - y)
    | x < y            = euclid x (y - x)


------------------------- Exercise 3

pow :: Int -> Int -> Int
--pow x y = if y == 0 then 1 else x * pow (x) (y-1)  

pow _ 0 = 1
pow x y = if even y 
          then pow (x*x) (y `div` 2) 
          else x * pow(x*x) ((y-1) `div` 2)


------------------------- Exercise 4

collatz :: Int -> Int
collatz 1 = 1
collatz x = if even x 
            then collatz (x `div` 2)
            else collatz (3*x + 1)

collatzCount :: Int -> Int -> Int
collatzCount 1 c = c
collatzCount n c = if even n
                   then collatzCount (n `div` 2) (c+1)
                   else collatzCount (3*n + 1) (c+1)

collatzMax :: Int -> (Int,Int) -> (Int,Int)
collatzMax 0 (c, s) = (c, s)
collatzMax n (c, s) = if (collatzCount n 0) > s 
                      then collatzMax (n-1) (n, collatzCount n 0) 
                      else collatzMax (n-1) (c, s)
------------------------- Exercise 5

ackermann :: Int -> Int -> Int
ackermann 0 n = n+1
ackermann m 0 = ackermann (m-1) 1 
ackermann m n = ackermann (m-1) (ackermann (m) (n-1))

