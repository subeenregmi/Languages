------------------------- Exercise 1

add :: [Int] -> Int
add []     = 0
add (x:xs) = x + add xs

times :: (Num a) => [a] -> a
times [] = 1
times (x:xs) = x * times xs

range :: (Eq a, Num a) => a -> a -> [a]
range n m = if n==m then [m] else (n : range (n+1) m)

factorial :: Int -> Int
factorial n = times (range 1 n)


------------------------- Exercise 2

count :: [a] -> Int
count []     = 0
count (_:xs) = 1 + count xs

append :: [a] -> [a] -> [a]
append (x:xs) (y:ys) = x : append (xs) (y:ys)
append [] (y:ys) = y : append [] ys
append [] [] = []

concatenate :: [[a]] -> [a]
concatenate [] = []
concatenate (x:xs) = x ++ concatenate xs


------------------------- Exercise 3

member :: Int -> [Int] -> Bool
--member y [] = False
--member y (x:xs)
--    | y == x    = True
--    | otherwise = member y xs
member _ [] = False
member y (x:xs) = (y == x) || member y xs

remove :: (Eq a) => a -> [a] -> [a]
remove _ [] = []
remove y (x:xs)
    | y==x      = remove y xs
    | otherwise = x : remove y xs

at :: [a] -> Int -> a
at [] y = error "Index too large"
at (x:xs) y
    | y < 0     = error "Negative Index"
    | y == 0    = x
    | otherwise = at xs (y-1)

------------------------- Exercise 4

final :: [a] -> a
final (x:[]) = x
final (x:xs) = final xs

ordered :: [Int] -> Bool
ordered [] = True
ordered [x] = True
ordered (x:y:xs) = (x < y) && ordered xs

pair :: [a] -> [b] -> [(a,b)]
pair [] _     = []
pair _ []     = []
pair (x:xs) (y:ys) = (x, y) : pair xs ys

find :: Int -> [(Int,String)] -> String
find x [] = ""
find x (y:ys)
    | x == fst y = snd y
    | otherwise  = find x ys 


------------------------- Exercise 5


------------------------- Exercise 6

merge :: (Eq a, Ord a) => [a] -> [a] -> [a]
merge [] a = a 
merge a [] = a
merge (x:xs) (y:ys)
    | x == y = x : merge xs ys
    | x < y  = x : merge xs (y:ys)
    | x > y  = y : merge (x:xs) ys

msort :: (Eq a, Ord a) => [a] -> [a]
msort a
    | length a == 1 = a
    | otherwise     = merge (msort (take n a)) (msort (drop n a))
  where n = (length a) `div` 2
