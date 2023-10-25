------------------------- Exercise 1

doubles :: [Int] -> [Int]
--recursion
{-
doubles [] = []
doubles (x:xs) = x*2 : doubles xs
-}
--map and filter 
doubles a = map (*2) a

--recursion
{-
multiplesOfThree :: [Int] -> [Int]
multiplesOfThree [] = []
multiplesOfThree (x:xs)
    | isDivisibleByThree x = multiplesOfThree xs
    | otherwise            = x : multiplesOfThree xs
 where 
  isDivisibleByThree :: Int -> Bool
  isDivisibleByThree x = x `mod` 3 == 0
-}
--map and filter
multiplesOfThree :: [Int] -> [Int]
multiplesOfThree x = filter  (isNotDivisibleByThree) x
 where
  isNotDivisibleByThree x = x `mod` 3 /= 0

--recursion
{-
doubleMultiplesOfThree :: [Int] -> [Int]
doubleMultiplesOfThree [] = []
doubleMultiplesOfThree (x:xs)
    | x `mod` 3 == 0 = x*2 : doubleMultiplesOfThree xs
    | otherwise      = doubleMultiplesOfThree xs
-}
--map and filter
doubleMultiplesOfThree :: [Int] -> [Int]
doubleMultiplesOfThree x = map (*2) (filter (isNotDivisibleByThree) x)
 where
  isNotDivisibleByThree :: Int -> Bool
  isNotDivisibleByThree x = x `mod` 3 == 0
------------------------- Exercise 2

shorts :: [String] -> [String]
shorts x = filter (lengthLessThanORFour) x
 where
  lengthLessThanORFour :: String -> Bool
  lengthLessThanORFour a = (length a) < 4

incrementPositives :: [Int] -> [Int]
incrementPositives x = map (+1) (filter (>0) x)

difference :: String -> String -> String
difference [] _ = []
difference a [] = a
difference a (x:xs) = difference (filter (/= x) a) (xs)

oddLengthSums :: [[Int]] -> [Int]
oddLengthSums x = map (sum) (filter (oddL) x)
 where
  oddL :: [Int] -> Bool
  oddL x = ((length x) `mod` 2) /= 0


------------------------- Exercise 3


numbered :: [a] -> [(Int,a)]
numbered x = zip [1..] x 

everyother :: [a] -> [a]
everyother x = map (snd) (filter (oddFst) (numbered x))
 where 
  oddFst (a, b) = odd a

same :: Eq a => [a] -> [a] -> [Int]
same a b = map (fst) (filter (isSame) (numbered (zip a b)))
 where
  isSame (_, (a, b)) = a == b
