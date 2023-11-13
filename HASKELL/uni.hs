multTwo :: (Num a) => [a] -> [a]
multTwo b = [x*2 | x <- b]

--foldl (+) 0 [1, 2, 3, 4]
-- is equivalent to
-- ((+)((+)((+)((+) 0 1) 2) 3) 4)
-- (f) [[1]] 2
-- ((F)((F)((F)((F) [] 1) 2) 3) 4)

--foldr (+) 0 [1, 2, 3, 4] 
-- is equivalent to
-- ((+) 1 ((+) 2((+) 3 ((+) 4 0))))

prefixes :: [a] -> [[a]]
prefixes = foldl (\x y -> if null x then [[y]] else x ++ [last x ++ [y]]) []
