
data IntTree = Empty | Node Int IntTree IntTree
  deriving Show

t :: IntTree
t = Node 4 (Node 2 (Node 1 Empty Empty) (Node 3 Empty Empty)) (Node 5 Empty (Node 6 Empty Empty))


------------------------- Exercise 1

isEmpty :: IntTree -> Bool
isEmpty Empty = undefined
isEmpty _     = undefined

rootValue :: IntTree -> Int
rootValue Empty        = undefined
rootValue (Node i _ _) = undefined

height :: IntTree -> Int
height = undefined

member :: Int -> IntTree -> Bool
member = undefined

-------------------------

{-
instance Show IntTree where
    show = unlines . aux ' ' ' '
      where
        aux _ _ Empty = []
        aux c d (Node x s t) = 
          [ c:' ':m | m <- aux ' ' '|' s ] ++ 
          ['+':'-':show x] ++ 
          [ d:' ':n | n <- aux '|' ' ' t ]
-}


insert :: Int -> IntTree -> IntTree
insert x Empty = Node x Empty Empty
insert x (Node y left right)
    | x == y    = Node y left right
    | x <  y    = Node y (insert x left) right
    | otherwise = Node y left (insert x right)

build :: [Int] -> IntTree
build = foldr insert Empty

flatten :: IntTree -> [Int]
flatten Empty        = []
flatten (Node x l r) = flatten l ++ [x] ++ flatten r

treemap :: (Int -> Int) -> IntTree -> IntTree
treemap f Empty        = Empty
treemap f (Node x l r) = Node (f x) (treemap f l) (treemap f r)

------------------------- Exercise 2

present :: Int -> IntTree -> Bool
present = undefined   

largest :: IntTree -> Int
largest Empty            = undefined
largest (Node x l Empty) = undefined
largest (Node x l r)     = undefined

ordered :: IntTree -> Bool
ordered = undefined

deleteLargest :: IntTree -> IntTree
deleteLargest = undefined

delete :: Int -> IntTree -> IntTree
delete _ Empty = undefined
delete y (Node x l r)
    | y < x     = undefined
    | y > x     = undefined
    | isEmpty l = undefined
    | otherwise = undefined

sorted :: IntTree -> Bool
sorted Empty = undefined
sorted t     = undefined
  where
    inRange :: Int -> Int -> IntTree -> Bool
    inRange = undefined

------------------------- Exercise 3

{-

instance Show a => Show (Tree a) where
    show = unlines . aux ' ' ' '
      where
        aux _ _ Empty = []
        aux c d (Node x s t) = 
          [ c:' ':m | m <- aux ' ' '|' s ] ++ 
          ['+':'-':show x] ++ 
          [ d:' ':n | n <- aux '|' ' ' t ]


type IntTree = Tree Int

-}


------------------------- Exercise 4

data Balance = Negative | Neutral | Positive

data AVLTree a = Leaf | AVLNode Balance a (AVLTree a) (AVLTree a)

{-
instance Show a => Show (AVLTree a) where
  show = show . forget

forget :: AVLTree a -> Tree a
forget Leaf              = Empty
forget (AVLNode _ x l r) = Node x (forget l) (forget r)
-}


insertAVL :: Ord a => a -> AVLTree a -> (Bool,AVLTree a)
insertAVL x Leaf = undefined
insertAVL x (AVLNode b y l r)
  | x <  y , growLeft  = balanceLeft  b y newLeft r
  | x  > y , growRight = balanceRight b y l newRight
  | x == y             = (False,AVLNode b y l r)
  | x <  y             = undefined
  | otherwise          = undefined
  where
    (growLeft, newLeft)  = insertAVL x l
    (growRight,newRight) = insertAVL x r

    balanceLeft  = undefined
    -- 6 cases

    balanceRight Negative x l r = undefined
    balanceRight Neutral  x l r = undefined
    balanceRight Positive x a (AVLNode Positive y b c) = (False, AVLNode Neutral y (AVLNode Neutral x a b) c)
    -- 3 more cases

