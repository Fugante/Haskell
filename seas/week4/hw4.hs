fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
    | even x = (x - 2) * fun1 xs
    | otherwise = fun1 xs

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
    | even n = n + fun2 (n `div` 2)
    | otherwise = fun2 (3 * n + 1)

fun1' :: [Integer] -> Integer
fun1' = foldr ((*) . subtract 2) 1 . filter even

collatz :: Integer -> Integer
collatz n
    | even n = n `div` 2
    | otherwise = 3 * n + 1

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (>1) . iterate collatz


data Tree a = Leaf
    | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)


-- addNode :: a -> Tree a -> Tree a
-- addNode a Leaf = Node 0 Leaf a Leaf
-- addNode a (Node n Leaf a' l) = Node n (Node (n + 1) Leaf a Leaf) a' l
-- addNode a (Node n l@(Node _ _ _ _) a' Leaf) = Node n l a' (Node (n + 1) Leaf a Leaf)
-- addNode a (Node n r@(Node _ _ _ _) a' l@(Node _ _ _ _)) = Node n addNode(a r) a' l

-- foldTree :: [a] -> Tree a
-- foldTree l = foldr (\x -> ) Leaf

xor :: [Bool] -> Bool
xor = foldr (\a b -> (a || b) && (not (a && b))) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\a b -> f a : b) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (flip f) base $ reverse xs

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x, y) | x <- xs, y <- ys]

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map ((+1).(*2)) $ filter (\n -> notElem n (map sieve numPairs)) [1..n]
    where   numPairs = cartProd [1..n] [1..n]
            sieve t = (fst t) + (snd t) + 2*(fst t)*(snd t)