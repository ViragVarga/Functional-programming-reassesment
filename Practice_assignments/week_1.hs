-- Part 1
data Tree a = Leaf a | Node (Tree a) (Tree a)
    deriving Show

count :: Tree a -> Integer
count (Leaf a) = 0
count (Node l r) = 1 + count l + count r

depth :: Tree a -> Integer
depth (Leaf a) = 1
depth (Node l r) | depth l > depth r = 1 + depth l
                 | depth l < depth r = 1 + depth r
                 | otherwise = 1 + depth l

flatten :: Tree a -> [a]
flatten (Leaf a) = [a]
flatten (Node l r) = flatten l ++ flatten r

a = 5
b = 3
c = 4
d = 1

main = putStrLn (show (flatten (Node (Leaf a) (Node (Node (Leaf b) (Leaf c)) (Leaf d)))))