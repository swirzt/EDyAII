data Tree a = E | L a | N Int (Tree a) (Tree a) deriving (Show,Eq)

fromList :: [a] -> Tree a
fromList [] = E
fromList [x] = L x
fromList xs = let n = length xs 
                  k = div n 2
                  in merge (fromList (take k xs)) (fromList (drop k xs)) 

(|||) :: a -> b -> (a,b)
x ||| y = (x,y)

toList :: Tree a -> [a]
toList E = []
toList (L x) = [x]
toList (N _ x y) = toList x ++ toList y

concatT :: Tree (Tree a) -> Tree a
concatT E = E
concatT (L x) = x
concatT (N _ xs ys) = let (xs',ys') = concatT xs ||| concatT ys in
                                    merge xs' ys'

merge :: Tree a -> Tree a -> Tree a
merge E x = x
merge x E = x
merge x y = N (size x + size y) x y

size :: Tree a -> Int
size E = 0
size (L _) = 1
size (N r _ _) = r

-- --supongo que el indice es valido
-- takeT :: Tree a -> Int -> Tree a
-- takeT _ 0 = E
-- takeT (L x) 1 = L x
-- takeT (N s l r) n | n1 == n = l
--                      | n1 < n = Node (s-n1) l (takeT r (n - n1))
--                      | otherwise = takeT l n
--                      where n1 = size l

-- --supongo que el indice es valido
-- dropT :: Tree a -> Int -> Tree a
-- dropT t 0 = t
-- dropT (L x) 1 = E
-- dropT (N s l r) n | n1 == n = r
--                      | n1 < n = dropT r (n - n1)
--                      | otherwise = Node (s-n) (dropT l n) r
--                      where n1 = size l
                         

-- stripSuffix :: Tree a -> Tree a -> Maybe (Tree a)
-- stripSuffix s t@(N n l r) | n < ls = Nothing
--                           | n == ls = me fijo si son igual
--                           | otherwise = size r

