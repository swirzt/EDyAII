import Prelude hiding (map,take,drop)


--1)

data BTree a = Empty | Node Int (BTree a) a (BTree a)
                deriving Show

-- Devuelve el tamaño de un BTree
size :: BTree a -> Int
size Empty = 0
size (Node r _ _ _) = r

-- Devuelve el n-esimo por indice (comienza en 0)
nth :: BTree a -> Int -> a
nth (Node _ Empty x _) 0 = x
nth (Node _ l _ _) 0 = nth l 0
nth (Node _ l x r) n | n == z = x
                     | n > z = nth r (n-z-1)
                     | True = nth l n
                        where z = size l

-- Agrega un elemento al BTree
cons :: a -> BTree a -> BTree a
cons x Empty = Node 1 Empty x Empty
cons x (Node k l y r) = Node (k+1) (cons x l) y r

-- Crea un arbol de N elementos (desde el 0) y aplica f a sus elementos
tabulate :: (Int -> a) -> Int -> BTree a
tabulate _ 0 = Empty
tabulate f 1 = Node 1 Empty (f 0) Empty
tabulate f n = let k = div n 2 
                   m = k + 1 
                   ((l, x), r) = tabulate f k ||| f k ||| tabulate (f.(+m)) (n-m)
                   in Node n l x r

-- Aplica una funcion f a todos los elementos de BTree
map :: (a -> b) -> BTree a -> BTree b
map _ Empty = Empty
map f (Node k l x r) = let ((l', x'), r') = map f l ||| f x ||| map f r
    in Node k l' x' r'

-- Devuelve los primeros N elementos de BTree (N es un cardinal)
take :: Int -> BTree a -> BTree a
take 0 _ = Empty
take n h@(Node k l x r) | n >= k = h
                        | n == z = Node n l x Empty
                        | n < z  = take n l
                        | True   = Node n l x (take (n-z) r)
                            where z = (size l) + 1

-- Quita los primeros N elementos de BTree (N es cardinal)
drop :: Int -> BTree a -> BTree a
drop 0 x = x
drop n (Node k l x r) | n >= k = Empty
                      | n == z = r
                      | n < z  = Node (k-n) (drop n l) x r
                      | True   = drop (n-z) r
                            where z = (size l) + 1


-- 2)
data Tree a = E | Leaf a | Join (Tree a) (Tree a)
            deriving Show

(|||) :: a -> b -> (a,b)
x ||| y = (x,y)

mapreduce :: (a -> b) -> (b -> b -> b) -> b -> Tree a -> b
mapreduce f g e = mr
    where mr E          = e
          mr (Leaf a)   = f a
          mr (Join l r) = let (l',r') = mr l ||| mr r in g l' r'

mcss :: (Num a, Ord a) => Tree a -> a
mcss = (\(m,_,_,_) -> m) . mapreduce f g e
    where
        f n = let k = max n 0 in (k, k, k, n)
        g (m1, p1, s1, t1) (m2, p2, s2, t2) =
            (max (max m1 m2) (s1 + p2), max p1 (t1 + p2), max s2 (t2 + s1), t1 + t2)
        e = (0, 0, 0, 0)

-- 3)
-- Se por hecho que no existe un (Join E E)
sufijos :: Tree Int -> Tree (Tree Int)
sufijos t = sufijos' t E where
  sufijos' (Leaf _)   a = Leaf a
  sufijos' (Join l r) a = let (l', r') = case a of
                                         E -> (sufijos' l r, sufijos' r E)
                                         _ -> sufijos' l (Join r a) ||| sufijos' r a
                          in Join l' r'