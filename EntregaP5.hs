import Prelude hiding (take)

data BTree a = Empty | Node Int (BTree a) a (BTree a)
                deriving Show

-- Función de "paralelización"
(|||) :: a -> b -> (a,b)
x ||| y = (x,y)

-- Devuelve el tamaño de un BTree
size :: BTree a -> Int
size Empty = 0
size (Node r _ _ _) = r

-- Agrega un elemento al BTree
cons :: a -> BTree a -> BTree a
cons x Empty = Node 1 Empty x Empty
cons x (Node k l y r) = Node (k+1) (cons x l) y r

-- Crea un arbol de N elementos (desde el 0) y aplica f a sus elementos
tabulate :: (Int -> a) -> Int -> BTree a
tabulate _ 0 = Empty
tabulate f n = let m = div n 2 
                   ((l, x), r) = tabulate f m ||| f m ||| tabulate (f.(+(m+1))) (n-m-1)
                   in Node n l x r

-- Devuelve los primeros N elementos de BTree (N es un cardinal)
take :: Int -> BTree a -> BTree a
take 0 _ = Empty
take _ Empty = Empty
take n h@(Node k l x r) | n >= k = h
                        | n == z = Node n l x Empty
                        | n < z  = take n l
                        | True   = Node n l x (take (n-z) r)
                            where z = (size l) + 1
