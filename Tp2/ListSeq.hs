import Par

emptyS :: [a]
emptyS = []

singletonS :: a -> [a]
singletonS x = [x]

lenghtS :: [a] -> Int
lenghtS [] = 0
lenghtS (_:xs) = 1 + lenghtS xs

nthS :: [a] -> Int -> a
nthS (x:_) 0 = x
nthS (_:xs) n = nthS xs (n-1)

tabulateS  :: (Int -> a) -> Int -> [a]
tabulateS _ 0 = []
tabulateS f k = tabulateS' f 0 (k-1)

tabulateS' :: (Int -> a) -> Int -> Int -> [a]
tabulateS' f x k = if x == k then [f x] else let (y,ys) = f x ||| (tabulateS' f (x+1) k) in y:ys

mapS :: (a -> b) -> [a] -> [b]
mapS f [] = []
mapS f (x:xs) = let (y,ys) = f x ||| mapS f xs in y : ys

filterS :: (a -> Bool) -> [a] -> [a]
filterS _ [] = []
filterS f (x:xs) = let (y,ys) = f x ||| filterS f xs
                        in if y then x : ys
                                else ys

-- ¿Cual es la profundidad esperada?
appendS :: [a] -> [a] -> [a]
appendS [] y = y
appendS (x:xs) y = x : appendS xs y


takeS :: [a] -> Int -> [a]
takeS _ 0 = []
takeS [] _ = []
takeS (x:xs) n = x : takeS xs (n-1)

dropS :: [a] -> Int -> [a]
dropS x 0 = x
dropS [] _ = []
dropS (_:xs) n = dropS xs (n-1)

showtS :: [a] -> TreeView a [a]
showtS [] = EMPTY
showtS [x] = ELT x
showtS xs = let n = lenghtS xs
                m = div n 2
                (xss,yss) = takeS xs m ||| dropS xs m
                in NODE xss yss


showlS :: [a] -> ListView a ([a])
showlS [] = NIL
showlS (x:xs) = CONS x xs 

-- ¿Cual deberia ser el span de la funcion?
joinS :: [[a]] -> [a]
joinS [] = []
joinS [x] = x
joinS (x:y:xs) = let (xss,yss) = appendS x y ||| joinS xs in appendS xss yss

reduceS    :: (a -> a -> a) -> a -> [a] -> a
reduceS f e xs = let t = showtS xs in case t of 
                                      EMPTY     -> e
                                      ELT x     -> x
                                      NODE l r -> f (reduceS f e l) (reduceS f e r)

-- + [1,2,3,4,5,6,7] = (((1+2) + (3+4)) + (5+6)) + 7 <=> ((1+2) + (3+4)) + (5 + (6+7))         
data TreeView a t = EMPTY | ELT a | NODE t t
            deriving Show
data ListView a t = NIL | CONS a t
            deriving Show
-- + [1,2,3,4] = + [1+2,3+4] = + [(1+2)+(3+4)] = (1+2) + (3+4)
-- Fijate que onda el scan, deberia ser como el reduce pero con bigote. Esta en las diapositivas
-- Trucazo: Hay una relacion entre la asociatividad del scan y del reduce salite de aca, va a quedar lineal y el maximo de un logaritmo


fromList :: [a] -> [a]
fromList x = x