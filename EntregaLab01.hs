module Lab01 where

import Data.List

{-
2. Definir las siguientes funciones y determinar su tipo:

b) apply, que toma una función y un valor, y devuelve el resultado
   de aplicar la función al valor dado

i) xor, el operador de disyunción exclusiva

j) max3, que toma tres números enteros y devuelve
   el máximo entre ellos
-}

apply :: (a -> a) -> a -> a
apply f x = f x

xor :: Bool -> Bool -> Bool
xor = (/=)

max3 :: Int -> Int -> Int -> Int
max3 x y z = max (max x y) z

{-
4)
Defina un operador infijo *$ que implemente la multiplicación de un
vector por un escalar. Representaremos a los vectores mediante listas
de Haskell. Así, dada una lista ns y un número n, el valor ns *$ n
debe ser igual a la lista ns con todos sus elementos multiplicados por
n. Por ejemplo,

[ 2, 3 ] *$ 5 == [ 10 , 15 ].

El operador *$ debe definirse de manera que la siguiente
expresión sea válida:
-}

(*$) :: [Int] -> Int -> [Int]
ns *$ n = [x * n | x <- ns]

v = [1, 2, 3] *$ 2 *$ 4

{-
5) Definir las siguientes funciones usando listas por comprensión:

c) 'cuadrupla', que dado un entero 'n', devuelve todas las cuadruplas
'(a,b,c,d)' que satisfacen a^2 + b^2 = c^2 + d^2,
donde 0 <= a, b, c, d <= 'n'
-}

cuadrupla :: Int -> [(Int,Int,Int,Int)]
cuadrupla n = [(a,b,c,d) | a <- [0..n], b <- [0..n], c <- [0..n], d <- [0..n], a^2 + b^2 == c^2 + d^2]

{-
6) El producto escalar de dos listas de enteros de igual longitud
es la suma de los productos de los elementos sucesivos (misma
posición) de ambas listas.  Definir una función 'scalarProduct' que
devuelva el producto escalar de dos listas.

Sugerencia: Usar las funciones 'zip' y 'sum'. -}

scalarProduct :: [Int] -> [Int] -> Int
scalarProduct xs ys = sum [x*y | (x,y) <- zip xs ys]

{-
7) Definir mediante *recursión explícita*
las siguientes funciones y escribir su tipo más general:

b) 'alguno', que devuelve True si algún elemento de una
lista de valores booleanos es True, y False en caso
contrario

k) 'masDe', que dada una lista de listas 'xss' y un
número 'n', devuelve la lista de aquellas listas de 'xss'
con longitud mayor que 'n' -}

alguno :: [Bool] -> Bool
alguno [] = False
alguno (x:xs) = x || alguno xs

masDe :: [[a]] -> Int -> [[a]]
masDe [] _ = []
masDe (xs:xss) n | length xs > n = xs : masDe xss n
                 | otherwise     = masDe xss n
