module Lab01 where

import Data.List
import Data.Char

{-
1) Corregir los siguientes programas de modo que sean aceptados por GHCi.
-}

-- a)
nop b | b         = False
      | otherwise = True

-- b)
dentro []          =  error "empty list"
dentro [x]         =  []
dentro (x:xs)      =  x : dentro xs

-- c)
largo []         =  0
largo (_:xs)     =  1 + largo xs

-- d)
list123 = 1 : (2 : (3 : []))

-- e)

[]     ++! ys = ys
(x:xs) ++! ys = x : xs ++! ys

-- f)
addToTail x xs = head xs :  map (+x) (tail xs)

-- g)
listmin xs = head (sort xs)

-- h) (*)
smap f [] = []
smap f [x] = [f x]
smap f (x:xs) = f x : (smap f xs)

{-
2. Definir las siguientes funciones y determinar su tipo:

a) five, que dado cualquier valor, devuelve 5

b) apply, que toma una función y un valor, y devuelve el resultado de
aplicar la función al valor dado

c) ident, la función identidad

d) first, que toma un par ordenado, y devuelve su primera componente

e) derive, que aproxima la derivada de una función dada en un punto dado

f) sign, la función signo

g) vabs, la función valor absoluto (usando sign y sin usarla)

h) pot, que toma un entero y un número, y devuelve el resultado de
elevar el segundo a la potencia dada por el primero

i) xor, el operador de disyunción exclusiva

j) max3, que toma tres números enteros y devuelve el máximo entre llos

k) swap, que toma un par y devuelve el par con sus componentes invertidas
-}
five :: a -> Int
five _ = 5

apply :: (a -> a) -> a -> a
apply f x = f x

ident :: a -> a
ident x = x

first :: (a,b) -> a
first (x,_) = x

derive :: (Float -> Float) -> Float -> Float -> Float
derive f x h = (f (x + h) - f x) / h

sign :: Float -> Float
sign x | x < 0     = -1
       | x == 0    = 0
       | otherwise = 1

vabss :: Float -> Float
vabss x = sign x * x

vabsns :: Float -> Float
vabsns x | x >= 0    = x
         | otherwise = -x

pot :: Int -> Int -> Int
pot x y = x ^ y

xor :: Bool -> Bool -> Bool
xor x y | x == y    = False
        | otherwise = True

max3 :: Int -> Int -> Int -> Int
max3 x y z = max (max x y) z

swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)

{- 
3) Definir una función que determine si un año es bisiesto o no, de
acuerdo a la siguiente definición:

año bisiesto 1. m. El que tiene un día más que el año común, añadido al mes de febrero. Se repite
cada cuatro años, a excepción del último de cada siglo cuyo número de centenas no sea múltiplo
de cuatro. (Diccionario de la Real Academia Espaola, 22ª ed.)

¿Cuál es el tipo de la función definida?
-}

esBis :: Int -> Bool
esBis x = if (mod x 4) == 0 && (mod x 100) /= 0 then True else if (mod x 400) == 0 then True else False

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

a) 'divisors', que dado un entero positivo 'x' devuelve la
lista de los divisores de 'x' (o la lista vacía si el entero no es positivo)

b) 'matches', que dados un entero 'x' y una lista de enteros descarta
de la lista los elementos distintos a 'x'

c) 'cuadrupla', que dado un entero 'n', devuelve todas las cuadruplas
'(a,b,c,d)' que satisfacen a^2 + b^2 = c^2 + d^2,
donde 0 <= a, b, c, d <= 'n'

(d) 'unique', que dada una lista 'xs' de enteros, devuelve la lista
'xs' sin elementos repetidos
-}

divisors :: Int -> [Int]
divisors x = if x<=0 then [] else [y | y <- [1..x],mod x y == 0]

matches :: Int -> [Int] -> [Int]
matches x xs = [y | y <- xs, y == x]

-- sqrtpiso :: Int -> Int
-- sqrtpiso x = last [y | y <- [1..x], y^2 <= x]

-- cuadrupla :: Int -> [(Int,Int,Int,Int)]
-- cuadrupla n = let k = sqrtpiso n in [(a,b,c,d) | a <- [0..k], b <- [0..k],c <- [0..k],d <- [0..k],(a^2)+(b^2)+(c^2)+(d^2) == n]

cuadrupla :: Int -> [(Int,Int,Int,Int)]
cuadrupla n = [(a,b,c,d) | a <- [0..n], b <- [0..n], c <- [0..n], d <- [0..n],a^2 + b^2 == c^2 + d^2]

unique :: [Int] -> [Int]
unique xs = [x | (x,i) <- zip xs [0..], not (elem x (take i xs))]

{- 
6) El producto escalar de dos listas de enteros de igual longitud
es la suma de los productos de los elementos sucesivos (misma
posición) de ambas listas.  Definir una función 'scalarProduct' que
devuelva el producto escalar de dos listas.

Sugerencia: Usar las funciones 'zip' y 'sum'. -}

scalarProduct :: [Int] -> [Int] -> Int
scalarProduct xs ys = sum [x*y | (x,y) <- zip xs ys]

{-
7) Definir mediante recursión explícita
las siguientes funciones y escribir su tipo más general:

a) 'suma', que suma todos los elementos de una lista de números

b) 'alguno', que devuelve True si algún elemento de una
lista de valores booleanos es True, y False en caso
contrario

c) 'todos', que devuelve True si todos los elementos de
una lista de valores booleanos son True, y False en caso
contrario

d) 'codes', que dada una lista de caracteres, devuelve la
lista de sus ordinales

e) 'restos', que calcula la lista de los restos de la
división de los elementos de una lista de números dada por otro
número dado

f) 'cuadrados', que dada una lista de números, devuelva la
lista de sus cuadrados

g) 'longitudes', que dada una lista de listas, devuelve la
lista de sus longitudes

h) 'orden', que dada una lista de pares de números, devuelve
la lista de aquellos pares en los que la primera componente es
menor que el triple de la segunda

i) 'pares', que dada una lista de enteros, devuelve la lista
de los elementos pares

j) 'letras', que dada una lista de caracteres, devuelve la
lista de aquellos que son letras (minúsculas o mayúsculas)

k) 'masDe', que dada una lista de listas 'xss' y un
número 'n', devuelve la lista de aquellas listas de 'xss'
con longitud mayor que 'n' -}

suma :: [Int] -> Int
suma [] = 0
suma (x:xs) = x + suma xs

alguno :: [Bool] -> Bool
alguno [] = False
alguno (x:xs) = x || alguno xs

todos :: [Bool] -> Bool
todos [] = False
todos [x] = x
todos (x:xs) = x && todos xs

codes :: [Char] -> [Int]
codes [] = []
codes (x:xs) = 0 : [y+1 | y <- codes xs]

restos :: [Int] -> Int -> [Int]
restos [] _ = []
restos (x:xs) y = mod x y : restos xs y

cuadrados :: [Int] -> [Int]
cuadrados [] = []
cuadrados (x:xs) = x^2 : cuadrados xs

longitudes :: [[a]] -> [Int]
longitudes [] = []
longitudes (x:xs) = length x : longitudes xs

orden :: [(Int,Int)] -> [(Int,Int)]
orden [] = []
orden ((x,y):xs) = if x >= (3*y) then orden xs else (x,y) : orden xs

pares :: [Int] -> [Int]
pares [] = []
pares (x:xs) = if mod x 2 == 0 then x : pares xs else pares xs

letras :: [Char] -> [Char]
letras [] = []
letras (x:xs) = if isDigit(x) then letras xs else x : letras xs

masDe :: [[a]] -> Int -> [[a]]
masDe [] _ = []
masDe (x:xs) y = if length xs > y then x : masDe xs y else masDe xs y