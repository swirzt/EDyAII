--1)
test :: (Int -> Int) -> Int -> Bool
esMenor :: Int -> Int -> Bool
eq :: Int -> Int -> Bool
showVal :: Int -> String


--2)
(+5) :: Int -> Int
(0<) :: Int -> Bool
('a':) :: [Char] -> [Char]
(++'\n') :: String -> String
filter (==7) :: [Int] -> [Int]
map (++[1]) :: [Int] -> [Int]


{- 3)
a)
f :: (Int -> Int) -> Int
EJ1 f g = 5
EJ2 f h = 6
b)
f :: Int -> (Int -> Int)
EJ1 f x = (+x)
EJ2 f y = (^y)
c)
f :: (Int -> Int) -> (Int -> Int)
EJ1 f g = g
EJ2 f h = k
where g,h,k funciones (Int -> Int)
d)
f :: Int -> Bool
EJ1 f = positive
EJ2 f = par
par x = mod x 2 == 0
e)
f :: Bool -> (Bool -> Bool)
EJ1 f x = (==x)
EJ2 f x = not
f)
f :: (Int, Char) -> Bool
EJ1 f (x,y) = chr x == y
EJ2 f (x,y) = True
g)
f :: (Int, Int) -> Int
EJ1 f (x,_) = x
EJ2 f (x,y) = x+y
h)
f :: Int -> (Int, Int)
EJ1 f x = (x,x)
EJ2 f y = (0,y)
i)
f :: a -> Bool
EJ1 f _ = False
EJ2 f y = y == y
j)
f :: a -> a
EJ1 f x = x
EJ2 f y = show a
-}

{- 4)
a) Correcto, el resultado es True
b) Error sintactico el segundo If no tiene nada que evaluar
c) Correcto, el resultado es False
d) Error sintactico, 1 < 2 devuelve True y luego queda True < 3 Error
e) Correcto, 'a' se representa con un digito menor que 'z', el resultado es 0
f) Incorrecto error de tipo, este if devuelve un Bool o un Int
g) Correcto, Devuelve True
-}

{- 5)
a) f x = x
b) greater (x,y) = x > y
c) f (x,y) = x
-}

-- 6)
--a) 
smallest :: a -> a -> a -> a
smallest = \x y z -> if x < y then if x < z then x else z else if y < z then y else z

--b)
second :: a -> (a -> a) 
second = \_ -> (\x -> x)

--c) 
andThen :: Bool -> Bool -> Bool
andThen = \x y -> if x then y else False

--d)
twice :: (a -> a) -> a -> a
twice = \f x -> f (f x)

--e)
flip :: (a -> a -> a) -> a -> a -> a
flip = \f x y -> f y x

--f)
inc :: Num -> Num
inc = \x -> x+1

--7)
iff :: Bool -> Bool -> Bool
iff x y = if x then not y else y

alpha :: a -> a
alpha x = x

{- 8)
h :: a-> b -> d
h = f o g
h x y = (f o g) x y

o :: (b -> c) -> (a -> b) -> (a -> c)
-}

-- 9)
zip3recu :: [a] -> [a] -> [a] -> [(a,a,a)]
zip3recu (x:xs) (y:ys) (z:zs) = (x,y,z) : zip3recu xs ys zs
zip3recu _ _ _ = []

zip3' :: [a] -> [a] -> [a] -> [(a,a,a)]
zip3' xs ys zs = [(x,y,z) | ((x,y),z) <- zip (zip xs ys) zs]

{- 10)
a) Incorrecto, [[]] ++ xs = [] : xs, xs debe ser una lista
b) Solo vale si xs = []
c) Correcto, para cualquier xs lista
d) No puede ser correcto
e) Correcto para cualquier xs de cualquier tipo
f) Incorrecto, no es posible
g) Incorrecto
h) Correcto para cualquier xs lista
i) Correcto para cualquier xs cualquier tipo
j) Correcto   "      "      "     "       "
-}

-- 11)
--a)
modulus :: [Float] -> Float

--b)
vmod :: [[Float]] -> [Float]

-- 12)
type NumBin = [Bool]

mas1 :: NumBin -> NumBin
mas1 [] = [True]
mas1 (x:xs) = if x then False : mas1 xs else True : xs

suma :: NumBin -> NumBin -> NumBin
suma (x:xs) (y:ys) = if x && y then False : mas1 (suma xs ys) else (x || y) : suma xs ys
suma [] y = y
suma x [] = x

producto :: NumBin -> NumBin -> NumBin
-- Utilizo campesino ruso, a = xs, b = y:ys, k = ys, 2.x = False : x
-- producto _ [] = [False] -- Considero [False] == [] Representan lo mismo
producto xs [y] = if y then xs
                       else [False] -- Si es un solo bit, y puede ser 1 o 0 (devuelvo xs o vacio)
producto xs (y:ys) = if y then suma xs (False:(producto xs ys))
                          else False : (producto xs ys)
-- Numero guardado en little-endian, si y = 1 => b = 2k+1
-- Al sacarle el bit menos significativo a y:ys, lo estoy dividiendo por 2 por ende ys = k

div2 :: NumBin -> NumBin
div2 [] = [False]
div2 (x:xs) = if xs == [] then [False] else xs

rest2 :: NumBin -> NumBin
rest2 [] = [False]
rest2 (x:xs) = [x]

-- 13)
divisors :: Int -> [Int]
divisors x = if x <= 0 then [] else [y | y <- [1..x],mod x y == 0]

matches :: Int -> [Int] -> [Int]
matches x xs = [y | y <- xs, y == x]

sqrtpiso :: Int -> Int
sqrtpiso x = last [y | y <- [1..x], y^2 <= x]

cuadrupla :: Int -> [(Int,Int,Int,Int)]
cuadrupla n = let k = sqrtpiso n in [(a,b,c,d) | a <- [0..k], b <- [0..k],c <- [0..k],d <- [0..k],(a^2)+(b^2)+(c^2)+(d^2) == n]

unique :: [Int] -> [Int]
unique xs = [x | (x,i) <- zip xs [0..], not (elem x (take i xs))]

-- 14)
scalarProduct :: [Int] -> [Int] -> Int
scalarProduct xs ys = sum [x*y | (x,y) <- zip xs ys]