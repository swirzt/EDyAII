module Lab02 where

{-
   Laboratorio 2
   EDyAII 2020
-}

-- import Data.List

{-
1) Inferir, de ser posible, los tipos de las siguientes funciones:
(puede suponer que sqrt :: Float → Float)
-}

-- a)
modulus :: Floating a => [a] -> a
modulus = sqrt . sum . map (^2)

-- b)
vmod :: Floating a => [[a]] -> [a]
vmod [] = []
vmod (v : vs) = modulus v : vmod vs


-- 2) Dado el siguiente tipo para representar números binarios:
type NumBin = [Bool]

{- donde el valor False representa el número 0 y True el 1. Definir las siguientes operaciones tomando como convención una representación Little-Endian (i.e. el primer elemento de las lista de dı́gitos es el dı́gito menos significativo del número representado).-}

--a) suma binaria
mas1 :: NumBin -> NumBin
mas1 [] = [True]
mas1 (x:xs) = if x then False : mas1 xs else True : xs

sumaBin :: NumBin -> NumBin -> NumBin
sumaBin [] y = y
sumaBin x [] = x
sumaBin (x:xs) (y:ys) = if x && y then False : mas1 (sumaBin xs ys) else (x || y) : sumaBin xs ys

--b) producto binario
prodBin :: NumBin -> NumBin -> NumBin
prodBin _ [] = [False]
prodBin xs [y] = if y then xs else [False]
prodBin xs (y:ys) = if y then sumaBin xs (False:(prodBin xs ys)) else False : (prodBin xs ys)

-- c) cociente y resto de la división por dos

cociente :: NumBin -> NumBin
cociente (_:[]) = [False]
cociente (_:xs) = xs

modBin :: NumBin -> NumBin
modBin (x:_) = [x]


-- 3) Dado el tipo de datos
data CList a = EmptyCL | CUnit a | Consnoc a (CList a) a

{-a) Implementar las operaciones de este tipo algebraico teniendo en cuenta que:

* Las funciones de acceso son headCL, tailCL, isEmptyCL, isCUnit.
* headCL y tailCL no están definidos para una lista vacı́a.
* headCL toma una CList y devuelve el primer elemento de la misma (el de más a la izquierda).
* tailCL toma una CList y devuelve la misma sin el primer elemento.
* isEmptyCL aplicado a una CList devuelve True si la CList es vacı́a (EmptyCL) o False en caso contrario.
* isCUnit aplicado a una CList devuelve True sii la CList tiene un solo elemento (CUnit a) o False en caso contrario.-}

headCL :: CList a -> a
headCL (CUnit x)       = x
headCL (Consnoc x _ _) = x

tailCL :: CList a -> CList a
tailCL (CUnit _)       = EmptyCL
tailCL (Consnoc _ y z) = case y of
                         EmptyCL -> CUnit z
                         CUnit x -> Consnoc x EmptyCL z
                         otherwise -> Consnoc (headCL y) (tailCL y) z

isEmptyCL :: CList a -> Bool
isEmptyCL EmptyCL = True
isEmptyCL _       = False

isCUnit :: CList a -> Bool
isCUnit (CUnit _) = True
isCUnit _         = False

-- b) Definir una función reverseCL que toma una CList y devuelve su inversa.

reversCL :: CList a -> CList a
reversCL (Consnoc x y z) = Consnoc z (reversCL y) x
reversCL x = x

-- c) Definir una función inits que toma una CList y devuelve una CList con todos los posibles inicios de la CList.

lastCL :: CList a -> a
lastCL (CUnit x) = x
lastCL (Consnoc _ _ z) = z

delLastCL :: CList a -> CList a
delLastCL (CUnit _) = EmptyCL
delLastCL (Consnoc x y z) = case y of
                            EmptyCL -> CUnit x
                            CUnit y' -> Consnoc x EmptyCL y' 
                            otherwise -> Consnoc x (delLastCL y) (lastCL y)

snocCL :: a -> CList a -> CList a
snocCL x EmptyCL = CUnit x
snocCL x (CUnit y) = Consnoc y EmptyCL x
snocCL x (Consnoc y z w) = Consnoc y (snocCL w z) x

inits :: CList a -> CList (CList a)
inits EmptyCL   = CUnit EmptyCL
inits (CUnit x) = Consnoc EmptyCL EmptyCL (CUnit x)
inits (Consnoc x EmptyCL y) = Consnoc EmptyCL (CUnit (CUnit x)) (Consnoc x EmptyCL y)
inits x = snocCL x (inits (delLastCL x))

-- d) Definir una función lasts que toma una CList y devuelve una CList con todas las posibles terminaciones de la CList.

delHeadCL :: CList a -> CList a
delHeadCL (CUnit _) = EmptyCL
delHeadCL (Consnoc x y z) = case y of 
                            EmptyCL -> CUnit z
                            CUnit y' -> Consnoc y' EmptyCL z
                            otherwise -> Consnoc (headCL y) (tailCL y) z

consCL :: a -> CList a -> CList a
consCL x EmptyCL = CUnit x
consCL x (CUnit y) = Consnoc x EmptyCL y
consCL x (Consnoc y z w) = Consnoc x (consCL y z) w

lasts :: CList a -> CList (CList a)
lasts EmptyCL   = CUnit EmptyCL
lasts (CUnit x) = Consnoc (CUnit x) EmptyCL EmptyCL
lasts (Consnoc x EmptyCL y) = Consnoc (Consnoc x EmptyCL y) (CUnit (CUnit y)) EmptyCL
lasts x = consCL x (lasts (delHeadCL x))

-- e) Definir una función concatCL que toma una CList de CList y devuelve la CList con todas ellas concatenadas

biConcatCL :: CList a -> CList a-> CList a
biConcatCL x EmptyCL = x
biConcatCL EmptyCL x = x
biConcatCL (CUnit x) (CUnit y) = Consnoc x EmptyCL y
biConcatCL (CUnit x) (Consnoc y z w) = Consnoc x (biConcatCL (CUnit y) z) w
biConcatCL (Consnoc x y z) (CUnit w) = Consnoc x (biConcatCL y (CUnit z)) w
biConcatCL (Consnoc x1 y1 z1) (Consnoc x2 y2 z2) = Consnoc x1 (biConcatCL (biConcatCL y1 (CUnit z1)) (biConcatCL (CUnit x2) y2)) z2

concatCL :: CList (CList a) -> CList a
concatCL EmptyCL = EmptyCL
concatCL (CUnit x) = x
concatCL (Consnoc x y z) = biConcatCL (biConcatCL x (concatCL y)) z 

-- 4) Dada las siguientes representaciones de árboles generales y de árboles binarios

data GenTree a = EmptyG | NodeG a [GenTree a]

data BinTree a = EmptyB | NodeB (BinTree a) a (BinTree a)

{-defina una función g2bt que dado un árbol nos devuelva un árbol binario de la siguiente manera:
la función g2bt reemplaza cada nodo n del árbol general (NodeG) por un nodo n' del árbol binario (NodeB ), donde
el hijo izquierdo de n' representa el hijo más izquierdo de n, y el hijo derecho de n' representa al hermano derecho
de n, si existiese (observar que de esta forma, el hijo derecho de la raı́z es siempre vacı́o).-}

g2btAux :: GenTree a -> [GenTree a] -> BinTree a
g2btAux EmptyG _                = EmptyB
g2btAux (NodeG x []) []         = NodeB EmptyB x EmptyB
g2btAux (NodeG x []) (z:zs)     = NodeB EmptyB x (g2btAux z zs)
g2btAux (NodeG x (y:ys)) []     = NodeB (g2btAux y ys) x EmptyB
g2btAux (NodeG x (y:ys)) (z:zs) = NodeB (g2btAux y ys) x (g2btAux z zs)

g2bt :: GenTree a -> BinTree a
g2bt x = g2btAux x []

-- 5) Definir las siguientes funciones sobre árboles binarios de búsqueda (bst):

data BST a = Hoja | Nodo (BST a) a (BST a)
--a) maximum, que calcula el máximo valor en un bst.

maximumB :: Ord a => BST a -> a
maximumB (Nodo _ x Hoja) = x
maximumB (Nodo _ _ t)      = maximumB t

--b) checkBST, que chequea si un árbol binario es un bst.

minimumB :: Ord a => BST a -> a
minimumB (Nodo Hoja x _) = x
minimumB (Nodo t _ _)      = minimumB t

checkBST :: Ord a => BST a -> Bool
checkBST Hoja               = True
checkBST (Nodo Hoja _ Hoja) = True
checkBST (Nodo i x Hoja)    = let k = maximumB i in (k <= x) && checkBST i
checkBST (Nodo Hoja x d)    = let k = minimumB d in (k > x) && checkBST d
checkBST (Nodo i x d)       = let k = maximumB i in
                              let j = minimumB d in
                              (k <= x) && checkBST i && (j > x) && checkBST d