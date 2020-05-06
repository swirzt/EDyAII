-- 1)
type Colour = (Int,Int,Int)

mezclar :: Colour -> Colour -> Colour
mezclar (x1,y1,z1) (x2,y2,z2) = ((x1+x2) `div` 2,(y1+y2) `div` 2,(z1+z2) `div` 2)

-- 2)
type Linea = ([Char],Int)

vacia :: Linea
vacia = ([],0)

moverIzq :: Linea -> Linea
moverIzq lin@(x,0) = lin
moverIzq (x,n) = (x,n-1)

moverDer :: Linea -> Linea
moverDer lin@(x,n) = if n == length x then lin else (x,n+1)

moverIni :: Linea -> Linea
moverIni (x,_) = (x,0)

moverFin :: Linea -> Linea
moverFin (x,_) = (x,length x)

insertar :: Char -> Linea -> Linea
insertar x (lin,0) = (x:lin,1)
insertar x (c:lin,n) = let (y,z) = insertar x (lin,n-1) in (c:y,z+1)

borrar :: Linea -> Linea
borrar lin@(x,0) = lin
borrar (x:xs,1) = (xs,0)
borrar (x:xs,n) = let (y,z) = borrar (xs,n-1) in (x:y,z+1)

-- 3)
data CList a = EmptyCL | CUnit a | Consnoc a (CList a) a
               deriving Show

consCL :: a -> CList a -> CList a
consCL x EmptyCL = CUnit x
consCL x (CUnit y) = Consnoc x EmptyCL y
consCL x (Consnoc y z w) = Consnoc x (consCL y z) w

snocCL :: a -> CList a -> CList a
snocCL x EmptyCL = CUnit x
snocCL x (CUnit y) = Consnoc y EmptyCL x
snocCL x (Consnoc y z w) = Consnoc y (snocCL w z) x

lastCL :: CList a -> a
lastCL (CUnit x) = x
lastCL (Consnoc _ _ z) = z

delLastCL :: CList a -> CList a
delLastCL (CUnit _) = EmptyCL
delLastCL (Consnoc x y z) = case y of
                            EmptyCL -> CUnit x
                            -- CUnit y' -> Consnoc x EmptyCL y' 
                            otherwise -> Consnoc x (delLastCL y) (lastCL y)

delHeadCL :: CList a -> CList a
delHeadCL (CUnit _) = EmptyCL
delHeadCL (Consnoc x y z) = case y of 
                            EmptyCL -> CUnit z
                            CUnit y' -> Consnoc y' EmptyCL z
                            otherwise -> Consnoc (headCL y) (tailCL y) z

isEmptyCL :: CList a -> Bool
isEmptyCL EmptyCL = True
isEmptyCL _       = False

isCUnit :: CList a -> Bool
isCUnit (CUnit _) = True
isCUnit _         = False

headCL :: CList a -> a
headCL (CUnit x)       = x
headCL (Consnoc x _ _) = x

tailCL :: CList a -> CList a
tailCL (CUnit _)       = EmptyCL
tailCL (Consnoc _ y z) = case y of
                         EmptyCL -> CUnit z
                         CUnit x -> Consnoc x EmptyCL z
                         otherwise -> Consnoc (headCL y) (tailCL y) z

reversClist :: CList a -> CList a
reversClist (Consnoc x y z) = Consnoc z (reversClist y) x
reversClist x = x

inits :: CList a -> CList (CList a)
inits EmptyCL   = CUnit EmptyCL
inits x = snocCL x (inits (delLastCL x))

lasts :: CList a -> CList (CList a)
lasts EmptyCL   = CUnit EmptyCL
lasts x = consCL x (lasts (delHeadCL x))

biConcatCL :: CList a -> CList a -> CList a
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

cl2l :: CList a -> [a]
cl2l EmptyCL = []
cl2l (CUnit x) = [x]
cl2l t@(Consnoc x _ _) = x : cl2l (tailCL t)

l2cl :: [a] -> CList a
l2cl [] = EmptyCL
l2cl [x] = CUnit x
l2cl (x:xs) = consCL x (l2cl xs)

-- 4)
data Aexp = Num Int | Prod Aexp Aexp | Div Aexp Aexp

eval :: Aexp -> Int
eval (Num x) = x
eval (Prod x y) = (eval x) * (eval y)
eval (Div x y) = (eval x) `div` (eval y)

seval :: Aexp -> Maybe Int
seval (Num x) = Just x
seval (Prod x y) = case seval x of
                        Just x' -> case seval y of
                                       Just y' -> Just (x' * y')
                                       _       -> Nothing
                        _       -> Nothing
seval (Div x y) = case seval y of
                       Just y' -> if y' == 0 then Nothing
                                  else case seval x of
                                            Just x' -> Just (x' `div` y')
                                            _       -> Nothing
                       _       -> Nothing

-- 5)
data Tree a = Hoja | Nodo (Tree a) a (Tree a)
              deriving Show

completo :: a -> Int -> Tree a
completo _ 0 = Hoja
completo x d = let k = completo x (d-1) in Nodo k x k

balanceado :: a -> Int -> Tree a
balanceado x n  | n == 0 = Hoja
                | n == 1 = Nodo Hoja x Hoja
                | odd n  = let t = balanceado x (div (n-1) 2) 
                            in Nodo t x t
                | True   = let m = div (n-1) 2
                            in Nodo (balanceado x m) x (balanceado x (n-m)) 

-- 6)
data GenTree a = EmptyG | NodeG a [GenTree a]
                 deriving Show
data BinTree a = EmptyB | NodeB (BinTree a) a (BinTree a)
                 deriving Show

g2btAux :: GenTree a -> [GenTree a] -> BinTree a
g2btAux EmptyG _                = EmptyB
g2btAux (NodeG x []) []         = NodeB EmptyB x EmptyB
g2btAux (NodeG x []) (z:zs)     = NodeB EmptyB x (g2btAux z zs)
g2btAux (NodeG x (y:ys)) []     = NodeB (g2btAux y ys) x EmptyB
g2btAux (NodeG x (y:ys)) (z:zs) = NodeB (g2btAux y ys) x (g2btAux z zs)

g2bt :: GenTree a -> BinTree a
g2bt x = g2btAux x []

-- 7)
type BST a = BinTree a
maximumB :: Ord a => BST a -> a
maximumB (NodeB _ x EmptyB) = x
maximumB (NodeB _ _ t)      = maximumB t

minimumB :: Ord a => BST a -> a
minimumB (NodeB EmptyB x _) = x
minimumB (NodeB t _ _) = minimumB t

checkBST :: Ord a => BST a -> Bool
checkBST EmptyB = True
checkBST (NodeB EmptyB _ EmptyB) = True
checkBST (NodeB i x EmptyB)      = let k = maximumB i in (k <= x) && checkBST i
checkBST (NodeB EmptyB x d)      = let k = minimumB d in (k > x) && checkBST d
checkBST (NodeB i x d) = let k = maximumB i in
                            let j = minimumB d in
                                (k <= x) && checkBST i && (j > x) && checkBST d

-- 8)
member :: Ord a => a -> BST a -> Bool
member _ EmptyB = False
member x t@(NodeB l y r) = member' t y
        where member' EmptyB c = x == c
              member' (NodeB l' z r') c = if x > z then member' r' c
                                                   else member' l' z

-- 9)
data Color = R | B
data RBT a = E | T Color (RBT a) a (RBT a)

balanceR :: Color -> RBT a -> a -> RBT a -> RBT a
balanceR B a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
balanceR B a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
balanceR c l a r = T c l a r

balanceL :: Color -> RBT a -> a -> RBT a -> RBT a
balanceL B (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
balanceL B (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
balanceL c l a r = T c l a r

insert :: Ord a => a -> RBT a -> RBT a
insert x t = makeBlack (ins x t)
                where ins x E                        = T R E x E
                      ins x (T c l y r ) | x < y     = balanceL c (ins x l) y r
                                         | x > y     = balanceR c l y (ins x r)
                                         | otherwise = T c l y r
                      makeBlack E                    = E
                      makeBlack (T _ l x r )         = T B l x r

-- 10) 
type Rank = Int
data Heap a = EmptyH | N Rank a (Heap a) (Heap a)

rank :: Heap a -> Rank
rank EmptyH = 0
rank (N r _ _ _) = r

makeH x a b = if rank a > rank b then N (rank b + 1) x a b
                                 else N (rank a + 1) x b a

merge :: Ord a => Heap a -> Heap a -> Heap a
merge h1 EmptyH = h1
merge EmptyH h2 = h2
merge h1@(N _ x a1 b1) h2@(N _ y a2 b2) = if x <= y then makeH x a1 (merge b1 h2)
                                                    else makeH y a2 (merge h1 b2)

fromList :: [a] -> Heap a
