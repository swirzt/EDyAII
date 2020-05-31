import qualified Arr as A
import Arr (!)
import Par

data TreeView a t = EMPTY | ELT a | NODE t t
            deriving Show
data ListView a t = NIL | CONS a t
            deriving Show

emptyS :: A.Arr a
emptyS = A.empty

singletonS :: a -> A.Arr a
singletonS x = A.fromList [x]

lengthS    :: A.Arr a -> Int
lengthS xs =  A.length xs

nthS       :: A.Arr a -> Int -> a 
nthS xs n = xs ! n

tabulateS  :: (Int -> a) -> Int -> A.Arr a
tabulateS = A.tabulate

mapS       :: (a -> b) -> A.Arr a -> A.Arr b
mapS f xs = tabulateS (\ i -> f (nthS xs i)) (lengthS xs)

filterS    :: (a -> Bool) -> A.Arr a -> A.Arr a 
filterS f x = let n = lengthS x in case n of 
                                     0         -> emptyS    
                                     1         -> if f (nthS x 0) then x else emptyS  
                                     otherwise -> let   m = div n 2
                                                        (xs,ys) = filterS f (takeS x m) ||| filterS f (dropS x m)
                                                  in appendS xs ys


appendS    :: A.Arr a -> A.Arr a -> A.Arr a
appendS xs ys = let n = lengthS xs 
                    m = lengthS ys
                    in tabulateS (\ i -> if i < n then nthS xs i else nthS ys (i-n)) (n + m) 

takeS      :: A.Arr a -> Int -> A.Arr a
takeS xs i = A.subArray 0 i xs

dropS      :: A.Arr a -> Int -> A.Arr a
dropS xs i = A.subArray i ((lengthS xs) - i) xs

showtS     :: A.Arr a -> TreeView a (A.Arr a)
showtS x = let n = lengthS x in case n of
                                0         -> EMPTY
                                1         -> ELT (nthS x 0)
                                otherwise ->let m = div n 2   
                                                (xs,ys) = takeS x m ||| dropS x m
                                                in NODE xs ys

showlS     :: A.Arr a -> ListView a (A.Arr a)
showlS x = let n = lengthS x in case n of
                               0          -> NIL
                               otherwise  -> let (xs,ys) = nthS x 0 ||| dropS x 1 in CONS xs ys

joinS      :: A.Arr (A.Arr a) -> A.Arr a
joinS xss = let n = lengthS xss in case n of
                                    0         -> emptyS
                                    1         -> nthS xss 0
                                    2         -> appendS (nthS xss 0) (nthS xss 1)
                                    otherwise -> let m = div n 2
                                                     (ys1,ys2) = joinS (takeS xss m) ||| joinS (dropS xss m)
                                                     in appendS ys1 ys2 

reduceS    :: (a -> a -> a) -> a -> A.Arr a -> a
reduceS f e xs = let t = showtS xs in case t of 
                                      EMPTY     -> e
                                      ELT x     -> x
                                      NODE l r -> f (reduceS f e l) (reduceS f e r)

fromListS   :: [a] -> A.Arr a
fromListS = A.fromList