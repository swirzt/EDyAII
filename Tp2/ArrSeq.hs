import qualified Arr as A
import Par
import Arr ((!))
import Seq
import TestTree

emptyArr :: A.Arr a
emptyArr = A.empty

singletonArr :: a -> A.Arr a
singletonArr x = A.fromList [x]

lengthArr    :: A.Arr a -> Int
lengthArr = A.length

nthArr       :: A.Arr a -> Int -> a 
nthArr = (!)

tabulateArr  :: (Int -> a) -> Int -> A.Arr a
tabulateArr = A.tabulate

mapArr       :: (a -> b) -> A.Arr a -> A.Arr b
mapArr f xs = tabulateArr (\ i -> f (nthArr xs i)) (lengthArr xs)

filterArr    :: (a -> Bool) -> A.Arr a -> A.Arr a 
filterArr f x = let n = lengthArr x in case n of 
                                     0         -> emptyArr    
                                     1         -> if f (nthArr x 0) then x else emptyArr  
                                     otherwise -> let   m = div n 2
                                                        (xs,ys) = filterArr f (takeArr x m) ||| filterArr f (dropArr x m)
                                                  in appendArr xs ys

appendArr    :: A.Arr a -> A.Arr a -> A.Arr a
appendArr xs ys = let n = lengthArr xs 
                      m = lengthArr ys
                  in tabulateArr (\ i -> if i < n then nthArr xs i else nthArr ys (i-n)) (n + m) 

takeArr      :: A.Arr a -> Int -> A.Arr a
takeArr xs i = let n = lengthArr xs in if n <= i then xs else A.subArray 0 i xs

dropArr      :: A.Arr a -> Int -> A.Arr a
dropArr xs i = let n = lengthArr xs in if n <=i then emptyArr else A.subArray i ((lengthArr xs) - i) xs

showtArr     :: A.Arr a -> TreeView a (A.Arr a)
showtArr x = let n = lengthArr x in case n of
                                0         -> EMPTY
                                1         -> ELT (nthArr x 0)
                                otherwise ->let m = div n 2   
                                                (xs,ys) = takeArr x m ||| dropArr x m
                                                in NODE xs ys

showlArr :: A.Arr a -> ListView a (A.Arr a)
showlArr x = let n = lengthArr x in case n of
                               0          -> NIL
                               otherwise  -> let (xs,ys) = nthArr x 0 ||| dropArr x 1 in CONS xs ys

joinArr :: A.Arr (A.Arr a) -> A.Arr a
joinArr = A.flatten

contractArr :: (a -> a -> a) -> A.Arr a -> A.Arr a
contractArr f xs = let n = lengthArr xs
                       k = div n 2
                       h i = f (nthArr xs (2*i)) (nthArr xs (2*i+1))
                        in if even n then tabulateArr (\i-> h i) k
                                     else tabulateArr (\i-> if i == k then nthArr xs (2*i) else h i) (k+1)

reduceArr :: (a -> a -> a) -> a -> A.Arr a -> a
reduceArr f e xs = case lengthArr xs of
                                  0 -> e
                                  1 -> nthArr xs 0
                                  otherwise -> let ys = contractArr f xs in reduceArr f e ys

expandArr :: (a->a->a) -> A.Arr a -> A.Arr a -> A.Arr a
expandArr f xs ys = let n = lengthArr xs
                        combinArr i = if even i then (nthArr ys (div i 2)) 
                                                else f (nthArr ys (div i 2)) (nthArr xs (i-1))
                        in tabulateArr (combinArr) n

scanArr :: (a->a->a) -> a -> A.Arr a -> (A.Arr a, a)
scanArr f e xs = case lengthArr xs of
                               0 -> (singletonArr e,e)
                               1 -> (singletonArr e, f e (nthArr xs 0))
                               otherwise -> let s = contractArr f xs 
                                                (ys,y) = scanArr f e s
                                                rs = expandArr f xs ys
                                            in (rs,y)


fromListArr :: [a] -> A.Arr a
fromListArr = A.fromList

instance Seq A.Arr where
   emptyS = emptyArr
   singletonS = singletonArr
   lengthS = lengthArr
   nthS = nthArr 
   tabulateS = tabulateArr
   mapS = mapArr
   filterS = filterArr
   appendS = appendArr
   takeS = takeArr
   dropS = dropArr
   showtS = showtArr
   showlS = showlArr
   joinS = joinArr
   reduceS = reduceArr
   scanS = scanArr
   fromList = fromListArr
