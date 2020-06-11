import Par
import Seq
import TestTree

emptyList :: [a]
emptyList = []

singletonList :: a -> [a]
singletonList x = [x]

lengthList :: [a] -> Int
lengthList xs = ll xs 0
            where ll [] n = n
                  ll (_:ys) n = ll ys (n+1)

nthList :: [a] -> Int -> a
nthList (x:_) 0 = x
nthList (_:xs) n = nthList xs (n-1)

tabulateList  :: (Int -> a) -> Int -> [a]
tabulateList _ 0 = []
tabulateList f k = tabulateList' f 0 (k-1)

tabulateList' :: (Int -> a) -> Int -> Int -> [a]
tabulateList' f x k = if x == k then [f x] else let (y,ys) = f x ||| (tabulateList' f (x+1) k) in y:ys

mapList :: (a -> b) -> [a] -> [b]
mapList f [] = []
mapList f (x:xs) = let (y,ys) = f x ||| mapList f xs in y : ys

filterList :: (a -> Bool) -> [a] -> [a]
filterList _ [] = []
filterList f (x:xs) = let (y,ys) = f x ||| filterList f xs
                        in if y then x : ys
                                else ys

appendList :: [a] -> [a] -> [a]
appendList [] y = y
appendList (x:xs) y = x : appendList xs y

takeList :: [a] -> Int -> [a]
takeList _ 0 = []
takeList [] _ = []
takeList (x:xs) n = x : takeList xs (n-1)

dropList :: [a] -> Int -> [a]
dropList x 0 = x
dropList [] _ = []
dropList (_:xs) n = dropList xs (n-1)

showtList :: [a] -> TreeView a [a]
showtList [] = EMPTY
showtList [x] = ELT x
showtList xs = let n = lengthList xs
                   m = div n 2
                   (xss,yss) = takeList xs m ||| dropList xs m
                in NODE xss yss

showlList :: [a] -> ListView a [a]
showlList [] = NIL
showlList (x:xs) = CONS x xs 

joinList :: [[a]] -> [a]
joinList = reduceList (appendList) []

contract :: (a -> a -> a) ->[a] -> [a]
contract _ [] = []
contract _ u@[x] = u
contract f (x:y:xs) = let (ys,yss) = f x y ||| contract f xs in ys : yss

reduceList :: (a -> a -> a) -> a ->[a] -> a
reduceList _ e [] = e
reduceList _ _ [x] = x
reduceList f e xs = let ys = contract f xs in reduceList f e ys

scanList :: (a -> a -> a) -> a -> [a] -> ([a],a)
scanList _ e [] = ([e],e)
scanList f e [x] = ([e],f e x)
scanList f e xs = let s = contract f xs
                      (ys,y) = scanList f e s
                      r = expand f xs ys
                      in (r,y)

expand :: (a -> a -> a) -> [a] -> [a] -> [a]
expand _ [] _ = []
expand _ [_] ys = ys
expand f xs@(x:_:xss) ys@(y:yss) = y : (f y x) : expand f xss yss

-- expand :: (a->a->a) -> [a] -> ([a],a) -> Int -> ([a],a)
-- expand _ [] (_,yss) _ = ([],yss)
-- expand _ [_] t _ = t 
-- expand f l@(x:_:xss) t@((y:ys),yss) n = if even n then let (zs,z) = expand f l t (n+1) in (y:zs,z)
--                                                   else let (zs,z) = expand f xss (ys,yss) (n+1) 
--                                                            k = f y x
--                                                            in (k:zs,z)

fromListList :: [a] -> [a]
fromListList = id

instance Seq [] where
   emptyS = emptyList
   singletonS = singletonList
   lengthS = lengthList
   nthS = nthList 
   tabulateS = tabulateList
   mapS = mapList
   filterS = filterList
   appendS = appendList
   takeS = takeS
   dropS = dropList
   showtS = showtList
   showlS = showlList
   joinS = joinList
   reduceS = reduceList
   scanS = scanList
   fromList = fromListList

