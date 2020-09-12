-- -- 4)
-- -- b)
-- data Paren = Open | Close

-- parenToInt :: Paren -> Int
-- parenToInt Open = 1
-- parenToInt Close = -1

-- matchParen :: Seq Paren -> Bool
-- matchParen s =  let xs = mapS (parenToInt) s
--                     (ys,y) = scanS (+) 0 xs
--                     zs = filterS (\i->i < 0) ys
--                 in ((lengthS zs) == 0) && (y == 0)

-- -- 6)
-- divisor :: (Int, Seq Int) -> Int
-- divisor (x,s) = lengthS (filterS (\i->mod x i == 0) s)

-- multiplos :: Seq Int -> Int
-- multiplos s = let xs = tabulateS (\i -> (nthS s i, dropS s (i+1))) (lengthS s)
--                   ys = mapS (divisor) xs
--               in reduceS (+) 0 ys

import Arr
import ArrSeq
import qualified Arr as A
import Par
data Sec = Nada | All (Int,Int,Int) | Some ((Int,Int,Int),(Int,Int,Int),Int)

-- agrupar :: Sec -> Sec -> Sec
-- agrupar Nada a = a
-- agrupar a Nada = a
-- agrupar (All (i,j,k)) (All (i',j',k')) = if j < i' then All (i,j',k+k'+1) else Some ((i,j,k),(i',j',k'),0)
-- agrupar (All (i,j,k)) (Some ((i',j',k'),b,c)) = if j < i' then Some ((i,j',k+k'+1),b,c) else Some ((i,j,k),b,max c k')
-- agrupar (Some (a,(i,j,k),c)) (All (i',j',k')) = if j < i' then Some (a,(i,j',k+k'+1),c) else Some (a,(i',j',k'),max c k)
-- agrupar (Some (a,(i,j,k),c)) (Some ((i',j',k'),b',c')) = let t = if j < i' then max c (k + k'+1) else max (max k k') c in Some (a,b',t) 

-- sccml :: A.Arr Int -> Int
-- sccml xs = let ys = mapArr (\i->All (i,i,0)) xs
--                (_,zs) = scanArr (agrupar) Nada ys
--                in case zs of
--                    All (_,_,z) -> z
--                    Some ((_,_,z1),(_,_,z2),z3) -> max (max z1 z2) z3

mapcontractArr :: (b -> a) -> (a -> a -> a) -> A.Arr b -> A.Arr a
mapcontractArr g f xs = let n = lengthArr xs
                            k = div n 2
                            h i = f (g (nthArr xs (2*i))) (g (nthArr xs (2*i+1)))
                        in if even n then tabulateArr (\i-> h i) k
                                     else tabulateArr (\i-> if i == k then g(nthArr xs (2*i)) else h i) (k+1)

mapreduceArr :: (b -> a) -> (a -> a -> a) -> a -> A.Arr b -> a
mapreduceArr g f e xs = case lengthArr xs of
                                  0 -> e
                                  1 -> f e (g (nthArr xs 0))
                                  otherwise -> let ys = mapcontractArr g f xs in mapreduceArr id f e ys

aguaHist :: A.Arr Int -> Int
aguaHist xs = let n = lengthArr xs
                  h i = if i == 0 then (0,nthArr xs i)
                                  else (nthArr xs (i-1),nthArr xs i)
                  h' i = if i == 0 then 0
                                   else nthArr xs (n-i)
                  (ys,ysr) = tabulateArr h n ||| tabulateArr h' n
                  f (x,_) (x',y') = (max x x',y')
                  ((zs,z),(zsr,zr)) = scanArr f (0,0) ys ||| scanArr max 0 ysr
                  m = lengthArr zs - 2 -- Saco 2 porque no me imporatn el caso base y los 2 extremos
                  fin = tabulateArr (\i -> max 0 $ (min (fst (nthArr zs (i+2))) (nthArr zsr (m+1-i))) - (snd (nthArr zs (i+2)))) m
                  in reduceArr (+) 0 fin

-- aguaHistJuanchi :: A.Arr Int -> (A.Arr (Int,Int),A.Arr (Int,Int,Int))
-- aguaHistJuanchi xs = let n = lengthArr xs
--                          h i = if i == 0 then (0,i)
--                                          else (nthArr xs (i-1),i)
--                          ys = tabulateArr h n
--                          f (x,_) (x',y') = (max x x',y')
--                          (zs,z) = scanArr f (0,0) ys
--                          zs' = appendArr (dropArr zs 1) (singletonArr z)
--                         --  g (x,y,z) (z',y',z') = 
--                          in (zs,mapArr (\(j,i)->(j,max 0 $ j - (nthArr xs i),fst $ nthArr zs' i)) zs')
--                         --  ws = mapreduceArr (\(j,i)->(j,max 0 $ j - (nthArr xs i),snd $ nthArr zs' i)) (\i j ->j) (0,0,0) zs'
--                         --  in ws

sccml :: A.Arr Int -> Int
sccml xs = let ys = mapArr (\i->(i,i,1,1)) xs
               f (w1,x1,y1,z1) (w2,x2,y2,z2) = if w2 > x1 && z2 == y2 then (w1,x2,y2+y1,z2+z1) else (w1,x2,y2+y1,z2)
               (zs,z) = scanArr f (minBound,minBound,0,0) ys
               ks = appendArr (dropArr zs 1) (singletonArr z)
               in (mapreduceArr (\(_,_,_,i)->i) max minBound ks) - 1

acuerdo :: A.Arr Int -> Int -> Int -> (Int, Float)
acuerdo xs x y = let meses = lengthArr xs
                     (acum,total) = scanArr (+) 0 xs
                     fixed = appendArr (dropArr acum 1) (singletonArr total)
                     h i = if (nthArr fixed i) >= x then (y,2) else (nthArr xs i,1)
                     pagos = tabulateArr h meses
                     mesesy = lengthArr (filterArr (\(_,i)->i==2) pagos)
                     totalfinal = reduceArr (+) 0 (mapArr (\(i,_)->i) pagos)
                     in (totalfinal,((fromIntegral mesesy)/(fromIntegral meses))*100)

intereses :: A.Arr Int -> Int -> Int
intereses xs x = let meses = lengthArr xs
                     ys = tabulateArr (\i->(nthArr xs i,nthArr xs i,if i ==0 then 0 else nthArr xs (i-1))) meses
                     f (x1,y1,z1) (x2,y2,z2) = if x1 + y1 >= x then (x1+x2, y2, z1) else (x1+x2, y2, z2)
                     (zs,z) = scanArr f (0,0,0) ys
                     fix = appendArr (dropArr zs 1) (singletonArr z)
                     casifin = mapArr (\(i,j,k)->if i >= x then k else j) fix
                 in reduceArr (+) 0 casifin
