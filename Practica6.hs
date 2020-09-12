-- 1)
-- a)
promedios :: Seq s => s Int -> s Float
promedios x = let (seq,val) = scanS (+) 0 x
                  reseq = dropS 1 seq
                  (finseq,largo) = appendS reseq (singleton val) ||| lengthS x
                  in tabulateS (\i -> (nthS i finseq) / (i+1)) largo
-- b)
mayores :: Seq s => s Int -> Int
mayores x = let (seq,val) = mapS (max) minBound x
                reseq = dropS 1 seq
                (finseq,largo) = appendS reseq (singleton val) ||| (lengthS x) - 1
                finfin = tabulateS (\i->(nthS (i+1) finseq)>(nthS i finseq)) largo
                in reduceS (+) 0 finfin

--2)
     w' x'
     y' z'
w x
y z

type Matriz = (Int,Int,Int,Int)
multM :: Matriz -> Matriz -> Matriz
multM (w,x,y,z) (w',x',y',z') = (w*w'+x*y',w*x'+x*z',y*w'+z*y',y*x'+z*z')

fibSeq :: Int -> Seq Int
fibSeq x = let xs = tabulateS (\_->(1,1,1,0)) x
               (ys,_) = scanS multM (1,0,0,1) xs
           in mapS (\(i,_,_,_)->i) ys

-- 5)
-- b)
-- data Sec = Nothing | All (Int,Int,Int) | Some ((Int,Int,Int),(Int,Int,Int),Int)

-- sccml :: Seq Int -> Int
-- sccml xs = let ys = mapS (\i->(i,i,1)) xs
--                (y,((_,_,a),(_,_,b),c)) = scanS (agrupar) Nothing ys
--                in max (max a b) c
--             where
--                 third (_,_,k) = k
--                 agrupar Nothing a = a
--                 agrupar a Nothing = a
--                 agrupar (All (i,j,k)) (All (i',j',k')) = if j < i' then All (i,j',k+k') else Some ((i,j,k),(i',j',k'),0)
--                 agrupar (All (i,j,k)) (Some ((i',j',k'),b,c) = if j < i' then Some ((i,j',k+k',b,c)) else Some ((i,j,k),b,max c k')
--                 agrupar (Some (a,(i,j,k),c) (All (i',j',k')) = if j < i' then Some (a,(i,j',k+k'),c) else Some (a,(i',j',k'),max c k)
--                 agrupar (Some (a,(i,j,k),c) (Some ((i',j',k'),b',c') = let t = if j < i' then max c (k + k') else max (max k k') c in Some (a,b',t)

sccml :: Seq Int -> Int
sccml xs = let ys = mapS (\i->(i,i,1,1)) xs
               f (w1,x1,y1,z1) (w2,x2,y2,z2) = if w2 > x1 && z2 == y2 then (w1,x2,y2+y1,z2+z1) else (w1,x2,y2+y1,z2)
               zs = scanS f (minBound,minBound,0,0) ys
               in mapreduceArr (\(_,_,_,i)->i) max minBound zs

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



-- 3)
aguaHist :: Seq s => s Int -> s Int
aguaHist xs = let n = lengthS xs
                  ys = tabulateS h n
                  h i | i == 0 = (0,nthS i xs,nthS (i+1) xs)
                      | i == n-1 = (nthS (i-1) xs,nthS i xs,0)
                      | otherwise = (nthS (i-1) xs,nthS i xs,nthS (i+1) xs)
                  f (x,_,z) (x',y',z') = (max x x',y',max z z')
                 in scanS f (0,0,0) ys

-- 6)
divisor :: (Int, Seq Int) -> Int
divisor (x,s) = lengthS (filterS (\i->mod x i == 0) s)

multiplos :: Seq Int -> Int
multiplos s = let xs = tabulateS (\i -> (nthS s i, dropS s (i+1))) (lengthS s)
              in mapreduceS divisor (+) 0 xs

-- 7)
lt :: (a -> a -> Ordering) -> a -> a -> Bool
lt f x y = f y x == LT

gteq :: (a -> a -> Ordering) -> a -> a -> Bool
gteq f x y = f y x == GT && f y x EQ

merge :: (a -> a -> Ordering) -> Seq a -> Seq a -> Seq a
merge f xs ys = case showtS xs of
                    EMPTY -> ys
                    ELT x -> let (y1,y2) = filterS (\i->lt f x) ys ||| filterS (\i->gteq f x) ys in appendS (appendS y1 (singletonS x)) y2
                    NODE l r -> let x = nthS r 0
                                    (y1,y2) = filterS (\i->lt f x) ys ||| filterS (\i->gteq f x) ys
                                    (l',r') = merge l y1 ||| merge r y2
                                in appendS l' r'

sort :: (a -> a -> Ordering) -> Seq a -> Seq a
sort f xs = case showtS xs of
                NODE l r -> let (l',r') = sort f l ||| sort f r in merge f l' r'
                _ -> xs

maxE :: (a -> a -> Ordering) -> Seq a -> a
maxE f xs = case showtS xs of
                ELT x -> x
                NODE l r -> let (ml,mr) = maxE f l ||| maxE f r in if f ml mr == GT then ml else mr

maxS :: (a -> a -> Ordering) -> Seq a -> Nat
maxS f xs = snd $ maxS' f xs

maxS' :: (a -> a -> Ordering) -> Seq a -> (a,Int)
maxS' f xs = case showtS xs of
                ELT x -> (x,0)
                NODE l r -> let ((ml,il),(mr,ir)) = maxS' f l ||| maxS' f r 
                                ll = lengthS l
                            in if f ml mr == GT then (ml,il) else (mr,ll+ir)

combine :: (a -> a -> Ordering) -> Seq a -> Seq a
combine f l r = if lengthS l == 0 then r else
                if lengthS r == 0 then l else
                let ll = lengthS l in
                    if f (nthS l (ll-1)) (nthS r 0) == EQ then appendS l (dropS r 1) else appendS l r    

group :: (a -> a -> Ordering) -> Seq a -> Seq a
group f xs = let ys = mapS (\i->singletonS i) xs
             in reduceS (combine f) emptyS ys

group' :: Ord a => Seq s => s (a, b) -> s (a, s b)
group' s = 
  let ss = mapS (\(i, j) -> singletonS (i, singletonS j)) s
  in reduceS comb emptyS ss 
  where
    comb l r
      | lengthS l == 0 = r
      | lengthS r == 0 = l
      | otherwise = if (cmp lastL firstR) == EQ then (takeS l (lengthS l - 1)) `appendS` singletonS (fst lastL, (appendS (snd lastL) (snd firstR))) `appendS` dropS r 1
                    else appendS l r
                    where lastL = nthS l ((lengthS l) - 1)
                          firstR = nthS r 0
                          cmp (a, _) (c, _) = compare a c 

-- collect (fromList [(2, "a"), (1, "b"), (1, "c"), (2, "d")] :: A.Arr (Int, String))
collect :: (Seq s, Ord a) => s (a, b) -> s (a, s b)
collect s = let s' = sortS cmp s
            in group' s'
            where cmp (a, _) (c, _) = compare a c


-- 8)

datosIngreso :: Seq (String,Seq Int) -> Seq (Int,Int)
datosIngreso xs = mapCollectreduce (\(_,s)->promedio s) ()