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
fibSeq :: Int -> Seq Int
fibSeq x | x < 0 = Empty
         | x == 0 = singletonS 0
         | otherwise = 