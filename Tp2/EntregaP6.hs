-- 4)
-- b)
data Paren = Open | Close

parenToInt :: Paren -> Int
parenToInt Open = 1
parenToInt Close = -1

matchParen :: Seq Paren -> Bool
matchParen s =  let xs = mapS (parenToInt) s
                    (ys,y) = scanS (+) 0 xs
                    zs = filterS (\i->i < 0) ys
                in ((lengthS zs) == 0) && (y == 0)

-- 6)
divisor :: (Int, Seq Int) -> Int
divisor (x,s) = lengthS (filterS (\i->mod x i == 0) s)

multiplos :: Seq Int -> Int
multiplos s = let xs = tabulateS (\i -> (nthS s i, dropS s (i+1))) (lengthS s)
                  ys = mapS (divisor) xs
              in reduceS (+) 0 ys