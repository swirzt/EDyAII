data Tree a = E | N Int (Tree a) a (Tree a) deriving Show

(|||) :: a -> b -> (a,b)
x ||| y = (x,y)

size :: Tree a -> Int
size E = 0
size (N n _ _ _) = n

dropWhileEnd :: (a -> Bool) -> Tree a -> Tree a
dropWhileEnd _ E = E
dropWhileEnd p s@(N n l x r) = if p x then case dropWhileEnd p r of
                                            E -> dropWhileEnd p l
                                            r' -> N (size l + size r' + 1) l x r'
                                      else let r' = dropWhileEnd p r
                                           in N (size l + size r' + 1) l x r'

dropWhileT :: (a -> Bool) -> Tree a -> Tree a
dropWhileT _ E = E
dropWhileT p (N _ l x r) = let ((l',r'),x') = dropWhileT p l ||| dropWhileT p r ||| p x
                          in if x' then case l' of
                                            E -> r'
                                            _ -> N (size l' + size r + 1) l' x r
                                   else N (size l' + size r + 1) l' x r

takeWhileEnd' :: (a -> Bool) -> Tree a -> (Tree a,Bool)
takeWhileEnd' _ E = (E,False)
takeWhileEnd' p (N _ l x r) = let ((l',bl'),(r',br')) = takeWhileEnd' p l ||| takeWhileEnd' p r
                              in if p x then case br' of
                                                False -> (N (size l' + size r + 1) l' x r,bl')
                                                _ -> (r',br')
                                        else (r',True)

takeWhileEnd :: (a -> Bool) -> Tree a -> Tree a
takeWhileEnd p s = fst (takeWhileEnd' p s)

fromList :: [a] -> Tree a
fromList [] = E
fromList xs = let n = length xs
                  k = div n 2
                  in N n (fromList (take k xs)) (xs !! k) (fromList (drop (k+1) xs))

toList :: Tree a -> [a]
toList E = []
toList (N _ l x r) = toList l ++ [x] ++ toList r