data Tree a = E | L a | N (Tree a) (Tree a)
        deriving Show

partir :: Tree a -> Tree (Tree a, a, Tree a)
partir E = E
partir (L x) = L (E,x,E)
partir x = partir' x E 
         
         
--          n
--     n         n
--  1     2    3   E

--               n
--        n                     n
-- (E,1,n) (L 1,2,L 3)   (n,3,E) E
--     2 3               1 2