vacia :: [a]
vacia = []

poner :: a -> [a] -> [a]
poner = (:)

primero :: [a] -> a
primero [x] = x
primero (_:xs) = primero xs

sacar :: [a] -> [a]
sacar [x] = []
sacar (x:xs) = x : sacar xs

esVacia :: [a] -> Bool
esVacia [] = True
esVacia _ = False