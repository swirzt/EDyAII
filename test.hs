fact :: Integer Int -> Integer Int
fact 0 = 1
fact x = x * fact(x-1)