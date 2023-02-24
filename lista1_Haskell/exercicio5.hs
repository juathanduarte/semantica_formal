potencia :: Int -> Int -> Int
potencia x 0 = 1
potencia x y = x * potencia x (y - 1)