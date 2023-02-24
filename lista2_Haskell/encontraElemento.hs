encontraElemento :: Int -> [Int] -> Bool
encontraElemento _ [] = False
encontraElemento x (a:b) = if x == a then True else encontraElemento x b