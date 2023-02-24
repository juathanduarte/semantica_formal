menorTres :: Int -> Int -> Int -> Int
menorTres a b c 
    | a < b && a < c = a
    | b < a && b < c = b
    | otherwise = c