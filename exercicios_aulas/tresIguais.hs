tresIguais :: Int -> Int -> Int -> Bool
tresIguais a b c
    | a == b && b == c = True
    | otherwise = False