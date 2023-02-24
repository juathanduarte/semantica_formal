contaInteiro :: Int -> [Int] -> Int
contaInteiro _ [] = 0
contaInteiro x (a:b) = if x == a then 1 + contaInteiro x b else contaInteiro x b