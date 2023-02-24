multLista :: [Int] -> Int
multLista [] = 1
multLista (x:xs) = x * multLista xs