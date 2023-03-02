data Arvore = Folha Int | Nodo Int Arvore Arvore
    deriving (Eq, Show)

multArvore:: Int -> Arvore -> Arvore
multArvore n (Folha x) = Folha (n*x)
multArvore n (Nodo x a1 a2) = Nodo (n*x) (multArvore n a1) (multArvore n a2)

contaFolhas :: Arvore -> Int
contaFolhas (Folha x) = 1
contaFolhas (Nodo x a1 a2) = contaFolhas a1 + contaFolhas a2

contaNodos :: Arvore -> Int
contaNodos (Folha x) = 0
contaNodos (Nodo x a1 a2) = 1 + contaNodos a1 + contaNodos a2

quantasVezes :: Int -> Arvore -> Int
quantasVezes n (Folha x) = if n == x then 1 else 0

maxArvore :: Arvore -> Int
maxArvore (Folha x) = x
maxArvore (Nodo x a1 a2) = max x (max (maxArvore a1) (maxArvore a2))

refleteArvore :: Arvore -> Arvore
refleteArvore (Folha x) = Folha x
refleteArvore (Nodo x a1 a2) = Nodo x (refleteArvore a2) (refleteArvore a1)

geraLista :: Arvore -> [Int]
geraLista (Folha x) = [x]
geraLista (Nodo x a1 a2) = geraLista a1 ++ [x] ++ geraLista a2

