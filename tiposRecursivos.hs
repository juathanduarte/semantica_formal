data Arvore = Folha Int | Nodo Int Arvore Arvore
    deriving (Eq, Show)

arv1 :: Arvore
arv1 =
    Nodo 10 (Nodo 14 (Nodo 1 (Folha 4) (Folha 2)) (Folha 6)) (Folha 9)

somaArvore :: Arvore -> Int
somaArvore (Folha x) = x
somaArvore (Nodo x a1 a2) = x + somaArvore a1 + somaArvore a2

multDoisArvore :: Arvore -> Arvore
multDoisArvore (Folha x) = Folha (x * 2)
multDoisArvore (Nodo x a1 a2) = Nodo (x * 2) (multDoisArvore a1) (multDoisArvore a2)