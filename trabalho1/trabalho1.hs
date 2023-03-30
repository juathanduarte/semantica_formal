-- Definição das árvore sintática para representação dos programas:

data E = Num Int
    |   Var String
    |   Soma E E
    |   Sub E E
    |   Mult E E
    deriving(Eq,Show)

data B = TRUE
    |   FALSE
    |   Not B
    |   And B B
    |   Or  B B
    |   Leq E E    -- menor ou igual
    |   Igual E E  -- verifica se duas expressões aritméticas são iguais
    deriving(Eq,Show)

data C = While B C
    |   DoWhile C B  -- Do C while B
    |   Repeat C B  -- repeat C until B
    |   If B C C
    |   Seq C C
    |   Atrib E E
    |   Skip
    deriving(Eq,Show)

-----------------------------------------------------
-----
----- As próximas funções, servem para manipular a memória (sigma)
-----
------------------------------------------------

--- A próxima linha de código diz que o tipo memória é equivalente a uma lista de tuplas, onde o primeiro elemento da tupla é uma String (nome da variável) e o segundo um Inteiro (conteúdo da variável):

type Memoria = [(String,Int)]

exSigma :: Memoria
exSigma = [ ("x", 10), ("temp",0), ("y",0)]

exSigma2 :: Memoria
exSigma2 = [("x",3), ("y",0), ("z",0)]

--- A função procuraVar recebe uma memória, o nome de uma variável e retorna o conteúdo dessa variável na memória. Exemplo:
--- *Main> procuraVar exSigma "x"
--- 10

procuraVar :: Memoria -> String -> Int
procuraVar [] s = error ("Variavel " ++ s ++ " nao definida no estado")
procuraVar ((s,i):xs) v
    |   s == v     = i
    |   otherwise  = procuraVar xs v

--- A função mudaVar, recebe uma memória, o nome de uma variável e um novo conteúdo para essa variável e devolve uma nova memória modificada com a varíável contendo o novo conteúdo. A chamada:
---
--- *Main> mudaVar exSigma "temp" 20
--- [("x",10),("temp",20),("y",0)]
---
--- essa chamada é equivalente a operação exSigma[temp->20]

mudaVar :: Memoria -> String -> Int -> Memoria
mudaVar [] v n = error ("Variavel " ++ v ++ " nao definida no estado")
mudaVar ((s,i):xs) v n
    |   s == v     = (s,n):xs
    |   otherwise  = (s,i):mudaVar xs v n

-------------------------------------
---
--- Completar os casos comentados das seguintes funções:
---
---------------------------------

ebigStep :: (E,Memoria) -> Int -- Aqui é definido o tipo de entrada da função ebigStep, que é uma tupla contendo uma expressão e uma memória. O tipo de saída é um número inteiro.
ebigStep (Var x,s) = procuraVar s x -- Se a expressão recebida for uma variável, a função procura o valor correspondente na memória e retorna o seu valor inteiro.
ebigStep (Num n,s) = n -- Se a expressão recebida for um número, retorna o próprio número.
ebigStep (Soma e1 e2,s)  = ebigStep (e1,s) + ebigStep (e2,s) -- Se a expressão recebida for uma soma, a função chama a função ebigStep para calcular o valor das subexpressões e1 e e2 e retorna a soma dos dois valores.
ebigStep (Sub e1 e2,s)  = ebigStep (e1,s) - ebigStep (e2,s) -- Se a expressão recebida for uma subtração, a função chama a função ebigStep para calcular o valor das subexpressões e1 e e2 e retorna a subtração dos dois valores.
ebigStep (Mult e1 e2,s)  = ebigStep (e1,s) * ebigStep (e2,s) -- Se a expressão recebida for uma multiplicação, a função chama a função ebigStep para calcular o valor das subexpressões e1 e e2 e retorna a multiplicação dos dois valores.

-- Exemplos:
-- ebigStep (Soma (Num 2) (Num 3), exSigma)
-- ebigStep (Soma (Var "x") (Num 3), exSigma)
-- ebigStep (Soma (Var "x") (Var "y"), exSigma)

-- ebigStep (Sub (Num 2) (Num 3), exSigma)
-- ebigStep (Sub (Var "x") (Num 3), exSigma)
-- ebigStep (Sub (Var "x") (Var "y"), exSigma)

-- ebigStep (Mult (Num 2) (Num 3), exSigma)
-- ebigStep (Mult (Var "x") (Num 3), exSigma)
-- ebigStep (Mult (Var "x") (Var "y"), exSigma)

bbigStep :: (B,Memoria) -> Bool -- Aqui é definido o tipo de entrada da função bbigStep, que é uma tupla contendo uma expressão booleana e uma memória. O tipo de saída é um valor booleano.
bbigStep (TRUE,s)  = True -- Se a expressão recebida for TRUE, retorna True.
bbigStep (FALSE,s) = False -- Se a expressão recebida for FALSE, retorna False.
bbigStep (Not b,s) -- Se a expressão recebida for uma negação, a função chama a função bbigStep para calcular o valor da subexpressão e retorna o valor da negação.
    |   bbigStep (b,s)   = False -- Se o valor da subexpressão for True, retorna False.
    |   otherwise        = True -- Se o valor da subexpressão for False, retorna True.
bbigStep (And b1 b2,s ) -- Se a expressão recebida for uma conjunção, a função chama a função bbigStep para calcular o valor das subexpressões b1 e b2 e retorna o valor da conjunção.
    |   bbigStep (b1,s) && bbigStep (b2,s) = True -- Se os dois valores das subexpressões forem True, retorna True.
    |   otherwise                          = False -- Se um dos valores das subexpressões for False, retorna False.
bbigStep (Or b1 b2,s ) -- Se a expressão recebida for uma disjunção, a função chama a função bbigStep para calcular o valor das subexpressões b1 e b2 e retorna o valor da disjunção.
    |   bbigStep (b1,s) || bbigStep (b2,s) = True -- Se um dos valores das subexpressões for True, retorna True.
    |   otherwise                          = False -- Se os dois valores das subexpressões forem False, retorna False.
bbigStep (Leq e1 e2,s) -- Se a expressão recebida for uma comparação de menor ou igual, a função chama a função ebigStep para calcular o valor das subexpressões e1 e e2 e retorna o valor da comparação.
    |   ebigStep (e1,s) <= ebigStep (e2,s) = True -- Se o valor da subexpressão e1 for menor ou igual ao valor da subexpressão e2, retorna True.
    |   otherwise                          = False -- Se o valor da subexpressão e1 for maior que o valor da subexpressão e2, retorna False.
bbigStep (Igual e1 e2,s) -- Se a expressão recebida for uma comparação de igualdade, a função chama a função ebigStep para calcular o valor das subexpressões e1 e e2 e retorna o valor da comparação.
    |   ebigStep (e1,s) == ebigStep (e2,s) = True -- Se o valor da subexpressão e1 for igual ao valor da subexpressão e2, retorna True.
    |   otherwise                          = False -- Se o valor da subexpressão e1 for diferente do valor da subexpressão e2, retorna False.

-- Exemplos:

-- bbigStep (Not (TRUE), exSigma)
-- bbigStep (Not (FALSE), exSigma)
-- bbigStep (Not (Leq (Num 2) (Num 3)), exSigma)

-- bbigStep (And (TRUE) (TRUE), exSigma)
-- bbigStep (And (TRUE) (FALSE), exSigma)
-- bbigStep (And (FALSE) (TRUE), exSigma)

-- bbigStep (Or (TRUE) (TRUE), exSigma)
-- bbigStep (Or (TRUE) (FALSE), exSigma)
-- bbigStep (Or (FALSE) (TRUE), exSigma)

-- bbigStep (Leq (Num 2) (Num 3), exSigma)
-- bbigStep (Leq (Num 3) (Num 2), exSigma)
-- bbigStep (Leq (Var "x") (Var "y"), exSigma)

-- bbigStep (Igual (Num 2) (Num 3), exSigma)
-- bbigStep (Igual (Num 3) (Num 3), exSigma)
-- bbigStep (Igual (Var "x") (Var "y"), exSigma)

cbigStep :: (C,Memoria) -> (C,Memoria) -- Aqui é definido o tipo de entrada da função cbigStep, que é uma tupla contendo um comando e uma memória. O tipo de saída é uma tupla contendo um comando e uma memória.
cbigStep (Skip,s) = (Skip,s) -- Se o comando recebido for Skip, retorna o próprio comando.
cbigStep (If b c1 c2,s) -- Se o comando recebido for um comando condicional, a função chama a função bbigStep para calcular o valor da expressão booleana e retorna o comando correspondente ao valor da expressão.
    |   bbigStep (b,s) = cbigStep (c1,s) -- Se o valor da expressão booleana for True, retorna o comando c1.
    |   otherwise = cbigStep (c2,s) -- Se o valor da expressão booleana for False, retorna o comando c2.
-- cbigStep (Seq c1 c2,s) -- Se o comando recebido for uma sequência, a função chama a função cbigStep para calcular o valor do comando c1 e retorna o comando correspondente ao valor do comando c1.
--     |   c1 == Skip = cbigStep (c2,s) -- Se o comando c1 for Skip, retorna o comando c2.
--     -- |   otherwise = let (c1',s') = cbigStep (c1,s) in cbigStep (Seq c1' c2, s') -- Se o comando c1 não for Skip, retorna a sequência do comando c1' com o comando c2.

cbigStep (Seq c1 c2, s) = let (c1',s') = cbigStep (c1,s) in cbigStep (c2, s')

cbigStep (Atrib (Var x) e,s) = (Skip, mudaVar s x (ebigStep (e,s))) -- Se o comando recebido for uma atribuição, a função chama a função ebigStep para calcular o valor da expressão e retorna o comando Skip e a memória com o valor da variável x alterado.
cbigStep (While b c, s) -- Se o comando recebido for um comando de repetição, a função chama a função bbigStep para calcular o valor da expressão booleana e retorna o comando correspondente ao valor da expressão.
    |   bbigStep (b,s) = cbigStep (Seq c (While b c), s) -- Se o valor da expressão booleana for True, retorna a sequência do comando c com o comando While b c.
    |   otherwise = (Skip,s) -- Se o valor da expressão booleana for False, retorna o comando Skip.
cbigStep (DoWhile c b,s) -- Se o comando recebido for um comando de repetição, a função chama a função bbigStep para calcular o valor da expressão booleana e retorna o comando correspondente ao valor da expressão.
    |   bbigStep (b,s) = cbigStep (Seq c (DoWhile c b), s) -- Se o valor da expressão booleana for True, retorna o comando DoWhile c' b.
    |   otherwise = (Skip,s) -- Se o valor da expressão booleana for False, retorna o comando Skip.
cbigStep (Repeat c b,s) -- Se o comando recebido for um comando de repetição, a função chama a função bbigStep para calcular o valor da expressão booleana e retorna o comando correspondente ao valor da expressão.
    |   bbigStep (b,s) = (Skip,s) -- Se o valor da expressão booleana for True, retorna o comando Skip.
    |   otherwise = cbigStep (Seq c (If b Skip (Repeat c b)),s) -- Se o valor da expressão booleana for False, retorna a sequência do comando c com o comando If b Skip (Repeat c b).

-- Exemplos:

-- cbigStep (If (TRUE) (Atrib (Var "x") (Num 3)) (Atrib (Var "x") (Num 2)), exSigma)
-- cbigStep (If (FALSE) (Atrib (Var "x") (Num 1)) (Atrib (Var "x") (Num 3)), exSigma)
-- cbigStep (If (Leq (Num 2) (Num 3)) (Atrib (Var "x") (Num 1)) (Atrib (Var "x") (Num 2)), exSigma)

-- cbigStep (Seq (Atrib (Var "x") (Num 1)) (Atrib (Var "y") (Num 2)), exSigma)
-- cbigStep (Seq (Atrib (Var "x") (Num 1)) (Atrib (Var "x") (Num 2)), exSigma)

-- cbigStep (While (TRUE) (Atrib (Var "x") (Num 1)), exSigma)
-- cbigStep (While (FALSE) (Atrib (Var "x") (Num 1)), exSigma)
-- cbigStep (While (Leq (Num 2) (Num 3)) (Atrib (Var "x") (Num 1)), exSigma)

-- cbigStep (DoWhile (Atrib (Var "x") (Num 1)) (TRUE), exSigma)
-- cbigStep (DoWhile (Atrib (Var "x") (Num 1)) (FALSE), exSigma)
-- cbigStep (DoWhile (Atrib (Var "x") (Num 1)) (Leq (Num 2) (Num 3)), exSigma)

-- cbigStep (Repeat (Atrib (Var "x") (Num 1)) (TRUE), exSigma)
-- cbigStep (Repeat (Atrib (Var "x") (Num 1)) (FALSE), exSigma)
-- cbigStep (Repeat (Atrib (Var "x") (Num 1)) (Leq (Num 2) (Num 3)), exSigma)

-- cbigStep (Atrib (Var "x") (Num 1), exSigma)
-- cbigStep (Atrib (Var "x") (Num 2), exSigma)

-- cbigStep (Skip, exSigma)

--------------------------------------

--------------------------------------
---
--- Exemplos de programas para teste
---
--- O ALUNO DEVE IMPLEMENTAR DOIS EXEMPLOS DE PROGRAMA, UM USANDO O IF E OUTRO USANDO O DO WHILE
-------------------------------------

progIf :: C
progIf = If (Igual (Var "x") (Num 0)) (Atrib (Var "y") (Num 1)) (Atrib (Var "y") (Num 2))

-- A função progIf define um programa que usa uma instrução condicional para decidir qual atribuição será realizada. A condição é dada pela expressão Igual (Var "x") (Num 0), que compara o valor da variável x com o número 0. Se a condição for verdadeira, a instrução Atrib (Var "y") (Num 1) é executada, atribuindo o valor 1 à variável y. Caso contrário, a instrução Atrib (Var "y") (Num 2) é executada, atribuindo o valor 2 à variável y.

-- Para rodar:
-- *Main> cbigStep (progIf, exSigma)

progDoWhile :: C
progDoWhile = DoWhile (Atrib (Var "x") (Soma (Var "x") (Num 1))) (Leq (Var "x") (Num 10)) -- Atribui o valor da variável x + 1 à variável x até que o valor da variável x seja igual a 10.

-- A função progDoWhile define um programa que usa uma instrução de loop para incrementar o valor da variável x até que ela atinja o valor 10. A instrução Atrib (Var "x") (Soma (Var "x") (Num 1)) é executada repetidamente até que a condição Igual (Var "x") (Num 10) seja satisfeita. Ou seja, a variável x será incrementada em 1 a cada iteração do loop, até que ela atinja o valor 10.

-- Para rodar
-- *Main> cbigStep (progDoWhile, exSigma)

---
--- O progExp1 é um programa que usa apenas a semântica das expressões aritméticas. Esse programa já é possível rodar com a implementação que fornecida:

progExp1 :: E
progExp1 = Soma (Num 3) (Soma (Var "x") (Var "y"))

---
--- para rodar:
-- *Main> ebigStep (progExp1, exSigma)
-- 13
-- *Main> ebigStep (progExp1, exSigma2)
-- 6

--- Para rodar os próximos programas é necessário primeiro implementar as regras da semântica
---

---
--- Exemplos de expressões booleanas:

teste1 :: B
teste1 = Leq (Soma (Num 3) (Num 3))  (Mult (Num 2) (Num 3))

teste2 :: B
teste2 = Leq (Soma (Var "x") (Num 3))  (Mult (Num 2) (Num 3))

-- Para rodar:
-- *Main> bbigStep (teste1, exSigma)
-- TRUE
-- *Main> bbigStep (teste2, exSigma)
-- FALSE

---
-- Exemplos de Programas Imperativos:

testec1 :: C -- Troca o valor das variáveis x e y
testec1 = Seq (Seq (Atrib (Var "z") (Var "x")) (Atrib (Var "x") (Var "y"))) -- Atribui o valor de x à z e o valor de y à x
               (Atrib (Var "y") (Var "z")) -- Atribui o valor de z à y

fatorial :: C -- Calcula o fatorial de x e armazena o resultado em y
fatorial = Seq (Atrib (Var "y") (Num 1)) -- Inicializa y com 1
                (While (Not (Igual (Var "x") (Num 1))) -- Enquanto x for diferente de 1
                       (Seq (Atrib (Var "y") (Mult (Var "y") (Var "x"))) -- y = y * x
                            (Atrib (Var "x") (Sub (Var "x") (Num 1))))) -- x = x - 1

-- Testando programas

-- *Main> cbigStep (testec1, exSigma2)

-- *Main> cbigStep (fatorial, exSigma2)