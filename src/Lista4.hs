module Lista4 where
{-
Andrei Martins Silva 12100457
Carlos Augusto Silva
-}
{-
Exercício 4.1 Usando a estrtura de árvore, 
monte uma função mapA, que jogue uma função passada
por parâmetro para todos os elementos de uma árvore. 
Deixe explícito o tipo desta função.
-}

data Arvore a = Nulo | Leaf a | Branch a (Arvore a) (Arvore a)

mapA :: (a -> [a]) -> Arvore a -> [a] 
mapA (f) (Branch x l r) = mapA f l ++ f x ++ mapA f r
mapA (f) (Leaf x) = f x
mapA _ (Nulo) = []
 
{-
Exercício 4.2 Usando o exercício acima, some 5 a cada elemento de uma árvore
de inteiros.
-}
soma5 :: Integer -> [Integer]
soma5 x = [x + 5]












