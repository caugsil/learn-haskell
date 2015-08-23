module Lista2 where

-- Andrei Martins Silva - 1210045-7 Noturno

{-
Exercício 2.1 Faça um novo tipo chamado Metros, que possui um value cons-
tructor de mesmo nome cujos parâmetros são um Int que representa a dimensão
e um Double que representa o valor da medida. 
Implemente as funções
1. areaQuadrado :: Metros → Metros: Calcula a área de um quadrado
2. areaRet :: Metros → Metros → Metros: Calcula a área de um retangulo
3. areaCubo :: Metros → Metros: Calcula a área de um cubo
Exemplo: areaQuadrado(Metros 1 2.0) = Metros 2 4.0
Use o pattern matching para ignorar as metragens erradas (cáclular a área de
um quadrado com um lado de dimensão 4 não é válido).
-}

data Metros = Metros Int Double deriving Show

areaQuadrado :: Metros -> Metros
areaQuadrado (Metros 1 l) = Metros 2 (l * l)
areaQuadrado (Metros _ _) = error "Nao foi possivel calcular a area"

areaRet :: Metros -> Metros -> Metros
areaRet (Metros 1 b) (Metros 1 h) = Metros 2 (b*h)
areaRet (Metros _ _) (Metros _ _) = error "Nao foi possivel calcular a area"

areaCubo :: Metros -> Metros
areaCubo (Metros 1 a) = Metros 2 (6 * a * a)
areaCubo (Metros 2 m) = Metros 2 (6 * m)
areaCubo (Metros _ _) = error "Nao foi possivel calcular a area"

{-
Exercício 2.2 (Validação de nomes) Faça o novo tipo Valido que possui dois
value constructor Sim e Nao. O value constructor Sim possui um parâmetro
(campo) String. Implemente uma função isNomeValido que recebe um nome
e retorna Nao caso a String seja vazia e Sim caso contrário.
-}

data Valido = Sim String | Nao deriving Show

isNomeValido :: String -> Valido
isNomeValido [] = Nao
isNomeValido s  = Sim s

{-
Exercício 2.3 Refaça o exercício 3 do capítulo anterior usando record syntax e
tipos com parâmetro (siga o exemplo da conversão de medidas SI para imperial).
-}

data Valido' = Sim' {nome :: String} | Nao' deriving Show

isNomeValido' :: String -> Valido'
isNomeValido' [] = Nao'
isNomeValido' s = Sim' {nome = s}

