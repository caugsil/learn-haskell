module Lista1 where
-- Andrei Martins Silva 1210045-7 Noturno
-- Carlos Augusto Silva

-- Exercicio 1.1
{-
    Crie o tipo Pergunta com os value constructors Sim ou Nao.
    Faca as funcoes abaixo determinando seus tipos explicitamente:
-}

data Pergunta = Nao | Sim deriving Show

-- 1. pergNum: recebe via parâmetro uma Pergunta e retorne 0 para Nao e 1 para Sim;

pergNum :: Pergunta -> Int
pergNum Nao = 0
pergNum Sim = 1

-- 2. listPergs: recebe via parâmetro uma lista de Perguntas e retorna 0’s e 1’s correspondentes aos constructores contidos na lista;

listPergs :: [Pergunta] -> [Int]
listPergs ps = [pergNum p | p<-ps]

--3. and’: recebe uma Pergunta como parâmetro e retorna a tabela verdade do and lógico usando Sim como verdadeiro e Nao como falso.

and' :: Pergunta -> Pergunta -> Pergunta
and' Sim Sim = Sim
and' _ _ = Nao

--4. or’: Idem acima, porém, deve ser usado o ou lógico.

or' :: Pergunta -> Pergunta -> Pergunta
or' Nao Nao = Nao
or' _ _ = Sim

--5. not’: Idem aos anteriores, porém, usando o not lógico.

not' :: Pergunta -> Pergunta
not' Sim = Nao
not' Nao = Sim

-- Exercício 1.2 
{-
    Gere as listas [1,11,121,1331,14641,161051,1771561] e
    [1,2,3,5,6,7,9,10,11,13,14,15,17,18,19,21,22,23,25,26,27,29,30,31,33,34,35,37,38,39] usando
    list comprehension.
-}

l1 :: [Int]
l1 = [11 ^ x | x<-[0..6]]

-- Exercício 1.3 
{-
    Faça o tipo Temperatura que pode ter valores Celsius, Farenheit
    ou Kelvin. Implemente as funções:
    
    Formulas para conversoes consultadas em: http://www.infoescola.com/fisica/conversao-de-escalas-termometricas/
-}

data Temperatura = Celsius | Fahrenheit | Kelvin

-- 1. converterCelsius: recebe um valor double e uma temperatura e faz a conversão para Celsius.

toCelsius :: Double -> Temperatura -> Double
toCelsius k Kelvin = k - 273
toCelsius f Fahrenheit = 5 * (f-32) / 9
toCelsius c Celsius = c


-- 2. converterKelvin: recebe um valor double e uma temperatura e faz a conversão para Kelvin.
toKelvin :: Double -> Temperatura -> Double
toKelvin c Celsius = c + 273
toKelvin f Fahrenheit = toKelvin (toCelsius f Fahrenheit) Celsius
toKelvin k Kelvin = k

-- 3. converterFarenheit: recebe um valor double e uma temperatura e faz a conversão para Farenheit.
toFahrenheit :: Double -> Temperatura -> Double
toFahrenheit c Celsius = (9 * (c/ 5)) + 32
toFahrenheit k Kelvin  = toFahrenheit (toCelsius k Kelvin) Celsius
toFahrenheit f Fahrenheit = f

{-
Exercício 1.4 Faça uma função que simule o vencedor de uma partida de pedra,
papel e tesoura usando tipos criados (você não poderá usar qualquer outro tipo
que não seja criado usando o data). Casos de empate devem ser considerados em
seu tipo.
-}
data Jogada = Pedra | Papel | Tesoura
data Vencedor = Jogador1 | Jogador2 | Empate deriving Show

jogo :: Jogada -> Jogada -> Vencedor
jogo Papel Papel     = Empate
jogo Papel Tesoura   = Jogador2
jogo Papel Pedra     = Jogador1
jogo Tesoura Tesoura = Empate
jogo Tesoura Papel   = Jogador1
jogo Tesoura Pedra   = Jogador2
jogo Pedra Pedra     = Empate
jogo Pedra Papel     = Jogador2
jogo Pedra Tesoura   = Jogador1



{-Exercício 1.5 Faça uma função que retorne uma string com todas as vogais
maiúsculas e minúsculas eliminadas de uma string passada por parâmetro usando
list compreenshion.
Dica: procure informações sobre a função elem.-}
consoantes :: String -> String
consoantes cs = [ c | c<-cs, c `notElem` "aeiouAEIOU"]

{-
Exercício 1.6 Gere as listas
usando list compreenshion.
-}

-- 1. ["AaBB", "AbBB", "AcBB", "AdBB", "AeBB", "AfBB", "AgBB"]
l161 :: [String]
l161 = ['A' : x : "BB" | x <- ['a'..'g']]

-- 2. [5,8,11,17,20,26,29,32,38,41]


-- 3. [1.0,0.5,0.25,0.125,0.0625,0.03125]
l163 :: [Float]
l163 = [1 / (2 ^ f)| f <- [0..5]]

{-
Exercício 1.7 Sabe-se que as unidades imperiais de comprimento podem ser
Inch, Yard ou Foot (há outras ignoradas aqui). Sabe-se que 1in = 0.0254m,
1yd = 0.9144m, 1ft = 0.3048. Faça a função converterMetros que recebe a uni
dade imperial e o valor correspondente nesta unidade e retorna o valor em metros.
Faça a função converterImperial que recebe um valor em metros e a unidade de
conversão e retorna o valor convertido para a unidade desejada.
-}
data UnidadeImp = Inches | Yards | Feet
data UnidadeSI = Metros Float deriving Show

converterMetros :: UnidadeImp -> Float -> UnidadeSI
converterMetros Inches i = Metros (i * 0.0254)
converterMetros Yards y = Metros (y * 0.9144)
converterMetros Feet f = Metros (f * 0.3048)

converterImperial :: Float -> UnidadeImp -> Float
converterImperial i Inches = i/0.0254
converterImperial y Yards = y/0.9144
converterImperial f Feet = f/0.3048

{-
Exercício 1.8 Faça um novo tipo chamado Mes que possui como valores todos
os meses do ano. Implemente:
-}

data Mes = Janeiro | Fevereiro | Março | Abril | Maio | Junho | Julho | Agosto | Setembro | Outubro | Novembro | Dezembro deriving Show

{-
1. a função checaFim que retorna o número de dias que cada mês possui (con-
sidere Fevereiro tendo 28 dias).
-}

checaFim :: Mes -> Int
checaFim Fevereiro = 28
checaFim Abril = 30
checaFim Junho = 30
checaFim Setembro = 30
checaFim Novembro = 30
checaFim _ = 31

-- 2. a função prox que recebe um mês atual e retorna o próximo mês.

prox :: Mes -> Mes
prox Janeiro = Fevereiro
prox Fevereiro = Março
prox Março = Abril
prox Abril = Maio
prox Maio = Junho
prox Junho = Julho
prox Julho = Agosto
prox Agosto = Setembro
prox Setembro = Outubro
prox Outubro = Novembro
prox Novembro = Dezembro
prox Dezembro = Janeiro

{-
3. a função estacao que retorna a estacao do ano de acordo com o mes e com
o hemisfério. Use apenas tipos criados pela palavra data aqui.
-}

data Estacao = Verao | Outono | Inverno | Primavera deriving Show
data Hemisferio = Norte | Sul

estacao :: Mes -> Hemisferio -> Estacao

estacao Janeiro Norte = Inverno
estacao Janeiro Sul = Verao
estacao Fevereiro Norte = Inverno
estacao Fevereiro Sul = Verao
estacao Março Norte = Primavera
estacao Março Sul = Outono
estacao Abril Norte = Primavera
estacao Abril Sul = Outono
estacao Maio Norte = Primavera
estacao Maio Sul = Outono
estacao Junho Norte = Verao
estacao Junho Sul = Inverno
estacao Julho Norte = Verao
estacao Julho Sul = Inverno
estacao Agosto Norte = Verao
estacao Agosto Sul = Inverno
estacao Setembro Norte = Outono
estacao Setembro Sul = Primavera
estacao Outubro Norte = Outono
estacao Outubro Sul = Primavera
estacao Novembro Norte = Outono
estacao Novembro Sul = Primavera
estacao Dezembro Norte = Inverno
estacao Dezembro Sul = Verao

{-
Exercício 1.9 Faça uma função que receba uma String e retorne True se esta for
um palíndromo, False caso contrário.
-}
palindromo :: String -> Bool
palindromo str = str == reverse str

{-
Exercício 1.10 Faça uma função que elimine todos os números pares, todos os
ímpares múltiplos de 7 e negativos de uma lista de inteiros passado via parâmetro.
Você deve retorna esta lista em ordem reversa em comparação a do parâmetro.
-}
changeList :: [Int] -> [Int]
changeList xs = [x | x <- reverse xs, mod x 2 > 0, mod x 7 > 0, x > 0]

{-
Exercício 1.11 Faça uma função que recebe três strings x, y e z como parâmetro.
A função retorna uma tupla com três coordenadas contendo a ordem reversa em
cada. A primeira coordenada deve conter string reversa do primeiro parâmetro e
assim por diante.
-}
coord :: (String, String, String) -> (String, String, String)
coord (x ,y ,z)= (reverse x, reverse y, reverse z)

{-
Exercício 1.12 Gere as listas abaixo usando list compreenshions
• [1,10,19,28,37,46,55,64]
• [2,4,8,10,12,16,18,22,24,28,30]
• [’@’,’A’,’C’,’D’,’E’,’G’,’J’,’L’]
-}
l1121 :: [Integer]
l1121 = [1, 10 .. 64]

-- hardcoded
l1122 :: [Integer]
l1122 = [x | x <- [2, 4 .. 30], x /= 6 && x /= 14 && x /= 20 && x /= 26]

--hardcoded
l1123 :: String
l1123 = [c | c <- '@' : ['A'..'L'], c /= 'B', c /= 'F', c /= 'H', c /= 'I', c /= 'K']

{-
Exercício 1.13 Faça uma função, chamada revNum, que receba uma String s e
um Int n. Esta deverá retornar as n primeiras letras em ordem reversa e o restante
em sua ordem normal.
Exemplo:
revN um 4 ”F AT EC” = ”ET AF C”
(1.1)
-}

revNum :: Int -> String -> String
revNum n s = reverse (take n s) ++ drop n s

{-
Exercício 1.14 Crie o tipo de dado Binario que pode ser Zero ou Um. Crie tam-
bém o tipo de dado Funcao que pode ser Soma2, Maior, Menor e Mult2. 
Faça a função aplicar que recebe uma Funcao e dois Binarios seu retorno consiste em
executar a operação desejada.
-}

data Binario = Zero | Um deriving Show
data Funcao = Soma2 | Maior | Menor | Mult2

aplicar :: Funcao -> Binario -> Binario -> Binario
aplicar Soma2 Um Um = Zero
aplicar Soma2 Zero q = q
aplicar Soma2 p Zero = p

aplicar Maior Zero Zero = Zero
aplicar Maior _ _ = Um
 
aplicar Menor Um Um = Um
aplicar Menor _ _ = Zero
 
aplicar Mult2 Zero _ = Zero
aplicar Mult2 _ Zero = Zero
aplicar Mult2 _ _ = Um

{-
Exercício 1.15 Faça uma função, chamada binList, usando list compreeshion
que recebe uma lista de Binarios (ver exercício acima) e retorna outra 
lista com elemento somado Um e convertido para Int.
binList [U m, Zero, Zero, U m, Zero] = [0, 1, 1, 0, 1]
(1.3)
-}

binList :: [Binario] -> [Int]
binList bs = [toInt'(aplicar Soma2 Um b) | b <- bs]  

toInt' :: Binario -> Int
toInt' Zero = 0
toInt' Um = 1