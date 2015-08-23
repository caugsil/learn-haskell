module Lista1 where
-- Andrei Martins Silva 1210045-7 Noturno

-- Exercicio 1.1
{-
    Crie o tipo Pergunta com os value constructors Sim ou Nao.
    Faca as funcoes abaixo determinando seus tipos explicitamente:
-}

data Pergunta = Nao | Sim

-- 1. pergNum: recebe via parâmetro uma Pergunta e retorne 0 para Nao e 1 para Sim;

pergNum :: Pergunta -> Int
pergNum Nao = 0
pergNum Sim = 1

-- 2. listPergs: recebe via parâmetro uma lista de Perguntas e retorna 0’s e 1’s correspondentes aos constructores contidos na lista;

listPergs :: [Pergunta] -> [Int]
listPergs ps = [pergNum p | p<-ps]

--3. and’: recebe uma Pergunta como parâmetro e retorna a tabela verdade do and lógico usando Sim como verdadeiro e Nao como falso.


--4. or’: Idem acima, porém, deve ser usado o ou lógico.
--5. not’: Idem aos anteriores, porém, usando o not lógico.

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







