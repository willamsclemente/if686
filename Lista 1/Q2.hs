logCartao = "14 JAN;Amazon;40.32;15 JAN;Uber;14.84;25 JAN;Uber;34.24;02 FEV;Spotify;8.50;06 FEV;Uber;6.94;05 MAR;Burger;29.90;10 MAR;Burger;24.99;15 MAR;UCI;19.00;08 ABR;Itunes;3.50;13 ABR;Picpay;20.00;"

minMaxCartao :: String -> (Double, Double)
minMaxCartao fat = minMax (map read (tiraVazia (valores (padrao1 (padrao (limpaLetra fat) 0) 0) []))) 
--As Funções limpaLetra, padrao e padrao1, modificam a string de entrada para conter só Valores e ';'
limpaLetra :: String -> String
limpaLetra [] = []
limpaLetra (a:as) | (a < '.') || (a > ';') = limpaLetra as
                  | otherwise = a: limpaLetra as

padrao :: String -> Int -> String
padrao [] _ = []
padrao (a:as) x | length as <= 2 = a: padrao as (x+1)
                | ((as !!1) == ';') && ((as !!2) == ';') = padrao as (x+1)
                | otherwise = a : padrao as (x+1)
				

padrao1 :: String -> Int -> String
padrao1 [] _ = []
padrao1 (a:as) x | length as <= 2 = a: padrao1 as (x+1)
                 | ((as !!0) == ';') && ((as !!1) == ';') = padrao1 as (x+1)
                 | otherwise = a : padrao1 as (x+1)
--Valores e cont retira os ';' da string deixando ela pronta para tirar o min e o max				 
valores :: String -> [String] -> [String]
valores [] l  = []:l
valores (a:as) l | length as <= 4 = valores as l
                 | (a == ';') && ((as !! 0) /= ';') =  valores as ((take (cont as 0) as) : l)
                 | otherwise =  valores as l

cont :: String -> Int -> Int
cont b x | (b !! x) == ';' = x
         | otherwise = cont b (x + 1)
--Tira Vazia retira as listas vazias da string
tiraVazia :: [String] -> [String]
tiraVazia [] = []
tiraVazia (a:as) | a == "" = tiraVazia as
                 |otherwise = a : tiraVazia as
--minMax tira os elementos minimo e maximos da lista e retornam eles		 
minMax :: [Double] -> (Double,Double)
minMax l = (minimum l, maximum l)			   