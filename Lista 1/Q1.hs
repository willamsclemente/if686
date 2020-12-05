logCartao = "14 JAN;Amazon;40.32;15 JAN;Uber;14.84;25 JAN;Uber;34.24;02 FEV;Spotify;8.50;06 FEV;Uber;6.94;05 MAR;Burger;29.90;10 MAR;Burger;24.99;15 MAR;UCI;19.00;08 ABR;Itunes;3.50;13 ABR;Picpay;20.00;"

logMes :: String -> String -> Double 
logMes fat mes = sum (map read (tiraVazia (tiraMes (tiraVazia (valores (';' : limpaDados3 (limpaDados2 (limpaDados1 (limpaDados fat)))) [] mes)) [])))

-- As Funções "LimpaDados" modificam a String de entrada, para a forma "MÊS:VALOR"
limpaDados :: String -> String  
limpaDados [] = []
limpaDados (a:as) | length as <= 4 = a: limpaDados as
                  | (a == ';') && ((as !! 0 < '.') || (as !! 0  > '9'))=   (limpaDados (tiraLoja as))
                  | otherwise = a: limpaDados as 
--Retira as Compras da String
tiraLoja :: String -> String 
tiraLoja (a:as) | (as !! 0) == ';' = as
                |otherwise = tiraLoja as
				
limpaDados1 :: String -> String
limpaDados1 [] = []
limpaDados1 (a:as) | length as <= 4 = a : limpaDados1 as 
                   | (as !!1) == ' ' = limpaDados1 as
                   | otherwise = a : limpaDados1 as				  

limpaDados2 :: String -> String
limpaDados2 [] = []
limpaDados2 (a:as) | length as <= 4 = a : limpaDados2 as 
                   | (as !!0) == ' ' = limpaDados2 as
                   | otherwise = a : limpaDados2 as

limpaDados3 :: String -> String
limpaDados3 [] = []
limpaDados3 (a:as) | a == ' ' = limpaDados3 as
                   | otherwise = a : limpaDados3 as
--As função valores e cont retirão os Mês:Valor no qual o mês é igual ao mês especificado na entrada e colocam em uma lista				   
valores :: String -> [String] -> String -> [String]
valores [] l mes = []:l
valores (a:as) l mes | length as <= 4 = valores as l mes
                     | take 3 as == mes =  valores as ((take (cont as 0) as) : l) mes
				     | otherwise =  valores as l mes

cont :: String -> Int -> Int
cont b x | ((b !! x) == ';') && ((b!!(x+1)) >= 'A' && (b!!(x+1))<= 'Z') = x
         | otherwise = cont b (x + 1)	
--tiraVazia retira as listas vazias que contem dentro do resultado
tiraVazia :: [String] -> [String]
tiraVazia [] = []
tiraVazia (a:as) | a == "" = tiraVazia as
                 |otherwise = a : tiraVazia as
--tiraMes pra finalizar tira o mês de Mês:Valor, deixando só valor para ser transformado em Double e somado			 
tiraMes :: [String] -> [String] -> [String]
tiraMes [] l  = []:l
tiraMes (a:as) l =  drop 4 a : tiraMes as l 

