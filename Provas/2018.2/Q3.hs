type Dicionario = [(Int, String)]

meuDicionario :: Dicionario
meuDicionario = [(1, "casa"), (3, "cafe"), (4, "teria"), (6, "era"), (7, "uma")]

teste :: String 
teste = "a 1 tinha 3 mas nao 6 7 34, a 1 6 7 sorve4"


decode :: Dicionario -> String -> String
decode _ [] = []
decode di (a:as) | (1 <= (charToInt a)) && ((charToInt a) <=9) = (troca di (charToInt a)) ++ decode di as
                 | otherwise = a : decode di as 
				 
troca :: Dicionario -> Int -> String
troca [] _ = []
troca ((n,p):as) a | a == n = p
                   | otherwise = troca as a
				 
charToInt :: Char -> Int
charToInt ch = fromEnum ch - fromEnum '0'
