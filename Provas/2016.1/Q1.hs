type Chave = [(Char, Char)]
rot13parcial :: Chave -- troca 'a' por 'n', 'b' por 'o' etc.
rot13parcial = [('a','n'),('b','o'),('c','p'),('d','q'),('e','r'),('f','s'),('g','t'),('h','u'),('i','v'),('j','w'),('k','x'),('l','y'), ('m','z')]

cipher :: Chave -> String -> String
cipher _ [] = []
cipher ch (a:as) = (troca ch a) : (cipher ch as)

troca :: Chave -> Char -> Char
troca [] a = a
troca ((x1,x2):xs) a | a == x1 = x2
                     | otherwise = troca xs a