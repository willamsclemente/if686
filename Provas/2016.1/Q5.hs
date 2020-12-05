data KeyTree = Node Char Char KeyTree KeyTree | Empty deriving (Show, Read, Eq, Ord)
chaveParcial :: KeyTree
chaveParcial = Node 'h' 'u' (Node 'c' 'p' (Node 'b' 'o' (Node 'a' 'n' Empty Empty) Empty) (Node 'e' 'r' Empty Empty))
                            (Node 'l' 'y' Empty (Node 'm' 'z' Empty Empty)) 

cipherT :: KeyTree -> String -> String
cipherT _ [] = []
cipherT arv (x:xs) = troca arv x : cipherT arv xs

troca :: KeyTree -> Char -> Char
troca Empty a = a
troca (Node x1 x2 arvesq arvdir) a | a == x1 = x2
                                   | a < x1 = troca arvesq a
								   | otherwise = troca arvdir a
