data Tree t = Node t (Tree t) (Tree t) | Leaf t  

testeOrdenado :: Tree Int
testeOrdenado = Node 10 (Node 5 (Leaf 3) (Leaf 6)) (Node 15 (Leaf 14) (Leaf 17))
testeNaoOrdenado :: Tree Int
testeNaoOrdenado = Node 10 (Node 5 (Leaf 3) (Leaf 6)) (Node 15 (Leaf 16) (Leaf 17))
							 
isSortedTree :: Ord t => Tree t -> Bool
isSortedTree (Leaf _) = True
isSortedTree (Node x arvesq arvdir) = (buscaEsq x arvesq) && (buscaDir x arvdir) && (isSortedTree arvesq) && (isSortedTree arvdir)


buscaEsq :: Ord t => t -> Tree t -> Bool
buscaEsq x (Leaf l)               | x >= l = True 
                                  | otherwise = False 

buscaEsq x (Node l arvesq arvdir) | x >= l = True && (buscaEsq x arvesq) && (buscaEsq x arvdir)
                                  | otherwise = False
								  

buscaDir :: Ord t => t -> Tree t -> Bool
buscaDir x (Leaf l)               | x < l = True
                                  | otherwise = False

buscaDir x (Node l arvesq arvdir) | x < l = True && (buscaDir x arvesq) && (buscaDir x arvdir)
                                  | otherwise = False							  
								  

								  


