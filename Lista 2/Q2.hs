data IntTree = Nilt | Node Int (IntTree) (IntTree) deriving (Show) 

isBST :: IntTree -> Bool -- Função que procura todos nós e manda eles para buscaEsq e buscaDir
isBST Nilt = True
isBST (Node x arvesq arvdir) = (buscaEsq x arvesq) && (buscaDir x arvdir) && isBST (arvesq) && isBST (arvdir)

							 
buscaEsq :: Int -> IntTree -> Bool -- Função que ver se todos os Nós a esquerda de um nó dado são menores que esse nó
buscaEsq _ Nilt = True
buscaEsq t (Node x arvesq arvdir) | t > x = True && (buscaEsq t arvesq) && (buscaEsq t arvdir) 
                                  | otherwise = False
							 
buscaDir :: Int -> IntTree -> Bool -- Função que ver se todos os Nós a direita de um nó dado são maiores que esse nó
buscaDir _ Nilt = True
buscaDir t (Node x arvesq arvdir) | t < x = True && (buscaDir t arvesq) && (buscaDir t arvdir) 
                                  | otherwise = False
								  
