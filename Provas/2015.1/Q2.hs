type Resultado = [Int]
type Jogos = [[Int]]

l :: Resultado
l = [1,3,4,5,7,9]

t :: Jogos
t = [[2,3,6,7,8,9],[1,2,3,4,5,6],[0,1,2,4,8,9],[1,3,4,5,7,9]]
     
premiados :: Resultado -> Jogos -> Int
premiados _ [] = 0
premiados res (x:xs) = (computaSena res res x) + premiados res xs

computaSena :: Resultado -> Resultado -> [Int] -> Int
computaSena _ [] _= 0
computaSena _ _ [] = 1
computaSena l (x:xs) (a:as) | x == a = computaSena l l as
                            | otherwise = computaSena l xs (a:as)
					  
acertos :: Resultado -> Jogos -> [Int]
acertos _ [] = []
acertos res (x:xs) = (computaAcertos res res x) : acertos res xs

computaAcertos :: Resultado -> Resultado -> [Int] -> Int
computaAcertos _ _ [] = 0
computaAcertos l [] (a:as) =  computaAcertos l l as
computaAcertos l (x:xs) (a:as) | x == a = 1  + (computaAcertos l l as)
                               | otherwise = (computaAcertos l xs (a:as))
							   
numPremios :: Resultado -> Jogos -> (Int, Int, Int)
numPremios res jog = (fim res jog (0,0,0))

fim :: Resultado -> Jogos -> (Int, Int, Int) -> (Int, Int, Int)
fim _ [] (x4, x5, x6) = (x4, x5, x6)
fim res (x:xs) (x4, x5, x6) | (computaAcertos res res x) == 4 = fim res xs (x4+1, x5, x6)
                            | (computaAcertos res res x) == 5 = fim res xs (x4, x5+1, x6)	
                            | (computaAcertos res res x) == 6 = fim res xs (x4, x5, x6+1)
                            | otherwise = fim res xs (x4, x5, x6)							