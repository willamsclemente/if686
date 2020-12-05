data Time = Egito | Russia | Arabia | Uruguai 
          | Ira | Marrocos | Portugal | Espanha deriving (Show, Eq) 

type Jogo = (Time, Int, Int, Time) 

jogos1 :: [Jogo]
jogos1 = [(Egito, 1, 3, Russia), (Arabia, 0, 3, Uruguai), 
          (Egito, 0, 0, Arabia),(Russia, 0, 2, Uruguai), 
          (Russia, 2, 0, Arabia), (Egito, 0, 2, Uruguai), 
          (Ira, 1, 1, Marrocos), (Portugal, 2, 2, Espanha), 
          (Ira, 1, 2, Portugal), (Ira, 0, 1, Espanha), 
          (Marrocos, 0, 3, Portugal), (Marrocos, 1, 1, Espanha)]

--a
gols ::  Time -> [Jogo] -> Int
gols _ [] = 0
gols t ((t1,g1,g2,t2):ts) | t == t1 = g1 + (gols t ts)
                          | t == t2 = g2 + (gols t ts)
						  |otherwise = (gols t ts)
						  
golsFeitos ::  Time -> [Jogo] -> Int
golsFeitos _ [] = 0
golsFeitos t ((t1,g1,g2,t2):ts) | t == t1 = g1 + (golsFeitos t ts)
                                | t == t2 = g2 + (golsFeitos t ts)
						        |otherwise = (golsFeitos t ts)
						  

golsTomados ::  Time -> [Jogo] -> Int
golsTomados _ [] = 0
golsTomados t ((t1,g1,g2,t2):ts) | t == t1 = g2 + (golsTomados t ts)
                                 | t == t2 = g1 + (golsTomados t ts)
					             |otherwise = (golsTomados t ts)
--b
saldo :: Time -> [Jogo] -> Int
saldo t l = (golsFeitos t l) - (golsTomados t l)
--c
pontos :: Time -> [Jogo] -> Int
pontos _ [] = 0
pontos t ((t1,g1,g2,t2):ts) | (t == t1) && (g1 > g2) = 3 + (pontos t ts)
                          | (t == t2) && (g2 > g1) = 3 + (pontos t ts)
						  | ((t == t1) || (t == t2)) && (g1 == g2) = 1 + (pontos t ts)
						  | otherwise = (pontos t ts)
						  
--d
type Grupo = (Char, Time, Time, Time, Time)
								 
								 
			