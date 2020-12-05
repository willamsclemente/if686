data Time = Egito | Russia | Arabia | Uruguai |
            Ira | Marrocos | Portugal | Espanha
            deriving (Show, Read, Eq, Ord)
			
type Grupo = (Char, Time, Time, Time, Time)

type Jogo = (Time, Int, Int, Time)

		  
type Tabela = (Time, Int, Int, Int, Int) -- Variavel auxiliar 
tabela :: [Tabela]
tabela = []

		  
classificados :: Grupo -> [Jogo] -> (Time, Time)
classificados g j =  primeirosLugares (drop 2 (qSortTuple (criarTabela g (limpaJogos g j) tabela)))

limpaJogos :: Grupo -> [Jogo] -> [Jogo] -- Limpa a lista de jogos recebido, deixando só os de interesse 
limpaJogos _ [] = []
limpaJogos (a, t1, t2, t3, t4) ((x1, x2 , x3, x4):xs) | (x1 == t1) || (x1 == t2) || (x1 == t3) || (x1 == t4) = (x1, x2, x3, x4) : limpaJogos (a, t1, t2, t3, t4) xs
                                                      | otherwise = limpaJogos (a, t1, t2, t3, t4) xs
													  
criarTabela :: Grupo -> [Jogo] -> [Tabela] -> [Tabela] -- Funcao que cria a tabela resultante
criarTabela g j t =   (addPontuacao j (addTime g t)) 

addTime :: Grupo -> [Tabela] -> [Tabela] -- Zerando a tabela
addTime (a, t1, t2, t3, t4) [] = [(t1, 0, 0, 0, 0), 
                                  (t2, 0, 0, 0, 0), 
								  (t3, 0, 0, 0, 0), 
								  (t4, 0, 0, 0, 0)]

addPontuacao :: [Jogo] -> [Tabela] -> [Tabela] -- Função que adiciona a pontução de cada time
addPontuacao [] t = t
addPontuacao ((t1, g1 , g2, t2):xs) t = addPontuacao xs (inserePonto2 t2 g1 g2 (inserePonto1 t1 g1 g2 t))

inserePonto1 :: Time -> Int -> Int -> [Tabela] -> [Tabela] -- Função que insere pontos do time mandante 
inserePonto1 _ _ _ [] = []
inserePonto1 t1 g1 g2 ((t, p, s, gf, gt):ts) | (t1 == t) && (g1 > g2) = (t, (p + 3), (s + g1 - g2), (gf + g1), (gt +g2)):ts
                                             | (t1 == t) && (g1 == g2) = (t, (p + 1), (s + g1 - g2), (gf + g1), (gt +g2)):ts
											 | (t1 == t) && (g1 < g2) = (t, p, (s + g1 - g2), (gf + g1), (gt +g2)) :ts
											 | otherwise = (t, p, s, gf, gt) : inserePonto1 t1 g1 g2 ts
											 

inserePonto2 :: Time -> Int -> Int -> [Tabela] -> [Tabela] -- Função que insere pontos do time visitante
inserePonto2 _ _ _ [] = []
inserePonto2 t2 g1 g2 ((t, p, s, gf, gt):ts) | (t2 == t) && (g2 > g1) = (t, (p + 3), (s + g2 - g1), (gf + g2), (gt +g1)):ts
                                             | (t2 == t) && (g2 == g1) = (t, (p + 1), (s + g2 - g1), (gf + g2), (gt +g1)):ts
											 | (t2 == t) && (g2 < g1) = (t, p, (s + g2 - g1), (gf + g2), (gt +g1)):ts
											 | otherwise = (t, p, s, gf, gt) : inserePonto2 t2 g1 g2 ts											 

qSortTuple :: [Tabela] -> [Tabela] -- Função que ordena a tabela, segundo o que foi especificado na questão
qSortTuple [] = []
qSortTuple ((t, p, s, gf, gt):ts) = qSortTuple [(t, y, s, gf, gt) | (t, y, s, gf, gt) <- ts, y < p] ++ qSortTuple [(t, y, x, gf, gt) | (t, y, x, gf, gt) <- ts, y == p, x < s] ++ qSortTuple [(t, y, x, z, gt) | (t, y, x, z, gt) <- ts, y == p, x == s, z < gf] ++ [(t, p, s, gf, gt)] ++ qSortTuple [(t, y, x, z, gt) | (t, y, x, z, gt) <- ts, y == p, x == s, z > gf] ++ qSortTuple [(t, y, x, gf, gt) | (t, y, x, gf, gt) <- ts, y == p, x > s] ++ qSortTuple [(t, y, s, gf, gt) | (t, y, s, gf, gt) <- ts, y > p]	


primeirosLugares :: [Tabela] -> (Time, Time) -- Função que retorna os dois classificados 
primeirosLugares [(t2, p2, s2, gf2, gt2), (t1, p1, s1, gf1, gt1)] = (t1, t2)
						  