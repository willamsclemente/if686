logSetembro :: String
logSetembro = "2016-09-27;19:31:52;Normal;208772;\n2016-09-27;18:12:02;Normal;155759;\n2016-09-26;17:12:02;Normal;155759;\n2016-09-26;16:11:02;Denied;188758;\n2016-09-25;19:12:02;Normal;155759;"

limpaString :: String -> [(String, String, String, String)]
limpaString l = fimLimpa (words (limpaBarraN l))

limpaBarraN :: String -> String
limpaBarraN [] = []
limpaBarraN (a:as) | (a == '\n') = limpaBarraN as
                   | (a == ';') = ' ' : limpaBarraN as 
                   | otherwise = a: limpaBarraN as 
				   
fimLimpa :: [String] -> [(String, String, String, String)]
fimLimpa [] = []
fimLimpa l = (l!!0, l!!1, l!!2, l!!3) : fimLimpa (drop 4 l)


acessosPorUsuario :: String -> [(Int, Int)]
acessosPorUsuario l = busca (limpaString l) [] 

busca :: [(String, String, String, String)] -> [(Int, Int)] -> [(Int, Int)]
busca [] l = l
busca ((x0,x1,x2,x3):xs) l = busca xs (computa (read x3) l)

computa :: Int -> [(Int, Int)] -> [(Int, Int)]
computa usuario [] = [(usuario, 1)]
computa usuario ((a0,a1):as) | usuario == a0 = ((a0,a1+1):as)
                             | otherwise = [(a0,a1)] ++ computa usuario as 