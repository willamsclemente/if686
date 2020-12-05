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


tiposDeAcesso :: String -> (Int, Int)
tiposDeAcesso l = busca (limpaString l) (0,0)

busca :: [(String, String, String, String)] -> (Int, Int) -> (Int, Int)
busca [] (n, d) = (n,d)
busca ((x0,x1,x2,x3):xs) (n, d) | x2 == "Normal" = busca xs (n+1, d)
                                | otherwise = busca xs (n, d+1)
