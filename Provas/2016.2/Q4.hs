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

type Dia = String
type Hora = String
type Usuario = String
data LogEntry = Permitido Dia Hora Usuario | Negado Dia Hora Usuario deriving Show

converte :: String -> [LogEntry]
converte l = computa (limpaString l)

computa :: [(String, String, String, String)] -> [LogEntry]
computa [] = []
computa ((x0,x1,x2,x3):xs) | x2 == "Normal" = Permitido x0 x1 x3 : computa xs 
                           | otherwise = Negado x0 x1 x3 : computa xs 

