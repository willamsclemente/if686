substr :: String -> String -> Bool
substr l c = procura l c (tamanho l)

procura :: String -> String -> Int -> Bool
procura _ [] _ = False
procura l (a:as) x | l == take x (a:as) = True
                   | otherwise = procura l as x


tamanho :: String -> Int
tamanho [] = 0
tamanho (a:as) = 1 + tamanho as

l = "xyz12abrt" 
p = "aaabrsabcfr" 
t = "aacrtxxeaayb"