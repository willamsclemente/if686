encode_rle :: String -> String
encode_rle l = resultado (words (limpaDados l))

limpaDados :: String -> String 
limpaDados [] = []
limpaDados (a:as) | as ==[] = a : limpaDados as
                  | a == (as!!0) = a : limpaDados as
                  | otherwise = a : ' ' : limpaDados as  

resultado :: [String] -> String
resultado [] = []
resultado (a:as) = (show (length a)) + (a!!0) : resultado as				  