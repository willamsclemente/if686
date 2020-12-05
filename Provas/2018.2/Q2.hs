decole_rle :: String -> String
decole_rle l = resultado (transfInt (pegaTuplas l))

resultado :: [(Int, Char)] -> String
resultado [] = []
resultado ((a0,a1):as) = (replicate a0 a1) ++ resultado as 

pegaTuplas :: String -> [(Char, Char)]
pegaTuplas [] = []
pegaTuplas l = (l!!0, l!!1) : pegaTuplas (drop 2 l)

transfInt :: [(Char, Char)] -> [(Int, Char)]
transfInt []=[]
transfInt ((a0,a1):as) = (charToInt a0, a1) : transfInt as

charToInt :: Char -> Int
charToInt ch = fromEnum ch - fromEnum '0'
