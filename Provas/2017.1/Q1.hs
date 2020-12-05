isSorted :: Ord t => [t] -> Bool
isSorted [] = True
isSorted (x:xs) =  (compara x xs) && (isSorted xs)

compara :: Ord t => t -> [t] -> Bool
compara _ [] = True
compara x (a:as) | x < a = True && compara x as
                 | otherwise = False