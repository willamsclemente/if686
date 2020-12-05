locate :: Eq t => t -> [t] -> Int
locate x l = locate1 x l (-1)
				
locate1 :: Eq t => t -> [t] -> Int -> Int
locate1 _ [] y = y
locate1 x (a:as) y | x == a = 0
                   | otherwise = 1 + locate1 x as (y - 1)				
				
l = "abcdewxyz" 
t = [5,98,7,32] 
a =[False, False]				