data Command = Forward Int | Backward Int | TurnLeft | TurnRight 
    deriving (Show, Read, Eq, Ord)

data Direction = North | South | West | East deriving (Show, Read, Eq, Ord) -- tipo auxiliar 
direcao :: Direction
direcao = North


destination :: (Int, Int) -> [Command] -> (Int, Int)
destination (x, y) l = findDirection (x,y, direcao) l



findDirection :: (Int, Int, Direction) -> [Command] -> (Int, Int) -- Função que define a proxima posicao do robo 
findDirection (x, y, dir) [] = (x,y)
findDirection (x, y, dir) (Forward t :ts) | (dir == North) = findDirection (x, y + t, North) ts 
                                          | (dir == South) = findDirection (x, y - t, South) ts
								          | (dir == East) = findDirection (x + t, y, East) ts
										  | otherwise  = findDirection (x - t, y, West) ts

findDirection (x, y, dir) (Backward t :ts)| (dir == North) = findDirection (x, y - t, North) ts 
                                          | (dir == South) = findDirection (x, y + t, South) ts
								          | (dir == East) = findDirection (x - t, y, East) ts
										  | otherwise  = findDirection (x + t, y, West) ts

findDirection (x, y, dir) (TurnLeft :ts)  | (dir == North) = findDirection (x, y, West) ts 
                                          | (dir == South) = findDirection (x, y, East) ts
								          | (dir == East) = findDirection (x, y, North) ts
										  | otherwise  = findDirection (x, y, South) ts

findDirection (x, y, dir) (TurnRight :ts) | (dir == North) = findDirection (x, y, East) ts 
                                          | (dir == South) = findDirection (x, y, West) ts
								          | (dir == East) = findDirection (x, y, South) ts
										  | otherwise  = findDirection (x, y, North) ts
				
				
				
							
							
							