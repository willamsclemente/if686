data Command = Forward Int | Backward Int | TurnLeft |  TurnRight deriving (Show, Read, Eq, Ord)
data Direction = North | South | West | East deriving (Show, Read, Eq, Ord)


faces ::  Direction -> [Command] -> Direction
faces r [] = r
faces r (x:xs) | (x == TurnLeft) && (r == North) = faces West xs
               | (x == TurnLeft) && (r == West) = faces South xs
               | (x == TurnLeft) && (r == South) = faces East xs
               | (x == TurnLeft) && (r == East) = faces North xs
               | (x == TurnRight) && (r == North) = faces East xs
               | (x == TurnRight) && (r == East) = faces South xs
               | (x == TurnRight) && (r == South) = faces West xs
               | (x == TurnRight) && (r == West) = faces North xs	
               | otherwise = faces r xs

t = [Forward 2, TurnLeft, TurnLeft, Forward 1]
x = [Backward 2, Forward 1]
y = [TurnLeft, TurnLeft, TurnLeft]
h = North			  