bSort :: Ord t => [t] -> [t]
bSort l =  ordena l 0

ordena :: Ord t => [t] -> Int -> [t]
ordena l pos | (l !! pos) > (l !! (pos+1))  = muda l pos 0

muda :: Ord t => [t] -> Int -> t -> [t]
muda l pos aux =
 