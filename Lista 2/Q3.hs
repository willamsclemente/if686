data List t = Nil | Cons t (List t) deriving (Show)

mapList :: (t -> t) -> List t -> List t -- Função que usa map na Lista
mapList _ Nil = Nil
mapList f (Cons x xs) = Cons (f x) (mapList f xs)
