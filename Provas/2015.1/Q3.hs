data Instrucao = PUSH Int | POP | ADD | SUB | DUP  deriving (Show, Read, Eq, Ord)


type Pilha = [Int]
t = [PUSH 3, PUSH 5, DUP, ADD, SUB]
evalI :: Instrucao -> Pilha -> Pilha
evalI (PUSH a) [] = a : []
evalI (PUSH a) (x:xs) = a : x : [] ++ xs 
evalI oper (x:xs)| oper == ADD = funAdd x xs
                 | oper == SUB = funSub x xs
				 | oper == DUP = x : x : [] ++ xs
				 | otherwise = tail (x:xs)


funAdd :: Int -> [Int] -> [Int]
funAdd x0 (x1:xs) = (x0 + x1) : [] ++ xs

funSub :: Int -> [Int] -> [Int]
funSub x0 (x1:xs) = (x0 - x1) : [] ++ xs

evalProg :: [Instrucao] -> Pilha
evalProg l = (funEval l [])

funEval :: [Instrucao] -> Pilha -> Pilha
funEval [] l = l
funEval (a:as) l = (funEval as (evalI a l))


