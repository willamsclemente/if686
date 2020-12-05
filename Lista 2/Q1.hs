data Ops = SUM | SUB | MUL deriving (Show, Read, Eq, Ord)  
data IntTree = Nilt Int | Node Ops IntTree IntTree deriving (Show, Read, Eq, Ord)

evalTree :: IntTree -> Int -- Função que procura os Nilts e envia eles para a função oper
evalTree (Node op (Nilt x) (Nilt y)) = oper op x y
evalTree (Node op (Nilt x) arvesq) = oper op x (evalTree arvesq)
evalTree (Node op arvdir (Nilt y)) = oper op (evalTree arvdir) y
evalTree (Node op arvdir arvesq) = oper op (evalTree arvdir) (evalTree arvesq)

oper :: Ops -> Int -> Int -> Int -- Função que realiza a operação nos nós dependendo do Ops
oper op x y | op == SUM = (x+y)
            | op == SUB = (x-y)
			| otherwise = (x*y)

   