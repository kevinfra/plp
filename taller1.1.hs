import Data.List

data AB a = Nil | Bin (AB a) a (AB a) deriving Eq

-- Crea una hoja de un árbol binario AB
abHoja :: a -> AB a
abHoja x = Bin Nil x Nil

-- Estructuras para tests

-- Heap (<) completo
ab1 = Bin (abHoja 4) 2 (abHoja 5)
-- Heap (<) completo
ab2 = Bin (abHoja 6) 3 (abHoja 7)
-- Heap (>) completo
ab3 = Bin (Bin (abHoja 4) 5 (abHoja 2)) 7 (Bin (abHoja 3) 6 (abHoja 1))
-- Heap (<)
ab4 = Bin ab1 1 (abHoja 3)
-- ABB completo
ab5 = Bin (Bin (abHoja 1) 2 (abHoja 3)) 4 (Bin (abHoja 5) 6 (abHoja 7))
-- Heap (<)
ab6 = Bin ab1 0 (abHoja 6)
-- ABB
ab7 = Bin (Bin (abHoja 1) 2 (abHoja 4)) 5 (abHoja 7)


recr :: b -> (a->[a] -> b -> b) -> [a] -> b
recr z f [] = z
recr z f (x:xs) = f x xs (recr z f xs)
-- Devuelve una lista con los elementos de los nodos de un árbol binario AB recorridos en profundidad de izquierda a derecha
inorder :: AB a -> [a]
inorder = foldAB [] (\i r d -> i ++ (r:d))



--recAB :: 
recAB::b-> (b -> a -> b -> AB a -> AB a -> b)-> AB a -> b
recAB fNil fBin t = case t of
    Nil -> fNil 
    Bin t1 a t2 -> fBin (rec t1) a (rec t2) t1 t2
    where rec = recAB fNil fBin

--foldAB 
foldAB::b->(b->a->b->b)-> AB a -> b
foldAB fNil fBin t = case t of
    Nil -> fNil 
    Bin t1 a t2 -> fBin (rec t1) a (rec t2)
    where rec = foldAB fNil fBin

mapAB :: (a -> b) -> AB a -> AB b
mapAB f = foldAB Nil (\izqrec a derrec -> Bin (izqrec) (f a) (derrec))

nilOCumple :: (a -> a -> Bool) -> a -> AB a -> Bool
nilOCumple f a Nil = True 
nilOCumple f a (Bin t1 r t2) = f a r 

esABB :: Ord a => AB a -> Bool
esABB = (\arbol -> recr True (\x xs rec -> if (xs == []) then True else ((x < head xs) && rec)) (inorder arbol)) 
--esABB = recAB True (\izqrec a derrec t1 t2 -> izqrec && derrec && (nilOCumple (<=) a t2) && (nilOCumple (>=) a t1)) ESTO NO ES LO QUE PIDEN

esHeap :: (a -> a -> Bool)  -> AB a -> Bool
esHeap f = recAB True (\izqrec a derrec t1 t2 -> izqrec && derrec && (nilOCumple (f) a t2) && (nilOCumple (f) a t1)) 

completo :: AB a -> Bool
completo a = (2^(altura a) - 1 == (nodos a))

altura:: AB a -> Integer 
altura = foldAB 0 (\i r d-> 1 + max i d)

nodos:: AB a -> Integer 
nodos = foldAB 0 (\i r d -> 1 + i + d)

insertarABB :: Ord a => AB a -> a -> AB a
insertarABB = recAB (abHoja) (\izqrec r derrec t1 t2 ->  \a -> if (a > r) then (if (nilOCumple (<) a t2) then Bin (izqrec r) a t2 else Bin t1 r (derrec a))
  else (if (nilOCumple (>=) a t1) then Bin t1 a (derrec r) else Bin (izqrec a) r t2) )