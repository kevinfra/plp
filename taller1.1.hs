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


recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
recr _ z [] = z
recr f z (x:xs)= f x xs (recr f z xs)

recAB :: (a -> AB a -> AB a -> b -> b -> b) -> b -> AB a -> b
recAB _ acum Nil = acum
recAB f acum (Bin izq raiz der) = f raiz izq der (recAB f acum izq) (recAB f acum der)

foldAB :: b -> (a -> b -> b -> b) -> AB a -> b
foldAB fNil fBin (Nil) = fNil
foldAB fNil fBin (Bin izq raiz der) =
  fBin raiz (foldAB fNil fBin izq) (foldAB fNil fBin der)

foldAB2 :: b -> (a -> b -> b -> b) -> AB a -> b
foldAB2 fNil fBin arbol = recAB (\raiz izq der resIzq resDer -> fBin raiz resIzq resDer) fNil arbol

mapAB :: (a -> b) -> AB a -> AB b
mapAB f = (\arbolA -> case arbolA of 
							Nil 				-> Nil
							(Bin izq raiz der)  -> Bin (mapAB f izq) (f raiz) (mapAB f der) )


esABB :: Ord a => AB a -> Bool
esABB arbol = sort (inorder arbol) == (inorder arbol)

nilOCumple :: (a -> a -> Bool) -> a -> AB a -> Bool
nilOCumple fComp elem Nil = True
nilOCumple fComp elem (Bin izq raiz der) = fComp elem raiz

esHeap f arbol = recAB (\raiz izq der resIzq resDer -> (nilOCumple f raiz izq) && (nilOCumple f raiz der) && resIzq && resDer) True arbol

-- Devuelve una lista con los elementos de los nodos de un árbol binario AB recorridos en profundidad de izquierda a derecha
inorder :: AB a -> [a]    
inorder = foldAB [] (\r i d -> i ++ (r:d))