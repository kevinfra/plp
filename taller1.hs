import Test.HUnit
import Data.List

-- Definiciones de tipos

data AB a = Nil | Bin (AB a) a (AB a) deriving Eq

instance Show a => Show (AB a) where
  show t = padAB t 0 0

-- Funciones auxiliares

pad :: Int -> String
pad i = replicate i ' '

padAB :: Show a => AB a -> Int -> Int -> String
padAB = foldAB (const $ const "") (\x ri rd n base ->let l = length $ show x in pad n ++ show x ++ ri 4 (base+l) ++ "\n" ++ rd (n+4+base+l) base)

-- Crea una hoja de un árbol binario AB
abHoja :: a -> AB a
abHoja x = Bin Nil x Nil

-- Devuelve una lista con los elementos de los nodos de un árbol binario AB recorridos en profundidad de izquierda a derecha
inorder :: AB a -> [a]    
inorder = foldAB [] (\r i d -> i ++ (r:d))

-- calcula la altura de un arbol
altura :: AB a -> Int
altura = foldAB 0 (\raiz resIzq resDer -> 1 + max resIzq resDer)

-- calcula la cantidad de nodos
cantNodos :: AB a -> Int
cantNodos = foldAB 0 (\raiz resIzq resDer -> (1 + resIzq) + resDer)

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
-- Heap (<) infinito, probar truncando
ab8 = Bin (mapAB (*2) ab8) 1 (mapAB ((+1) . (*2)) ab8)
-- Heap Gigante
abGigante = Bin (Bin (Bin (abHoja 39) 52 (abHoja 32)) 67 (Bin (abHoja 31) 50 (Nil))) 93 (Bin (abHoja 53) 74 (abHoja 55))

-- Ejercicios

-- TALLER --

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
foldAB2 fNil fBin = recAB (\raiz izq der resIzq resDer -> fBin raiz resIzq resDer) fNil

mapAB :: (a -> b) -> AB a -> AB b
mapAB f = foldAB2 Nil (\raiz resIzq resDer -> Bin resIzq (f raiz) resDer)

nilOCumple :: (a -> a -> Bool) -> a -> AB a -> Bool
nilOCumple fComp elem Nil = True
nilOCumple fComp elem (Bin izq raiz der) = fComp elem raiz

maximoAB :: (Ord a) => AB a -> a
maximoAB t = maximum (inorder t) 

minimoAB :: (Ord a) => AB a -> a
minimoAB t = minimum (inorder t) 

esABB :: Ord a => AB a -> Bool
esABB Nil = True
esABB (Bin izq raiz der) = recAB (\raiz izq der resIzq resDer -> resIzq && resDer && (if izq/=Nil then maximoAB izq <= raiz else True) && (if der/=Nil then minimoAB der > raiz else True) ) True (Bin izq raiz der)


esHeap :: (a -> a -> Bool)  -> AB a -> Bool
esHeap f = recAB (\raiz izq der resIzq resDer -> (nilOCumple f raiz izq) && (nilOCumple f raiz der) && resIzq && resDer) True

completo :: AB a -> Bool
completo arbol = (2 ^ (altura arbol)) == (cantNodos arbol) + 1

insertarABB :: Ord a => AB a -> a -> AB a
insertarABB t elem = recAB (\raiz izq der resIzq resDer -> if elem <= raiz then (Bin resIzq raiz der) else (Bin izq raiz resDer) )  (Bin Nil elem Nil) t


-- Solo inserta en el lado derecho cuando el lado izquierdo esta completo y tiene mas nodos que el lado derecho
insertarHeap :: (a -> a -> Bool) -> AB a -> a -> AB a
insertarHeap f t elem = (recAB (\raiz izq der resIzq resDer -> \x -> 
  if vaALaDerecha izq der
    then (Bin izq (elemORaiz f x raiz) (resDer (notElemORaiz f x raiz)))
    else (Bin (resIzq (notElemORaiz f x raiz)) (elemORaiz f x raiz) der)) (\y -> abHoja y) t) elem
    where 
      elemORaiz f x raiz = if (f x raiz) then x else raiz
      notElemORaiz f x raiz = if (f x raiz) then raiz else x
      vaALaDerecha i d = (completo i) && ((cantNodos i) > (cantNodos d))

truncar :: AB a -> Integer -> AB a
truncar arbol alturaMax = (recAB (\raiz izq der resIzq resDer -> \altura -> 
  if (altura+1) == alturaMax
    then (abHoja raiz)
    else (Bin (resIzq (altura+1)) raiz (resDer (altura+1)))) (\y -> Nil) arbol) 0
        
        
--Ejecución de los tests
main :: IO Counts
main = do runTestTT allTests

allTests = test [
  "ejercicio1" ~: testsEj1,
  "ejercicio2" ~: testsEj2,
  "ejercicio3" ~: testsEj3,
  "ejercicio4" ~: testsEj4,
  "ejercicio5" ~: testsEj5,
  "ejercicio6" ~: testsEj6,
  "ejercicio7" ~: testsEj7
  ]

testsEj1 = test [
  11 ~=? foldAB 0 (\x resizq resder -> (x + resizq) + resder) ab1,
  16 ~=? foldAB2 0 (\x resizq resder -> (x + resizq) + resder) ab2,
  foldAB 0 (\x resizq resder -> (x + resizq) + resder) ab3 ~=? foldAB2 0 (\x resizq resder -> (x + resizq) + resder) ab3
  ]
  
testsEj2 = test [
  14 ~=? foldAB 0 (\x resizq resder -> (x + resizq) + resder) (mapAB (+1) ab1)
  ]

testsEj3 = test [
  True ~=? nilOCumple (<) 1 (Nil::AB Int),
  True ~=? nilOCumple (<) 1 ab1,
  False ~=? nilOCumple (<) 5 ab1
  ]

testsEj4 = test [
  False ~=? esABB ab1,
  False ~=? esABB ab4,
  True ~=? esABB (Nil::AB Int),
  True ~=? esHeap (<) ab1,
  True ~=? esHeap (<) ab2,
  False ~=? esHeap (<) ab3,
  True ~=? esHeap (<) (Nil::AB Int)
  ]

testsEj5 = test [
  True ~=? completo ab1,
  False ~=? completo ab4
  ]

testsEj6 = test [
  True ~=? esHeap (<) (insertarHeap (<) (insertarHeap (<) ab6 3) 1),
  True ~=? esABB (insertarABB (insertarABB ab7 6) 9),
  [39,52,32,93,31,67,50,95,53,74,55] ~=? inorder (insertarHeap (>) abGigante 95)
  ]

testsEj7 = test [
  [8,4,12,2,10,6,14,1,9,5,13,3,11,7,15] ~=? inorder (truncar ab8 4),
  True ~=? esHeap (<) (truncar ab8 5),
  inorder (truncar abGigante 15) ~=? inorder abGigante,
  inorder (truncar abGigante 5) ~=? inorder abGigante,
  inorder (truncar abGigante 4) ~=? inorder abGigante,
  truncar abGigante 4 ~=? abGigante,
  inorder (truncar abGigante 3) ~=? [52,67,50,93,53,74,55],
  inorder (truncar abGigante 2) ~=? [67,93,74],
  inorder (truncar abGigante 1) ~=? [93],
  truncar ab8 1 ~=? abHoja 1
  ]
