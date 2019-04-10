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
padAB = foldAB (const $ const "") (\ri x rd n base ->let l = length $ show x in pad n ++ show x ++ ri 4 (base+l) ++ "\n" ++ rd (n+4+base+l) base)

-- Crea una hoja de un árbol binario AB
abHoja :: a -> AB a
abHoja x = Bin Nil x Nil

-- Devuelve una lista con los elementos de los nodos de un árbol binario AB recorridos en profundidad de izquierda a derecha
inorder :: AB a -> [a]    
inorder = foldAB [] (\r i d -> i ++ (r:d))

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
foldAB2 fNil fBin arbol = recAB (\raiz izq der resIzq resDer -> fBin raiz resIzq resDer) fNil arbol

mapAB :: (a -> b) -> AB a -> AB b
mapAB f = (\arbolA -> case arbolA of Nil -> Nil
                      (Bin izq raiz der) -> Bin (mapAB f izq) f raiz (mapAB f der) )

nilOCumple :: (a -> a -> Bool) -> a -> AB a -> Bool
nilOCumple fComp elem Nil = True
nilOCumple fComp elem (Bin izq raiz der) = fComp elem raiz

esABB :: Ord a => AB a -> Bool
esABB arbol = sort (inorder arbol) == (inorder arbol)

esHeap :: (a -> a -> Bool)  -> AB a -> Bool
esHeap f arbol = recAB (\raiz izq der resIzq resDer -> (nilOCumple f raiz izq) && (nilOCumple f raiz der) && resIzq && resDer) True arbol

completo :: AB a -> Bool
completo = undefined

insertarABB :: Ord a => AB a -> a -> AB a
insertarABB = undefined

-- Solo inserta en el lado derecho cuando el lado izquierdo esta completo y tiene mas nodos que el lado derecho
insertarHeap :: (a -> a -> Bool) -> AB a -> a -> AB a
insertarHeap undefined 

truncar :: AB a -> Integer -> AB a
truncar = undefined

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
  foldAB 0 (\x resizq resder -> (x + resizq) + resder) ab1 ~=? 11,
  foldAB2 0 (\x resizq resder -> (x + resizq) + resder) ab2 ~=? 16,
  foldAB 0 (\x resizq resder -> (x + resizq) + resder) ab3 ~=? foldAB2 0 (\x resizq resder -> (x + resizq) + resder) ab3
  ]
  
testsEj2 = test [
  foldAB 0 (\x resizq resder -> (x + resizq) + resder) (mapAB (+1) ab1) ~=? 14
  ]

testsEj3 = test [
  nilOCumple (<) 1 Nil ~=? True
  nilOCumple (<) 1 ab1 ~=? True
  nilOCumple (<) 5 ab1 ~=? False
  ]

testsEj4 = test [
  esABB ab1 ~=? False
  esABB ab4 ~=? False
  esABB Nil ~=? False
  esHeap (<) ab1 ~=? True
  esHeap (<) ab2 ~=? True
  esHeap (<) ab3 ~=? False
  esHeap (<) Nil ~=? True
  ]

testsEj5 = test [
  0 ~=? 0 --Cambiar esto por tests verdaderos.
  ]

testsEj6 = test [
  True ~=? esHeap (<) (insertarHeap (<) (insertarHeap (<) ab6 3) 1),
  True ~=? esABB (insertarABB (insertarABB ab7 6) 9)
  ]

testsEj7 = test [
  [8,4,12,2,10,6,14,1,9,5,13,3,11,7,15] ~=? inorder (truncar ab8 4),
  True ~=? esHeap (<) (truncar ab8 5)
  ]
