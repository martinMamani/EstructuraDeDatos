-- NUMEROS ENTEROS

-- 1. Defina las siguientes funciones:

--  A - Dado un numero devuelve su sucesor
sucesor :: Int -> Int
sucesor x = x+1

-- B - Dados dos números devuelve su suma utilizando la operación +.
sumar :: Int -> Int -> Int
sumar n m = n + m

-- C - Dado dos números, devuelve un par donde la primera componente es la división del
-- primero por el segundo, y la segunda componente es el resto de dicha división. Nota:
-- para obtener el resto de la división utilizar la función mod :: Int -> Int -> Int,
-- provista por Haskell.
divisionYResto :: Int -> Int -> (Int,Int)
divisionYResto n m = (n `div` m , mod n m)

-- Dado un par de números devuelve el mayor de estos.
maxDelPar :: (Int,Int)->Int
maxDelPar (n,m) = if (n>m)
                  then n
                   else m

--2. De 4 ejemplos de expresiones diferentes que denoten el número 10, utilizando en cada expresión a todas las funciones del punto anterior.
--Ejemplo: maxDePar (divisionYResto (suma 5 5) (sucesor 0))

-- 1  sucesor(maxDelPar(divisionYResto (sumar 5 4) 1)) = 10 
-- 2  maxDelPar(divisionYResto (sumar (sucesor 4) 5) 1) = 10 
-- 3  sucesor (sumar (maxDelPar(divisionYResto 5 1)) 4) = 10 
-- 4  sumar (maxDelPar(divisionYResto 5 1)) (sucesor 4) = 10 

