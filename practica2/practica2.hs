-- RECURSION SOBRE LISTAS 

-- Defina las siguientes funciones utilizando recursión estructural sobre listas, salvo que se 
--indique lo contrario:

-- A - Dada una lista de enteros devuelve la suma de todos sus elementos.
sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (x:xs)= x + sumatoria xs

-- B - Dada una lista de elementos de algún tipo devuelve el largo de esa lista, es decir, la 
-- cantidad de elementos que posee.
longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

-- C - Dada una lista de enteros, devuelve la lista de los sucesores de cada entero.
sucesores :: [Int] -> [Int]
sucesores [] = error "No hay sucesor de lista vacia"
sucesores (x:[]) = [x+1]
sucesores (x:xs) = x+1 : sucesores xs 

-- D - Dada una lista de booleanos devuelve True si todos sus elementos son True.
conjuncion :: [Bool] -> Bool
conjuncion   []   = False
conjuncion (x:xs) =  x && conjuncion xs 

-- E - Dada una lista de booleanos devuelve True si alguno de sus elementos es True.
disyuncion :: [Bool] -> Bool
disyuncion   []   = False
disyuncion (x:xs) =  x || disyuncion xs 

-- F - Dada una lista de listas, devuelve una única lista con todos sus elementos.
aplanar :: [[a]] -> [a]
aplanar [] = []
aplanar (x:xs) = x ++ aplanar xs

-- G - Dados un elemento e y una lista xs devuelve True si existe un elemento 
-- en xs que sea igual a e.
pertenece :: Eq a => a -> [a] -> Bool
pertenece e [] = False
pertenece e (x:xs)=  e == x || pertenece e xs

-- H - Dados un elemento e y una lista xs cuenta la cantidad de apariciones de e en xs.
apariciones :: Eq a => a -> [a] -> Int
apariciones e []= 0
apariciones e (x:xs)=  if(e == x) then 1 +  apariciones e xs else apariciones e xs


-- I - Dados un número n y una lista xs, devuelve todos los elementos de xs que son menores a n.
losMenoresA :: Int -> [Int] -> [Int]
losMenoresA n [] = []
losMenoresA n (x:xs)=  if(x<n) then  x :  losMenoresA n xs else losMenoresA n xs

-- J - Dados un número n y una lista de listas, devuelve la lista de aquellas listas que tienen más
--de n elementos.
lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]
lasDeLongitudMayorA n [] = []
lasDeLongitudMayorA n (x:xs) =  if((longitud x) > n) then  x :  lasDeLongitudMayorA n xs else lasDeLongitudMayorA n xs

-- K - Dados una lista y un elemento, devuelve una lista con ese elemento agregado al final de la
--lista.
agregarAlFinal :: [a] -> a -> [a]
agregarAlFinal xs e = xs ++ [e]


-- M - Dadas dos listas devuelve la lista con todos los elementos de la primera lista y todos los
-- elementos de la segunda a continuación. Definida en Haskell como ++.
concatenar :: [a] -> [a] -> [a]
concatenar l [] = l
concatenar [] l = l
concatenar (l:ls) xs =   l :  concatenar ls xs  


-- N - Dada una lista devuelve la lista con los mismos elementos de atrás para adelante. Definida
--en Haskell como reverse.
reversa :: [a] -> [a]
reversa [] = []
reversa (x:xs)=  agregarAlFinal (reversa xs ) x

-- Ñ - Dadas dos listas de enteros, devuelve una lista donde el elemento en la posición n es el
--máximo entre el elemento n de la primera lista y de la segunda lista, teniendo en cuenta que
--las listas no necesariamente tienen la misma longitud.
zipMaximos :: [Int] -> [Int] -> [Int]
zipMaximos [] xs = xs
zipMaximos xs [] = xs
zipMaximos (l:ls) (x:xs) =  max l x :   zipMaximos ls xs 


-- -- O - Dada una lista devuelve el mínimo 
elMinimo :: Ord a => [a] -> a
elMinimo [] = error "No hay minimo de una lista vacia"
elMinimo (x:[]) = x
elMinimo (x:xs)=  min x (elMinimo xs)

