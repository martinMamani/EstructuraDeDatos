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


-- RECURSION SOBRE NUMEROS

-- Defina las siguientes funciones utilizando recursión sobre números enteros, salvo que se indique
-- lo contrario:

-- 1 - Dado un número n se devuelve la multiplicación de este número y todos sus anteriores hasta
-- llegar a 0. Si n es 0 devuelve 1. La función es parcial si n es negativo.
factorial :: Int -> Int
factorial 0 = 1 
factorial n =  n * factorial (n-1)

-- 2 - Dado un número n devuelve una lista cuyos elementos sean los números comprendidos entre
-- n y 1 (incluidos). Si el número es inferior a 1, devuelve la lista vacía.
cuentaRegresiva :: Int -> [Int]
cuentaRegresiva 0 = [0]
cuentaRegresiva n =  n:  cuentaRegresiva (n-1)

-- 3 - Dado un número n y un elemento e devuelve una lista en la que el elemento e repite n veces.
repetir :: Int -> a -> [a]
repetir 0 e = []
repetir n e = e:  repetir (n-1) e

-- 4 - Dados un número n y una lista xs, devuelve una lista con los n primeros elementos de xs.
-- Si la lista es vacía, devuelve una lista vacía.
losPrimeros :: Int -> [a] -> [a]
losPrimeros n [] = []
losPrimeros 0 xs = []
losPrimeros n (x:xs) =  x: losPrimeros (n-1) xs

-- 5 - Dados un número n y una lista xs, devuelve una lista sin los primeros n elementos de lista
-- recibida. Si n es cero, devuelve la lista completa.
sinLosPrimeros :: Int -> [a] -> [a]
sinLosPrimeros 0 xs = xs
sinLosPrimeros n (x:xs)= if(n>0) then sinLosPrimeros (n-1) xs else sinLosPrimeros n xs 


-- REGISTROS

-- 1 - Definir el tipo de dato Persona, como un nombre y la edad de la persona. Realizar las
-- siguientes funciones:
type Nombre = String
type Edad = Int

data Persona =  Persona Nombre  Edad deriving Show 


carla = Persona "carla" 23
jose = Persona "jose" 25
mateo = Persona "mateo" 17

-- A - Dados una edad y una lista de personas devuelve a las personas mayores a esa edad.
mayoresA :: Int -> [Persona] -> [Persona]
mayoresA e [] = [] 
mayoresA e (p:ps) =  if (mayorA e p) then p : mayoresA e ps else mayoresA e ps

mayorA :: Int -> Persona -> Bool
mayorA e1 (Persona _ e2) = e2 > e1

-- B - Dada una lista de personas devuelve el promedio de edad entre esas personas.
--  Precondición: la lista al menos posee una persona.
promedioEdad :: [Persona] -> Int
promedioEdad ps = sumatoriaEdades ps `div` longitud ps

sumatoriaEdades :: [Persona] -> Int
sumatoriaEdades [] = 0 
sumatoriaEdades (p:ps) =  edadDe p  +  sumatoriaEdades ps

edadDe :: Persona -> Int
edadDe (Persona _ e ) = e

-- C - Dada una lista de personas devuelve la persona más vieja de la lista. Precondición: la
-- lista al menos posee una persona.
elMasViejo :: [Persona] -> Persona
elMasViejo (p:[]) = p
elMasViejo (p:ps) = personaMayor p (elMasViejo ps)

personaMayor :: Persona -> Persona -> Persona
personaMayor (Persona n1 e1) (p) = if (mayorA e1 p) then p else Persona n1 e1


-- 2 - Modificaremos la representación de Entreador y Pokemon de la práctica anterior de la 
-- siguiente manera:

data TipoDePokemon = Agua | Fuego | Planta deriving Show
data Pokemon = ConsPokemon TipoDePokemon Int deriving Show
data Entrenador = ConsEntrenador String [Pokemon] deriving Show

bulbasur = ConsPokemon Planta 10

charmander = ConsPokemon Fuego 20 

squirtle = ConsPokemon Agua 30

ash = ConsEntrenador "Ash" [squirtle, charmander,bulbasur]
bruk = ConsEntrenador "Bruk" [squirtle,squirtle]
ohu = ConsEntrenador "Ohu" [bulbasur]

-- Como puede observarse, ahora los entrenadores tienen una cantidad de Pokemon arbitraria.
-- Definir en base a esa representación las siguientes funciones:

-- A - Devuelve la cantidad de Pokémon que posee el entrenador.
cantPokemon :: Entrenador -> Int
cantPokemon (ConsEntrenador _  pks) = longitud pks

-- B - Devuelve la cantidad de Pokémon de determinado tipo que posee el entrenador.
cantPokemonDe :: TipoDePokemon -> Entrenador -> Int
cantPokemonDe tipo (ConsEntrenador _ pks) = cantPokemonDeTipo tipo pks 

cantPokemonDeTipo :: TipoDePokemon -> [Pokemon] -> Int 
cantPokemonDeTipo tipo [] = 0
cantPokemonDeTipo tipo (pk:pks) = 
    if (sonDeIgualTipo tipo pk) 
         then  1 + cantPokemonDeTipo tipo pks 
         else 0 + cantPokemonDeTipo tipo pks

sonDeIgualTipo :: TipoDePokemon -> Pokemon -> Bool 
sonDeIgualTipo tipo (ConsPokemon t e) = esMismoTipo tipo t 
-------------------------------------------------------------------
esMismoTipo :: TipoDePokemon -> TipoDePokemon -> Bool
esMismoTipo Planta Planta = True
esMismoTipo Fuego Fuego = True
esMismoTipo Agua Agua = True 
esMismoTipo _ _ = False
-------------------------------------------------------------------
superaA :: Pokemon -> Pokemon -> Bool
superaA (ConsPokemon tipo1 energia1) (ConsPokemon tipo2 energia2) = superaTipo  tipo1 tipo2

superaTipo :: TipoDePokemon -> TipoDePokemon -> Bool
superaTipo Agua Fuego = True
superaTipo Fuego Planta = True
superaTipo Planta Agua = True
superaTipo _ _ = False
--------------------------------------------------------------------
-- C - Dados dos entrenadores, indica la cantidad de Pokemon de cierto tipo, que le ganarían
-- a los Pokemon del segundo entrenador.
losQueLeGanan :: TipoDePokemon -> Entrenador -> Entrenador -> Int
losQueLeGanan tipo (ConsEntrenador _ pks) e2 = cantPokemonSuperan  (pokemonesDeTipo tipo pks) e2

pokemonesDeTipo :: TipoDePokemon -> [Pokemon] -> [Pokemon]
pokemonesDeTipo tipo [] = []
pokemonesDeTipo tipo (pk:pks) = 
    if(sonDeIgualTipo tipo pk) 
        then pk : pokemonesDeTipo tipo pks
        else pokemonesDeTipo tipo pks 

cantPokemonSuperan :: [Pokemon] -> Entrenador -> Int 
cantPokemonSuperan [] e = 0
cantPokemonSuperan (pk:pks) e = cantPokemonQueSupera pk e + cantPokemonSuperan pks e 

cantPokemonQueSupera :: Pokemon -> Entrenador -> Int
cantPokemonQueSupera p (ConsEntrenador _ pks) = cantPokemonSuperaA p pks 

cantPokemonSuperaA :: Pokemon -> [Pokemon] -> Int
cantPokemonSuperaA p [] = 0
cantPokemonSuperaA p (pk:pks) = 
    if (superaA p pk) 
        then 1 + cantPokemonSuperaA p pks
        else 0 + cantPokemonSuperaA p pks

-- D - Dado un entrenador, devuelve True si posee al menos un Pokémon de cada tipo posible.
esMaestroPokemon :: Entrenador -> Bool
esMaestroPokemon e = maestroTieneAlMenosUnPokemonDeCadaTipo e

maestroTieneAlMenosUnPokemonDeCadaTipo :: Entrenador -> Bool
maestroTieneAlMenosUnPokemonDeCadaTipo e = cantPokemonDe Agua e >= 1 && cantPokemonDe Fuego e >=1 && cantPokemonDe Planta e >=1


-- 3 - El tipo de dato Rol representa los roles (desarollo o management) de empleados IT dentro
-- de una empresa de software, junto al proyecto en el que se encuentran. Así, una empresa es
-- una lista de personas con diferente rol. La definición es la siguiente:

data Seniority = Junior | SemiSenior | Senior
data Proyecto = ConsProyecto String deriving Show
data Rol = Developer Seniority Proyecto | Management Seniority Proyecto 
data Empresa = ConsEmpresa [Rol]

sweetApp = ConsProyecto "sweetApp"
todoList = ConsProyecto "todoList"

mariano = Developer Junior todoList
jorge = Developer SemiSenior todoList
leandro = Management Senior todoList
fernando = Developer Senior todoList
juan = Developer Junior sweetApp
lucas = Management Senior sweetApp

arcor = ConsEmpresa [juan,lucas,fernando,mariano]
ace = ConsEmpresa [mariano,jorge,leandro]

-- Definir las siguientes funciones sobre el tipo Empresa:

-- A - Dada una empresa denota la lista de proyectos en los que trabaja, sin elementos repetidos.
proyectos :: Empresa -> [Proyecto]
proyectos (ConsEmpresa []) = []
proyectos (ConsEmpresa rs) = (cantDeProyectos rs)

sinRepetidos :: Ord a => [a] -> [a]
sinRepetidos [] = []
sinRepetidos (x:[])= [x]
sinRepetidos (x:xs) = if(pertenece x xs ) then sinRepetidos xs else x : sinRepetidos xs  


cantDeProyectos :: [Rol] -> [Proyecto]
cantDeProyectos [] = []
cantDeProyectos (rl:rls) = proyectoDe rl :  cantDeProyectos rls

proyectoDe :: Rol -> Proyecto
proyectoDe (Developer _ p) = p
proyectoDe (Management _ p) = p
-- Funciona todas las expresiones falta sacar los repetidos.

-- B - Dada una empresa indica la cantidad de desarrolladores senior que posee, que pertecen
-- además a los proyectos dados por parámetro.
-- losDevSenior :: Empresa -> [Proyecto] -> Int
-- losDevSenior (ConsEmpresa rls) ps =  desarrolladoresSeniorQueTrabajan rls ps


-- desarrolladoresSeniorQueTrabajan :: [Rol]->[Proyecto]->Int
-- desarrolladoresSeniorQueTrabajan [] ps = 0
-- desarrolladoresSeniorQueTrabajan rls [] = 0
-- desarrolladoresSeniorQueTrabajan (r:rls) ps = 
--     if(trabajanEnAlgunProyecto r ps && esDeRolSenior r)
--         then 1 + desarrolladoresSeniorQueTrabajan rls ps
--         else 0 + desarrolladoresSeniorQueTrabajan rls ps  

trabajanEnAlgunProyecto :: Rol -> [Proyecto] -> Bool
trabajanEnAlgunProyecto rl ps =  pertenece ( proyectoDe rl) ps


esDeRolSenior :: Rol -> Bool
esDeRolSenior (Developer s _ ) = esSenior s s
esDeRolSenior (Management s _ ) = esSenior s s
esDeRolSenior (Developer s  _ ) = False
esDeRolSenior (Management s _ ) = False

esSenior :: Seniority -> Seniority -> Bool
esSenior Senior Senior = True
esSenior _  _ = False
