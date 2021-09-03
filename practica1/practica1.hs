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

-- TIPOS ENUMERATIVOS

-- 1. Definir el tipo de dato Dir, con las alternativas Norte, Sur, Este y Oeste. Luego implementar
--las siguientes funciones:

data Dir = Este | Oeste | Sur | Norte deriving Show 

-- A - Dada una dirección devuelve su opuesta.
opuesto :: Dir -> Dir
opuesto Este = Oeste
opuesto Oeste = Este
opuesto Sur = Norte
opuesto Norte = Sur

-- B - Dadas dos direcciones, indica si son la misma. Nota: utilizar pattern matching y no ==.
iguales :: Dir -> Dir -> Bool
iguales Este Este = True
iguales Oeste Oeste = True
iguales Sur Sur = True
iguales Norte Norte = True
iguales _ _ = False

-- C - Dada una dirección devuelve su siguiente, en sentido horario, y suponiendo que no existe
--la siguiente dirección a Oeste. ¿Posee una precondición esta función? ¿Es una función
--total o parcial? ¿Por qué?
siguiente :: Dir -> Dir 
siguiente Norte = Este
siguiente Este = Sur
siguiente Sur = Oeste 

-- Esta funcion posee una precondicion por que no se puede hacer siguiente de Oeste. 
-- Es una funcion parcial , por que nos falta cubrir el caso de siguiente de Oeste.


--2. Definir el tipo de dato DiaDeSemana, con las alternativas Lunes, Martes, Miércoles, Jueves,
--Viernes, Sabado y Domingo. Supongamos que el primer día de la semana es lunes, y el último
--es domingo. Luego implementar las siguientes funciones:


data DiaDeSemana = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo deriving Show

-- A - Devuelve un par donde la primera componente es el primer día de la semana, y la
-- segunda componente es el último día de la semana.
primeroYUltimoDia :: (DiaDeSemana, DiaDeSemana)
primeroYUltimoDia = (Lunes , Domingo)

-- B - Dado un dia de la semana indica si comienza con la letra M.
empiezaConM :: DiaDeSemana -> Bool
empiezaConM Martes = True
empiezaConM Miercoles = True
empiezaConM _ = False

-- C - Dado dos dias de semana, indica si el primero viene después que el segundo.

vieneDespues :: DiaDeSemana -> DiaDeSemana -> Bool
vieneDespues Domingo Domingo = False
vieneDespues Domingo _ = True
vieneDespues Sabado Sabado = False
vieneDespues Sabado Domingo = False
vieneDespues Sabado _ = True
vieneDespues Viernes Viernes = False
vieneDespues Viernes Domingo = False
vieneDespues Viernes Sabado = False
vieneDespues Viernes _ = True
vieneDespues Jueves Jueves = False
vieneDespues Jueves Domingo = False
vieneDespues Jueves Sabado = False
vieneDespues Jueves Viernes = False
vieneDespues Jueves _ = True
vieneDespues Miercoles Miercoles = False
vieneDespues Miercoles Domingo = False
vieneDespues Miercoles Sabado = False
vieneDespues Miercoles Viernes = False
vieneDespues Miercoles Jueves = False
vieneDespues Miercoles _ = True
vieneDespues Martes Martes = False
vieneDespues Martes Domingo = False
vieneDespues Martes Sabado = False
vieneDespues Martes Viernes = False
vieneDespues Martes Jueves = False
vieneDespues Martes Miercoles = False
vieneDespues Martes _ = True
vieneDespues Lunes Lunes = False 
vieneDespues Lunes Domingo = True
vieneDespues Lunes Sabado = False
vieneDespues Lunes Viernes = False
vieneDespues Lunes Jueves = False
vieneDespues Lunes Miercoles = False
vieneDespues Lunes Martes = False

-- D - Dado un dia de la semana indica si no es ni el primer ni el ultimo dia.
estaEnElMedio :: DiaDeSemana -> Bool
estaEnElMedio Lunes = False
estaEnElMedio Domingo = False
estaEnElMedio _ = True


-- 3. Los booleanos también son un tipo de enumerativo. Un booleano es True o False. Defina
--las siguientes funciones utilizando pattern matching (no usar las funciones sobre booleanos
--ya definidas en Haskell):


-- A - Dado un booleano, si es True devuelve False, y si es False devuelve True.
--En Haskell ya está definida como not.
negar :: Bool -> Bool
negar True = False
negar False = True

-- B - Dados dos booleanos, si el primero es True y el segundo es False, devuelve False, sino
--devuelve True.
--Nota: no viene implementada en Haskell.
implica :: Bool -> Bool -> Bool
implica True False = False
implica True _ = True

-- C - Dados dos booleanos si ambos son True devuelve True, sino devuelve False.
--En Haskell ya está definida como &&.
and :: Bool -> Bool -> Bool
and True True = True
and _ _ = False

-- D - Dados dos booleanos si alguno de ellos es True devuelve True, sino devuelve False.
-- En Haskell ya está definida como ||.
or :: Bool -> Bool -> Bool
or False False = False
or _ _ = True

-- REGISTROS

-- 1. Definir el tipo de dato Persona, como un nombre y la edad de la persona. Realizar las
-- siguientes funciones:

type Alias = String
type Edad = Int

data Persona = P Alias Edad deriving Show

jose = P "Jose" 29
andres = P "Andres" 25

-- A - Devuelve el nombre de una persona.
nombre :: Persona -> String
nombre (P n e) = n

-- B - Devuelve la edad de una persona.
edad :: Persona -> Int
edad (P n e ) = e

-- C - Aumenta en uno la edad de la persona.
crecer :: Persona -> Persona
crecer (P n e ) = P (n) (e+1)

-- D - Dados un nombre y una persona, devuelve una persona con la edad de la persona y el
--nuevo nombre.
cambioDeNombre :: String -> Persona -> Persona
cambioDeNombre s (P n e )= P (s) (e)

-- E - Dadas dos personas indica si la primera es mayor que la segunda.
esMayorQueLaOtra :: Persona -> Persona -> Bool
esMayorQueLaOtra (P n e) (P s d) = if(e>d)
                                   then True
                                   else False

-- F - Dadas dos personas devuelve a la persona que sea mayor.
laQueEsMayor :: Persona -> Persona -> Persona
laQueEsMayor (P n e ) (P s d ) = if(e>d)
                                 then P (n) (e)
                                 else P (s) (d)

-- 2. Definir los tipos de datos Pokemon, como un TipoDePokemon (agua, fuego o planta) y un
-- porcentaje de energía; y Entrenador, como un nombre y dos pokemones. Luego definir las
-- siguientes funciones:

type Nombre = String 
type Energia = Int

data TipoDePokemon = Agua | Fuego | Planta deriving Show

data Pokemon =  ElPokemon TipoDePokemon Energia deriving Show

data Entrenador = ElEntrenador Nombre Pokemon Pokemon deriving Show

bulbasur = ElPokemon Planta 10

charmander = ElPokemon Fuego 20 

squirtle = ElPokemon Agua 30

ash = ElEntrenador "Ash" (bulbasur) (charmander)


