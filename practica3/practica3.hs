--TIPOS RECURSIVOS SIMPLES

-- 1 ----------------Celdas con bolitas-----------------------------

-- Representaremos una celda con bolitas de colores rojas y azules, de la siguiente manera:

data Color = Azul | Rojo deriving Show
data Celda = Bolita Color Celda | CeldaVacia deriving Show


-- En dicha representación, la cantidad de apariciones de un determinado color denota la cantidad
-- de bolitas de ese color en la celda. Por ejemplo, una celda con 2 bolitas azules y 2 rojas, podría
-- ser la siguiente:
celda1 = CeldaVacia
celda2 = Bolita Rojo (Bolita Azul (Bolita Rojo (Bolita Azul (Bolita Rojo CeldaVacia))))
celda3 = Bolita Azul CeldaVacia
-- 1 - Dados un color y una celda, indica la cantidad de bolitas de ese color. Nota: pensar si ya
-- existe una operación sobre listas que ayude a resolver el problema.
 
nroBolitas :: Color -> Celda -> Int
nroBolitas cl1 CeldaVacia = 0
nroBolitas cl1 (Bolita cl2 cld) =  if(esBolitaDeColor cl1 cl2) 
                                       then 1 + nroBolitas cl1 cld 
                                       else 0 + nroBolitas cl1 cld 

esBolitaDeColor :: Color -> Color -> Bool
esBolitaDeColor Azul Azul = True
esBolitaDeColor Rojo Rojo = True
esBolitaDeColor _  _ = False

-- 2 - Dado un color y una celda, agrega una bolita de dicho color a la celda.
poner :: Color -> Celda -> Celda
poner cl1 CeldaVacia = Bolita cl1 CeldaVacia
poner cl1 cld = moverACeldaVaciaYPoner cl1 cld

moverACeldaVaciaYPoner :: Color -> Celda -> Celda
moverACeldaVaciaYPoner cl1 (Bolita cl2 cld) = Bolita cl1 (Bolita cl2 cld)

--3 - Dado un color y una celda, quita una bolita de dicho color de la celda. Nota: a diferencia de
-- Gobstones, esta función es total.
sacar :: Color -> Celda -> Celda
sacar cl1 CeldaVacia = error "La Celda se encuentra vacia"
sacar cl1 (Bolita cl2 cld) = if(esBolitaDeColor cl1 cl2) then CeldaVacia else sacar cl2 cld
-- Falta el caso en que la celda tiene varias Bolitas y se saca un color .

-- 4 - Dado un número n, un color c, y una celda, agrega n bolitas de color c a la celda.
ponerN :: Int -> Color -> Celda -> Celda
ponerN 0 cl1 cld = cld 
ponerN n cl1 cld= Bolita cl1 (ponerN (n-1) cl1 cld)


--2 -------------------- Camino hacia el tesoro --------------------------

data Objeto = Cacharro | Tesoro deriving Show
data Camino = Fin | Cofre [Objeto] Camino | Nada Camino deriving Show 
-- Definir las siguientes funciones:

camino1 = Fin
camino2 = Nada (camino1)
camino3 = Cofre [Cacharro,Cacharro] (camino1)
camino4 = Nada (Cofre [Cacharro] (camino2))
camino5 = Cofre [Cacharro] (Nada (Cofre [Tesoro] Fin) ) 
camino6 = Cofre [Tesoro] Fin


--1 - Indica si hay un cofre con un tesoro en el camino.

hayTesoro :: Camino -> Bool
hayTesoro Fin = False
hayTesoro (Cofre obs restoDelCamino) = hayTesoroEn obs || hayTesoro restoDelCamino
hayTesoro (Nada restoDelCamino) = hayTesoro restoDelCamino

hayTesoroEn :: [Objeto]->Bool
hayTesoroEn [] = False
hayTesoroEn (ob:obs) = esTesoro ob ||  hayTesoroEn obs 

esTesoro ::  Objeto -> Bool
esTesoro Tesoro = True
esTesoro _  = False

-- 2 - Indica la cantidad de pasos que hay que recorrer hasta llegar al primer cofre con un tesoro.
-- Si un cofre con un tesoro está al principio del camino, la cantidad de pasos a recorrer es 0.
-- Precondición: tiene que haber al menos un tesoro.

tieneTesoro :: [Objeto] -> Bool
tieneTesoro [] = False
tieneTesoro [Tesoro] = True
tieneTesoro (ob:obs) =  esTesoro ob || tieneTesoro obs

pasosHastaTesoro :: Camino -> Int
pasosHastaTesoro Fin = error "no hay camino"
pasosHastaTesoro (Nada camino) = 1 + (pasosHastaTesoro camino)
pasosHastaTesoro (Cofre os camino) = if tieneTesoro os then 0 else (1 + pasosHastaTesoro camino)

-- Falta completar algunos ejercicios

-- 2 - TIPOS ARBOREOS

-- Dada esta definición para árboles binarios

data Tree a = EmptyT | NodeT a (Tree a) (Tree a) deriving Show

-- defina las siguientes funciones utilizando recursión estructural según corresponda:
arbol1 = EmptyT
arbol2 = NodeT 2 EmptyT EmptyT
arbol3 = NodeT 5 (NodeT 4 (EmptyT) (EmptyT)) (NodeT 4 (EmptyT) (EmptyT))
arbol4 = NodeT 6 (NodeT 4 (NodeT 5 EmptyT EmptyT)(NodeT 5 EmptyT EmptyT))
                 (NodeT 4 (NodeT 5 EmptyT EmptyT)(NodeT 5 EmptyT EmptyT))

-- 1 - Dado un árbol binario de enteros devuelve la suma entre sus elementos.
sumarT :: Tree Integer -> Integer
sumarT EmptyT = 0
sumarT (NodeT n t1 t2) =  n + sumarT t1 + sumarT t2

-- 2 - Dado un árbol binario devuelve su cantidad de elementos, es decir, el tamaño del árbol (size
-- en inglés).
sizeT :: Tree a -> Int
sizeT EmptyT = 0
sizeT (NodeT n t1 t2) = 1 + sizeT t1 + sizeT t2  

-- 3 -  Dado un árbol de enteros devuelve un árbol con el doble de cada número.
mapDobleT :: Tree Integer -> Tree Integer
mapDobleT  EmptyT = EmptyT
mapDobleT  (NodeT n t1 t2 ) = NodeT (n*2) (mapDobleT t1) (mapDobleT t2)

