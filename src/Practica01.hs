module Practica01 where

--TIPOS ALGEBRAICOS

--Ejercicio 1
data Shape = Circle Float | --representa el radio
            Square Float | --representa un lado
            Rectangle Float Float| --representa base y altura
            Triangle Float | --representa un lado
            Trapeze Float Float Float --representa base mayor, base menor y altura
            deriving (Show, Eq)

--Funcion que calcula el area de las figuras
area :: Shape -> Float
area (Circle r) = pi *(r^2)
area (Square l) = (l^2)
area (Rectangle b h) = b * h
area (Triangle x) = (sqrt 3 / 4) * x^2
area (Trapeze b1 b2 h) = ((b1 + b2) / 2) * h

--Funcion que calcula el perimetro de las figuras
perimeter :: Shape -> Float
perimeter (Circle r) = 2 * pi * r 
perimeter (Square l) = l * 4 
perimeter (Rectangle b h) = (b*2) + (h*2)  
perimeter (Triangle x) = x * 3 
perimeter (Trapeze b1 b2 h) = b1 + b2 + 2*( sqrt  (((b1-b2)/2)^2 + h^2))


--Ejercicio 2 (Les toca arreglar el sinonimo)
type Point = (Float, Float)

-- Funcion para calcular la distancia entre dos puntos
distance :: Point -> Point -> Float
distance (x, y) (n, m) = sqrt((n-x)^2+(m-y)^2) 

--Funcion para calcular la distancia de un punto al origen
from0 :: Point -> Float
from0 = distance(0,0)

--Ejercicio 3
data Haskellium = Haskellium
    { name :: String
    , lastName1 :: String
    , lastName2 :: String
    , location :: Point
    , houseShape :: Shape
    } deriving (Show, Eq)
                  
                 
--Funcion para regresar el hijo de dos Haskelliums dado su nombre
son :: Haskellium -> Haskellium -> String -> Haskellium
son father mother childName =
    Haskellium
        { name = childName
        , lastName1 = lastName1 father
        , lastName2 = lastName1 mother
        , location = location father
        , houseShape = houseShape father
        }
                                                                             
--Funcion para calcular las unidades para construir la casa de un Haskellium
houseCost :: Haskellium -> Float
houseCost person = area (houseShape person) + perimeter (houseShape person) * 2.5 -- en vez de multiplicar por dos si le sumo el perimetro por la altura de 2.5 del pdf en teoria sumo las paredes dee la casa

--Funcion para calcular el tiempo que le toma a un Haskellium para llegar a su trabajo
timeToWork :: Haskellium -> Float
timeToWork person =
    tiempo (distance (location person) (0, 0))

tiempo :: Float -> Float -- Creamos la función auxiliar tiempo, para ayudar con los dos casos que hay, la funcion recibe la distancia y si es menor a 300 divide entre 30 de la bici y si no la vlocidad de la moto 70
tiempo d
    | d < 300 = d / 30
    | otherwise = d / 70


--LISTAS Y FUNCIONES
--Ejercicio 1
palindromo :: Eq a => [a] -> Bool
palindromo []  = True
palindromo [_] = True
palindromo xs  = head xs == last xs && palindromo (init (tail xs))

--Ejercicio 2
myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ z []     = z
myFoldr f z (x:xs) = f x (myFoldr f z xs)


--Ejercicio 3
conjuntoPotencia :: [a] -> [[a]]
conjuntoPotencia = myFoldr (\x acc -> myFoldr (\ys r -> (x:ys):r) [] acc ++ acc ) [[]]  -- Utilizamos el mismo foldr para que sea mas facil

--ARBOLES

--Implementacion

data OneTwoTree a = Vacio   --Definimos nuestro nodo vacio
                   | Nodo a (OneTwoTree a)   --definimos nuestro nodo que tendra un hijo
                   | Rama a (OneTwoTree a) (OneTwoTree a)    --Definimos la rama que puede tener 2 hijos
                    deriving (Show, Eq)

--Ejercicio 2
suma :: OneTwoTree Int -> Int
suma Vacio = 0
suma (Nodo x n) = x + suma n
suma (Rama x n m) = x + suma n + suma m