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
area (Triangle x) = (x^2)/2 
area (Trapeze v b h) = ((v + b) * h)/2 

--Funcion que calcula el perimetro de las figuras
perimeter :: Shape -> Float
perimeter (Circle r) = 2 * pi * r 
perimeter (Square l) = l * 4 
perimeter (Rectangle b h) = (b*2) + (h*2)  
perimeter (Triangle x) = x * 3 
perimeter (Trapeze v b h) = v + b + (h*2) 


--Ejercicio 2 (Les toca arreglar el sinonimo)

type Point = (Float, Float)

-- Funcion para calcular la distancia entre dos puntos
distance :: Point -> Point -> Float
distance (x, y) (n, m) = sqrt((n-x)^2+(m-y)^2) 

--Funcion para calcular la distancia de un punto al origen
from0 :: Point -> Float
from0 (x, y) = sqrt((x)^2+(y)^2)

--Ejercicio 3
data Haskellium = Undefined

--Funcion para regresar el hijo de dos Haskelliums dado su nombre
son :: Haskellium -> Haskellium -> String -> Haskellium
son = undefined

--Funcion para calcular las unidades para construir la casa de un Haskellium
houseCost :: Haskellium -> Float
houseCost = undefined

--Funcion para calcular el tiempo que le toma a un Haskellium para llegar a su trabajo
timeToWork :: Haskellium -> Float
timeToWork = undefined

--LISTAS Y FUNCIONES
--Ejercicio 1
palindromo :: String -> Bool
palindromo = undefined

--Ejercicio 2
myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr = undefined

--Ejercicio 3
conjuntoPotencia :: [a] -> [[a]]
conjuntoPotencia = undefined

--ARBOLES

--Implementacion

data OneTwoTree a = Undefinedd

--Ejercicio 2
suma :: OneTwoTree Int -> Int
suma = undefined