module Practica01 where

--FUNCIONES
valorAbs :: Int -> Int
valorAbs x = if x >= 0
    then x --Si es mayor o igual a 0, directamente regresará el mismo número
    else -x --Si es menor a 0. multiplicará el número por - para que de el valor absoluto

esDivisor :: Int -> Int -> Bool
esDivisor n m = if mod m n == 0
    then True --Devolverá true si el residuo de la división entre dos números es 0
    else False --Devolverá false si el residuo no es 0

cuadratica :: Float -> Float -> Float -> Float -> Float
cuadratica a b c v = a*(v*v) + b*v + c --Calcula y devuelve el resultado de la ecuación a(x*x)+bx+c donde x es el valor v.

sumaFracciones :: (Int, Int) -> (Int, Int) -> (Int, Int)
sumaFracciones (a,b) (c,d) = if b==d --Primero verifica si los denominadores son iguales
    then  ((a+c),(b)) --Si son iguales, solo suma los numeradores y transcribe el denominador
    else  (((a*d)+(b*c)),(b*d)) --Si no, hace una suma de fracciones con diferente denominador

comparador :: Float -> Float -> Int
comparador n m = if n==m
    then 0 --Si n y m son iguales, devuelve 0
    else if n>m 
        then 1 --Si n es mayor a m, devuelve 1
        else (-1) --Si no ocurre ninguna de las condiciones anteriores, quiere decir que m es mayor a n, y devuelve -1.

puntoMedio :: (Float, Float) -> (Float, Float) -> (Float, Float)
puntoMedio (a,b) (c,d) = (((a+c)/2),((b+d)/2)) 


--RELACIONES
type Rel a b = [(a, b)]

relacionDivisor :: Rel Int Int
relacionDivisor = concat [ if (b `mod` a == 0) && (a `mod` 2 == b `mod` 2)
             then [(a,b)]
             else []
         | a <- [1..30], b <- [1..30] ]

relacionSumaEspecial :: Rel Int Int
relacionSumaEspecial = concat [ if ((a + b) `mod` 5 == 0) && (a < b)
             then [(a,b)]
             else []
         | a <- [1..30], b <- [1..30] ]

relacionCongruentesModuloN :: Int -> Rel Int Int
relacionCongruentesModuloN n = concat [ if (a `mod` n == b `mod` n) && (a /= b)
            then [(a,b)]
            else []
        | a <- [1..30], b <- [1..30] ]


--NATURALES
-- Cero es natural, Suc Cero es natural, Suc Suc Cero es natural, etc.
data Natural = Cero | Suc Natural deriving (Show,Eq) --Esto es para que se muestre y que se puedan comparar

esPar :: Natural -> Bool
esPar Cero = True
esPar (Suc Cero) = False
esPar (Suc (Suc n)) = esPar n

iguales :: Natural -> Natural -> Bool
iguales Cero Cero = True
iguales (Suc a) Cero = False
iguales Cero (Suc a) = False
iguales (Suc a) (Suc b) = iguales a b

maximo :: Natural -> Natural -> Natural 
maximo Cero Cero = Cero
maximo Cero (Suc a) = (Suc a)
maximo (Suc a) Cero = (Suc a)
maximo (Suc a) (Suc b) = Suc (maximo a b)

potencia :: Natural -> Natural -> Natural
potencia Cero (Suc a) = Cero
potencia (Suc a) Cero = (Suc Cero)
potencia a (Suc b) = multiplicacion a (potencia a b)

multiplicacion :: Natural -> Natural -> Natural
multiplicacion Cero Cero = Cero
multiplicacion Cero (Suc x) = Cero
multiplicacion (Suc x) Cero = Cero
multiplicacion x (Suc y) = suma x (multiplicacion x y)

suma :: Natural -> Natural -> Natural
suma Cero Cero = Cero
suma Cero (Suc x) = Suc x
suma (Suc x) Cero = Suc x
suma x  (Suc y) = Suc (suma x y)
