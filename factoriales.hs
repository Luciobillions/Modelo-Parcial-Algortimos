{-
(d) [3 puntos] Recursión en funciones.
Se quiere definir funciones recursivas factorial y sumaFactoriales usando recursión mutua: factorial n calcula n!, y sumaFactoriales n suma factoriales de 1 a n. Ejemplo:
> factorial 3
6
> sumaFactoriales 3
9 (1! + 2! + 3! = 1 + 2 + 6).

    Completa el perfil de tipo: factorial :: Int -> Int y sumaFactoriales :: Int -> Int
    Implementa las funciones con recursión mutua. Reemplaza undefined en el siguiente esquelet

-}


factorial :: Int -> Int
factorial 0 = 1 -- Caso base
factorial n = n * factorial (n-1) -- Llamar recursivamente a factorial (n-1)

sumaFactoriales :: Int -> Int
sumaFactoriales 0 = 0  -- Caso base
sumaFactoriales n = factorial n + sumaFactoriales (n-1)

mult3 :: Int -> (Int -> Int -> Int)
mult3 x y z = x * y * z


--f es (a -> b -> c -> d) que es la funcion 
mult3u :: (Int , Int, Int) -> Int
mult3u  (x, y, z) = mult3 x y z 