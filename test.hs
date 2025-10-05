

{-
filter (>3) . map (*2) $ [1,2,3,4]
1 * 2, 2 * 2 , 2 * 3 , 2 * 4 
[2,4,6,8] --map
filter > 3 
[4,6,8]
devuelve una lista de Int
-}

--recursion normal, cada llamada espera que termine la recursion para suma 1
longitud:: [Int] -> Int
longitud [] = 0
longitud (_:xs) = 1 + longitud xs

--recursion a la cola mas eficiente
longitudc::[Int] -> Int
longitudc xs = longaux xs 0
    where
        longaux [] acc = acc 
        longaux (_:xs) acc =  longaux xs (acc + 1)

{-
longitudc[1,2,3]
    longaux[1,2,3] 0
    longaux[2,3] 1
    longaux[3] 2
    longaux[] 3 -- caso base 
    3 
-}

eliminarUltimo::[Int] -> [Int]
eliminarUltimo [] = []
eliminarUltimo [x] = [] -- este es importante
eliminarUltimo(x:xs) = x : eliminarUltimo xs

duplicar :: [Int] -> [Int]
duplicar [] = []
duplicar (x:xs) = x : x : duplicar xs

eliminarImpares :: [Int] -> [Int]
eliminarImpares [] = []
eliminarImpares (x:xs) = if x  `mod` 2 == 0
    then x : eliminarImpares xs 
    else eliminarImpares xs

reversa :: [Int] -> [Int]
reversa [] = []
reversa (x:xs) = reversa xs ++ [x] 


{-
any (>5) [1,3,7,2]
devuelve un tipo Bool, T

compose (filter even) (map (+1)) [1,2,3,4]

[2,3,4,5]
[2,4]
-}

producto :: [Int] -> Int
producto xs = productoAux xs 1
    where
        productoAux [] acc = acc  
        productoAux (x:xs) acc = 
            if x == 0 then 0 -- Si 0, retornar 0 inmediatamente
            else productoAux xs (x*acc)  -- Multiplicar x a acc y recursionar