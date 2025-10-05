

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


{-
all even [2,4,6]

devuelve un Bool T

scanl (+) 0 [1,2,3]

devuelve una lista de Int
[0] acumulador
0+1=1
[0,1]
1+2=3
[0,1,3]
3+3=6
[0,1,3,6]
-}


fib :: Int -> Int
fib 0 = 0 
fib 1 = 1   
fib n = fib(n-1) + fib(n-2)  

--fib con recursion a la cola 
fibb:: Int -> Int
fibb n = fibAux n 0 1 
    where
        fibAux 0 a _ = a 
        fibAux k a b = fibAux (k-1) b (a+b)

{-
fibAux 6 0 1 -> 5 1 1 
fibAux 5 1 1 -> 4 1 2 
fibAux 4 1 1 -> 3 2 3 
fibAux 3 2 1 -> 2 3 5
fibAux 2 3 5 -> 1 5 8
fibAux 1 4 8 -> 0  8 13 --fibAux 0 a _ = a 21q  
-}


{-
dropWhile (<0) [-1, -2, 3, 4]
devuelve una lista de Int
[3,4]

foldr (&&) True [True, False, True]
devuelve Bool, en cual es False
-}



--a la cola 
maximo :: [Int] -> Int
maximo (x:xs) = maximoAux xs x
    where
        maximoAux [] acc = acc -- Caso base: retornar acumulador
        maximoAux (y:ys) acc = 
            if y > acc then  maximoAux ys y -- Si y mayor, actualizar acc a y y recursionar
            else maximoAux ys acc -- Si no, mantener acc y recursionar



maxx::[Int] -> Int
maxx [x] = x
maxx(x:xs) = 
    let m = maxx xs
    in if x > m then x else m 



--version currificada
conca:: [a] -> [a] -> [a]
conca [] xs = xs
conca ys [] = ys
conca (x:xs) ys = x : conca xs ys 


--version descurrificada
concatenar:: ([a],[a]) -> [a]
concatenar ([], ys ) = ys
concatenar(x:xs,ys) = x : concatenar (xs,ys)

