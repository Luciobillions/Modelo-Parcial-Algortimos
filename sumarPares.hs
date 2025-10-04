{-
Escribir una función recursiva 
sumaPares :: [Int] -> Int que reciba una lista de enteros 
y devuelva la suma de todos los números pares.
-}

sumaPares:: [Int] -> Int
sumaPares [] = 0    --caso base
sumaPares (x:xs) =  --caso recursivo
    if x `mod` 2 ==0  --si x es par
        then x + sumaPares xs --suma y continua
        else sumaPares xs   -- si no es par ignorarlo y continua



{-
Ejemplo con lista [1,2,4]
cabeza x = 1, xs=[2,4]
1 mod 2 = impar
x = 2, xs=[4]
2 mod 2 = 0 par
= 2 + sumaPares x=[4]
4 mod 2 = 0 par 
=2 + 4 sumaPares [] -- caso base
=2 + 4 + 0 
=6


-}

{-
sumaLista [x] = x
sumaLista(x:xs)= x + sumaPares xs
-}


--caso de recursion a la cola 
sumaLista :: [Int] -> Int
sumaLista xs = sumaAux xs 0
  where
    sumaAux [] acc = acc  
    sumaAux (x:xs) acc = sumaAux xs (acc + x) 


