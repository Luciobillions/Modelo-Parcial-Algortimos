

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


