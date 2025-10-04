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


herencia en poo
Para la herencia hay que tener en cuenta la clase padre y la clase hija
la clase hija va a heredar los metodos y atributos de esta clase padre, donde hay 
que tener dos palabras clases, el extends, y super que se usa dentro de la subclase
para referirse a la clase padre

en ves en interfaces establecemos un contrato con metodos que si utilizamos promete 
que cumplira lo que especifica el comentario, estos metodos no estan implementados
para utilizarlos la palabra clave es implements

pila vs cola
el funcionamiento de estas dos TADS son muy diferentes
en la pila se lleva una coleccion linea de elementos el cual es el llamado tope
la pila tiene un sistema en cual el primero en entrar es el ultimo en salir 
podemos verlo como una pila de libros apilada, la pila tiene definida operaciones
push(apilar) agrega un nuevo elemento al tope 
pop(desapilar) desapila el ultimo elemento del tope 
Top(tope) nos informa de cual es el elemento del tope 


en la cola en la cual se lleva una coleccion de elementos lineal
un extremo es llamado inicio y el otro final
en la cola el primer el elemento en entrar es el primero en salir como una cola 
de supermercado, esta TAD soporta operaciones como:
enqueue(Encolar) agregar un elemento en el final
dequeue(Desencolar) saca el elemento al inicio 
esVacia() devuelve un bool true si la cola esta vacia false lo contrario
vaciar() elimina los elementos de la cola 
longitud() devuelve la cantidad de elementos de la cola 

