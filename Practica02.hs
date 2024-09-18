----1. Longitud de una lista----
longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud(xs)

----2. Suma de n numeros---
sumaLista :: Num a => [a] -> a
sumaLista [] = 0
sumaLista (x:xs) = x + sumaLista xs

----3. Agregar elemento a una lista----
agregaElemento :: [a] -> a -> Bool -> [a]
agregaElemento lista elem True  = elem : lista   -- Inserta al principio
agregaElemento lista elem False = lista ++ [elem] -- Inserta al final

----4. Maximo de una lista----
maximoLista :: (Num a, Ord a) => [a] -> a
maximoLista [] = error "La lista no puede estar vacia"
maximoLista [x] = x
maximoLista (x:xs) = max x (maximoLista xs)
