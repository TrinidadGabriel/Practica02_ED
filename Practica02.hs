--------------- Listas y recursión ---------------

---Longitud de una lista---
longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud (xs) 

---Suma de n numeros de una lista---
sumaLista :: Num a => [a] -> a
sumaLista [] = 0
sumaLista (x:xs) = x + sumaLista xs

---Agregar un elemento a una lista---
agregaElemento :: [a] -> a -> Bool -> [a]
agregaElemento [] a booleano = a : []
agregaElemento (x:xs) a booleano = if booleano 
                    then a : (x:xs)
                    else (x:xs) ++ [a]

---Maximo de una lista---
maximoLista :: (Num a, Ord a) => [a] -> a
maximoLista [] = error "La lista no puede estar vacia"
maximoLista [x] = x
maximoLista (x:xs) = if x < maximoLista xs
                        then maximoLista xs
                        else x

---Recuperar un elemento segun un indice---
indice :: [a] -> Int -> a
indice [] y = error "La lista no tiene elementos"
indice (x:xs) y = if y < 0 || y >= longitud (x:xs)
                    then error "El indice esta fuera del rango"
                    else if y == 0
                            then x
                            else indice xs (y-1)

--------------- Listas por comprehensión ---------------

---Divisores de un numero entero---
divisores :: Int -> [Int]
divisores n = [x | x <- [1..n], mod n x == 0] 

---Convertir lista a conjunto---
conjunto :: Eq a => [a] -> [a]
conjunto [] = []
conjunto (x:xs) = x : conjunto [y | y <- xs, y /= x]

---Obtener los numeros de pares de una lista---
numerosPares :: [Int] -> [Int]
numerosPares [] = []
numerosPares (x:xs) = [n | n <- x:xs, mod n 2 == 0] 
        