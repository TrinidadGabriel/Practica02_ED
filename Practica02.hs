--------------- Listas y recursión ---------------

--Longitud de una lista--
longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud (xs) 

--Suma de n numeros de una lista--
sumaLista :: Num a => [a] -> a
sumaLista [] = 0
sumaLista (x:xs) = x + sumaLista xs

--Agregar un elemento a una lista--
agregaElemento :: [a] -> a -> Bool -> [a]
agregaElemento [] a booleano = a : []
agregaElemento (x:xs) a booleano = if booleano 
                    then a : (x:xs)
                    else (x:xs) ++ [a]

--Maximo de una lista--
maximoLista :: (Num a, Ord a) => [a] -> a
maximoLista [] = error "La lista no puede estar vacia"
maximoLista [x] = x
maximoLista (x:xs) = max x (maximoLista xs)

--Recuperar un elemento segun un indice--
indice :: [a] -> Int -> a
indice [] y = error "La lista no tiene elementos"
indice (x:xs) y = if y < 0 || y >= longitud (x:xs)
                    then error "El indice esta fuera del rango"
                    else if y == 0
                            then x
                            else indice xs (y-1)

--------------- Listas por comprehensión ---------------

divisores :: Int -> [Int]
divisores _ = undefined

conjunto :: Eq a => [a] -> [a]
conjunto _ = undefined

--Obtener los numeros de pares de una lista--
numerosPares :: [Int] -> [Int]
numerosPares [] = []
numerosPares (x:xs) = if x `mod` 2 == 0
                        then x : numerosPares xs
                        else numerosPares xs