--Proyecto 1

--ejercicio 1)a)


esCero :: Int -> Bool
esCero n | n == 0 = True
         | n /= 0 = False

--ejercicio 1)b)

esPositivo :: Int -> Bool
esPositivo n| n > 0 = True
            | n <= 0 = False
 

--ejercicio 1)c)

esVocal :: Char -> Bool 
esVocal n| n == 'a' || n == 'e' || n == 'i' || n == 'o' || n == 'u' = True
         | otherwise = False  

-- ejercicio 2) a)

paratodo ::  [Bool] -> Bool
paratodo [] = True
paratodo (x:xs) | x == True = paratodo xs 
                | x == False = False
               

--ejercicio 2) b)
sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (x:xs) = x  + sumatoria xs

--ejercicio 2) c)
productoria :: [Int] -> Int
productoria [] = 1
productoria (x:xs) = x*productoria xs

--ejercicio 2) d)

factorial :: Int -> Int 
factorial 0 = 1
factorial n = n * factorial (n-1)

--ejer 2 ) e)

promedio ::  [Int] -> Int
promedio [] = 0 
promedio (x:xs) = div (sumatoria (x:xs))  (length(x:xs))


--ejer 3)
pertenece :: Int -> [Int] -> Bool
pertenece n [] = False
pertenece n (x:xs) | n == x = True
                   | n /= x = pertenece n xs


--ejer 4)

encuentra :: Int -> [(Int , String)] -> String
encuentra n [] = ""
encuentra n ((x,y):xs) | n == x = y
                       | n /= x = encuentra n xs

--ejer 5) a)

paratodo' :: [a] -> (a -> Bool) -> Bool
paratodo' [] t = True
paratodo' (x:xs) t | (t x) == True = paratodo' xs t
                   | (t x) == False = False

--ejer 5) b)

existe' :: [a] -> (a -> Bool) -> Bool
existe' [] t = False
existe' (x:xs) t | (t x) == True = True
                 | (t x) == False = existe' xs t

--ejer 5) c)

sumatoria':: [a] -> (a -> Int) -> Int
sumatoria' [] t = 0
sumatoria' (x:xs) t = (t x) + sumatoria' xs t

--ejer 5) d)

productoria':: [a] -> (a -> Int) -> Int
productoria' [] t = 1
productoria' (x:xs) t = (t x) * productoria' xs t

--ejer 6)

paratodo'' :: [a] -> (a -> Bool) -> Bool
paratodo'' xs t = paratodo' xs t

--ejer 7) a)

todosPares :: [Int] -> Bool
todosPares xs = paratodo' xs even

--Ejer 7)b)

-- funcion auxiliar esmultiplo
esmultiplo :: Int -> Int -> Bool
esmultiplo y x = mod x y == 0

hayMultiplo :: Int -> [Int] -> Bool
hayMultiplo n xs = existe' xs (esmultiplo n)

--Ejer 7) c)

sumaCuadrados :: Int -> Int
sumaCuadrados x = sumatoria' [0..x] (^2)

--Ejer 7) d)

factorial' :: Int -> Int
factorial' x = productoria' [1..x] (*1)

--Ejer 7) e)

--funcion auxiliar listaPar
listaPar :: [Int] -> [Int]
listaPar [] = []
listaPar (x:xs) | mod x 2 == 0 = (x^1): listaPar xs
                | mod x 2 == 1 = (x^0): listaPar xs

multiplicaPares :: [Int] -> Int
multiplicaPares xs = productoria' (listaPar xs) (*1)

--Ejer 8)

--A)La funcion "map" es un tipo de funcion que toma una lista y devuelve otra lista cuyos elementos son los que se obtiene de aplicar una funcion determinada a cada elemento de la primer lista en el mismo orden.
-- A)La funcion "filter", toma una lista de elementos y devuelve otra, cuyos elementos de esta lista son los elementos de la primera que cumplen cierta condicion.

--B) La expresion "map succ xs" se trata de una funcion en donde, dada una lista, se devuelve otra lista con cada elemento de esta nueva lista incrementada en "1". Por lo tanto, "map succ [1, -4, 6, 2, -8]" devuelve la lista [2, -3, 7, 3, -9].

--C) La expresion "filter esPositivo xs" equivale a devolver una lista, pero solo con los elementos positivos de la primera lista. Es decir que, "filter esPositivo [1, -4, 6, 2, -8]" devuelve la lista [1, 6, 2].


--Ejer 9) a)

duplica :: [Int] -> [Int]
duplica [] = []
duplica (x:xs) = x*2 : duplica xs


--Ejer 9) b)

duplica' :: [Int] -> [Int]
duplica' (xs) = map (2*) (xs)

--Ejer 10) a)

espar :: [Int] -> [Int] 
espar [] = []
espar (x:xs) | mod x 2 == 0 = x: espar xs
             | mod x 2 /= 0 = espar xs

--Ejer 10)b)

g :: Int -> Bool
g x | mod x 2 == 0 = True
    | otherwise = False

espar' :: [Int] -> [Int]
espar' xs = filter g xs

--Ejer 10) c)

multiplicaPares' :: [Int] -> Int
multiplicaPares' (x:xs) = productoria' (espar (x:xs)) (*1)

--ejer 11) a)

sumarAlista :: Num a => a -> [a] -> [a]
sumarAlista n [] = []
sumarAlista n (x:xs) = n+x : sumarAlista n xs

encabezar :: a -> [[a]] -> [[a]]
encabezar n [] = []
encabezar n (x:xs) = (n:x): encabezar n xs

mayoresA :: Ord a => a -> [a] -> [a]
mayoresA n [] = []
mayoresA n (x:xs) | x > n = x : mayoresA n xs
                  | x <= n = mayoresA n xs

--Ejer 11)b)

sumarAlista' :: Num a => a -> [a] -> [a]
sumarAlista' n (xs) = map (n+) xs

-----------------------------------------------

encabezar' :: a -> [[a]] -> [[a]]
encabezar' n (xs) = map (n:) (xs)

------------------------------------------------------

mayoresA' :: Ord a => a -> [a] -> [a]
mayoresA' n xs = filter (>n) xs

--Ejer 12)

--funcion auxiliar pri
pri :: Int -> (Int,String) -> Bool
pri n (x,y) = n == fst (x,y)

cade :: (a, String) -> String
cade (x,y) = y

encuentra' :: Int -> [(Int , String)] -> String
encuentra' n [] = ""
encuentra' n xs = head (map (cade) (filter (pri n) xs))

--Ejer 13)a)

primIgualesA :: Ord a => a -> [a] -> [a]
primIgualesA n (x:xs) | n == x = (x: primIgualesA n xs)
                      | n /= x =  []  

--Ejer 13)b)

primIgualesA' :: Ord a => a -> [a] -> [a]
primIgualesA' n (x:xs) = takeWhile (n==) (x:xs)

--Ejer 14)a)

primIguales :: Eq a => [a] -> [a]
primIguales [] = []
primIguales [x] = [x] 
primIguales (x:xs) | x == head xs = x : (primIguales xs)
                   | x /= head xs = x : []

--Ejer 14)b)

primIguales' :: Ord a => [a] -> [a]
primIguales' [] = []
primIguales' (xs) = primIgualesA (head xs) xs

--Ejer 15)a)

minimo :: Ord a => [a] -> a
minimo [x] = x
minimo (x:xs) = min x (minimo xs) 

--ejer 15)b)

minimo' :: (Bounded a, Ord a) => [a] -> a
minimo' [] = maxBound
minimo' (x:xs) = min x (minimo xs)

--Ejer 16)
--a)
q' :: (a,b) -> a
q' x = fst x

--b)
p' :: (a,b) -> [(a,b)]
p' (x,y) = (x,y):[]

--c)
--f :: [(a,b)] -> ...
--f (a,b) = ...
--esta mal tipada por que la funcion pide como argumento una lista de pares, y el argumento que da es solo un par.

--d)
y':: [(a,b)] -> (a,b)
y' (x:xs) = x 
--Falla en el caso de la lista vacia

--e)
g' :: [(a,b)] -> (a,b)
g' ((x,y):((a,b):xs)) = (x,b)
--Falla en caso de lista vacia
--Falla en caso de que la lista tenga un solo elemento

--f)
o' :: [(Int,a)] -> a 
o' [(0,a)] = a 
--Falla en caso de lista vacia
--Falla cuando el primer elemento del par no es un cero
--Falla cuando hay mas de un par

--g) Falta hacer
--c' :: [(Int,a)] ->
--c' ((x,1):xs) = 
--Esta plantilla esta mal tipada, ya que pide que el segundo elemento del par sea de tipo a, y este asume ya directamente que es el numero "1"(Int), siendo esto no necesariamente asi

--h)
u' :: [(Int,a)] -> (Int,a)
u' ((1,x):xs) = (1,x)
--Falla en caso de lista vacia
--Falla en caso de que el primer elemento del primer par no sea "1"

--i)
h' :: (Int -> Int) -> Int -> Int
h' a b = a b + b

--j)
d' :: (Int -> Int) -> Int -> Int 
d' a 3 = (a 3)^2 
--Falla en el caso cuando le damos un estero distinto de 3 a la funcion.

--k)
--k' :: (Int -> Int) -> Int -> 
--k' 0 1 2 = 
--Esta mal tipada la funcion, ya que como argumentos solo da numeros, y la funcion pide como primer argumento que de funcion que vaya de enteros a enteros

--i) 
l' :: a -> (a -> a) -> a 
l' a v = v a 
 

--Ejer 17)
--a)
f :: (a,b) -> a
f (x,y) = x
--
f'' :: (a, b) -> a
f'' x = fst x
--b)
r :: (a,b) -> b
r (x,y) = y
--
r'' :: (a, b) -> b
r'' x = snd x

--c) 
--u :: (a,b) -> c
--No existe ninguna funcion que cumpla esto, ya que si tomamos un par de elementos de diferentes tipos, no podemos compararlos entre si, y menos podremos crear otro tipo diferentes al que ya tenemos


--d)
--v ::  a -> b
--no existe ninguna funcion que genericamente convierta un tipo de argumento en otro, sea cual sea el argumento dado

--e)
q :: (a -> b) -> a -> b
q t n = t n

--f)
z :: (a -> b) -> [a] -> [b]
z t [] = []
z t (x:xs) = (t x) : (z t xs)

z' :: (a -> b) -> [a] -> [b]
z' t xs = map t xs


--g)
--p :: (a -> b) -> a -> c
--Por el mismo razonamiento del pundo "d", aca estariamos tomando una funcion que manda de un argumento "a" a uno "b", y un argunmento a, lo que sucede es que tampoco existe una funcion que genericamente mande un argumento al otro sea cual sea.


--h)
walk :: (a -> b) -> (b -> c) -> a -> c -- Ejemplo ([Int] -> Int) -> (Int -> Bool) -> [Int] -> Bool
walk t r n = r (t n)
