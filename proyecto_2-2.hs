-------------------------Proyecto2

------------------------------------- ejercicio 1) a)

data Carrera = Matematica | Fisica | Computacion | Astronomia | Profesorado
      deriving (Eq, Show)
--ejercicio 1) b)

titulo :: Carrera -> String
titulo Matematica = "Licenciatura en Matematica"
titulo Fisica = "Licenciatura en Fisica"
titulo Computacion = "Licenciatura en Computacion"
titulo Astronomia = "Licenciatura en Astronomia"
titulo Profesorado = "Profesorado"

--ejercicio 1) c)

titulo' :: Carrera -> String
titulo' x | x==Matematica = "Licenciatura en Matematica"
          | x==Fisica = "Licenciatura en Fisica"
          | x==Computacion = "Licenciatura en Computacion"
          | x==Astronomia = "Licenciatura en Astronomia"
          | x==Profesorado = "Profesorado" 


------------------------------------------ejercicio 2) a)

type Ingreso = Int

data Cargo = Titular|Asociado|Adjunto|Asistente|Auxiliar
     deriving (Eq, Show)

data Area = Administrativa|Ensenanza|Economica|Postgrado
    deriving (Eq, Show)

data Rol = Decanx Genero|Docente  Cargo|NoDocente  Area|Estudiante  Caring
      deriving (Eq, Show)

data Genero = Femenina | Masculino
      deriving (Eq, Show)

type Caring = (Carrera, Ingreso)

--b)
--El tipo del constructor Docente es Cargo -> Rol (o sea que es de tipo Rol)

--c)

cuantos_doc :: [Rol] -> Cargo -> Int
cuantos_doc [] c = 0
cuantos_doc (x:xs) c | (x == Docente c)= 1 + cuantos_doc (xs) c
                     | otherwise = cuantos_doc xs c

--d)   
cuantos_doc' :: [Rol] -> Cargo -> Int
cuantos_doc' (xs) c = length (filter (== Docente c) xs)

--e)
--Se podría aumentar al constructor Decanx, un argumento que tenga los Constructores Feminina y Masculino

--f)
--Para poder representar un alumno que está inscripto en dos carreras debería modificarse el tipo Rol, los argumentos del constructor Estudiante, se declara un tipo para ambos, el cual puede ser una tupla 

estudia :: Rol -> Carrera -> Bool
estudia (Estudiante r) c = fst r == c  

------------------------------------------------------------ ejercicio 3)
--a)

data Persona = Per Apellido Nombre Dni FechaNac Rol
        deriving (Eq, Show)

type Apellido = String
type Nombre = String
type Dni = Int
type FechaNac = (Int,Int,Int)

--b)

type Per = Persona

--c)
  --1)

edad :: Persona -> (Int,Int,Int) -> Int
edad (Per _ _ _ (r, t, y) _) (z,x,c) | t < x || (t == x && r <= z) = (c-y)
                                     | otherwise = (c-y)- 1

--c)
  --2)

existe :: String -> [Persona] -> Bool
existe apellido [] = False
existe apellido ((Per q _ _ (_, _, _) _):xs) | q == apellido = True
                                             | otherwise = existe apellido xs

--c)
  --3)
estuastro :: Persona -> Bool
estuastro (Per _ _ _ _ c) = estudia c Astronomia

est_astronomia :: [Persona] -> [Persona]
est_astronomia [] = []
est_astronomia xs = filter (estuastro) xs

--c)
  --4)

padron_nodocente :: [Persona] -> [(String, Int)]
padron_nodocente [] = []
padron_nodocente ((Per a n d _ r) :xs) | r == NoDocente Administrativa = (a ++ "," ++ n, d): (padron_nodocente xs)
                                       | r == NoDocente Ensenanza = (a ++ "," ++ n, d): (padron_nodocente xs)
                                       | r == NoDocente Economica = (a ++ "," ++ n, d): (padron_nodocente xs)
                                       | r == NoDocente Postgrado = (a ++ "," ++ n, d): (padron_nodocente xs)
                                       | otherwise = padron_nodocente xs

----------------------------------------- ejer 4)

data Cola = Vacia | Encolada Persona Cola
      deriving (Show, Eq)
--a)
--let tt = Per "Tre" "dre" (2,3,4) 56 (Docente Titular)
--let uu = Per "Rts" "rrr" (7,8,9) 0 (Docente Adjunto)
--let yy = Per "fg" "hgf"  (8,4,6) 8 (Decanx Masculino)


--1)

atender :: Cola -> Cola 
atender Vacia = Vacia 
atender (Encolada _ Vacia) = Vacia
atender (Encolada _ (Encolada r b)) = Encolada r b 

--2)

encolar :: Persona -> Cola -> Cola
encolar p Vacia  = Encolada p Vacia 
encolar t (Encolada r c) | c == Vacia = Encolada r (Encolada t Vacia)
                         | otherwise = (Encolada r (encolar t c)) 

--3)
f' :: Persona -> Rol
f' (Per _ _ _ _ u) = u 

busca :: Cola -> Cargo -> Persona
busca Vacia _ = error "No hay personas que coincidan"
busca (Encolada p u ) car | (f' p) == Docente car = p
                          | otherwise  = busca u car 

--b)
-- Cola se parece al tipo Lista de algo, donde Vacia representa la lista Vacia, y Encolada representa una lista de elementos, en este caso una lista de personas. 1) "drop 1 xs" 2) " []++[p] ó xs++[p] 3) "find p xs"

-----------------------------------------Ejer 5)
data ListaAsoc a b = Vacia' | Nodo a b (ListaAsoc a b) deriving (Show, Eq)

type Diccionario = ListaAsoc String String
type Padron = ListaAsoc Int String

-----a)

type GuiaTel = ListaAsoc String Int

-----b)
--1)
la_long :: Integral c => ListaAsoc a b -> c 
la_long Vacia' = 0
la_long (Nodo _ _ (v)) = 1+ la_long v


--2)
la_concat :: (Eq a,Eq b) => ListaAsoc a b -> ListaAsoc a b -> ListaAsoc a b
la_concat Vacia' c = c 
la_concat d Vacia' = d
la_concat  (Nodo a b v) t | v == Vacia' = (Nodo a b (t))
                          | otherwise = Nodo a b (la_concat v (t))

--3)
la_pares :: ListaAsoc a b -> [(a,b)]
la_pares Vacia' = []
la_pares (Nodo a b v) = (a,b): la_pares v

--4)
la_busca :: Eq a => ListaAsoc a b -> a -> Maybe b
la_busca Vacia' _ = Nothing
la_busca (Nodo a b t) f | a == f = Just b 
                        | otherwise = la_busca t f

--5)
--Lo mismo que el tres

--6)
la_borrar :: Eq a => a -> ListaAsoc a b -> ListaAsoc a b 
la_borrar _ Vacia' = Vacia' 
la_borrar r (Nodo a b t) | r == a = la_borrar r t 
                         | otherwise = (Nodo a b (la_borrar r t))
						 
						 
-------c)
--El ejercicio 4 se podria resolver con nodos

encuentra :: Int -> ListaAsoc Int String -> String
encuentra _ Vacia' = ""
encuentra n (Nodo a b c) | n == a = b 
                         | otherwise = encuentra n c  



-------------------------------------- ejer 6)

data Arbol a = Hoja | Rama (Arbol a) a (Arbol a)
         deriving (Show, Eq)

type Prefijos = Arbol String

--a) 

a_long :: Integral b => Arbol a -> b
a_long Hoja = 1
a_long (Rama _ _ v) = 2 + a_long v

a_long' :: Integral b => Arbol a -> b
a_long' Hoja = 1 
a_long' (Rama t _ c) = 1 + a_long' t + a_long' c

--b)

a_hojas :: Integral b => Arbol a -> b
a_hojas Hoja = 1
a_hojas (Rama _ _ v) = 1 + a_hojas v

a_hojas' :: (Integral b, Eq a) => Arbol a -> b
a_hojas' Hoja = 1
a_hojas' (Rama a x t) | a == Hoja = 1 + a_hojas' t 
                      |otherwise = a_hojas' a + a_hojas' t

--c)

a_inc :: Num a => Arbol a -> Arbol a
a_inc Hoja = Hoja
a_inc (Rama a x b) = (Rama (a_inc a) (x+1) (a_inc b))

--d)

a_nombre :: Arbol Persona -> Arbol String
a_nombre Hoja = Hoja
a_nombre (Rama x (Per a n _ _ _) y) = Rama (a_nombre x) (a ++","++ n) (a_nombre y)

--e)

a_map :: (a -> b) -> Arbol a -> Arbol b
a_map _ Hoja = Hoja
a_map f (Rama a x b) = Rama (a_map f a) (f x) (a_map f b)

--punto c y d con a_map

a_inc' :: Num a => Arbol a -> Arbol a
a_inc' Hoja = Hoja
a_inc' (Rama a x b) = a_map (+1) (Rama a x b)

aux :: Persona -> String
aux (Per a n _ _ _) = a ++"," ++ n

a_nombre' :: Arbol Persona -> Arbol String
a_nombre' Hoja = Hoja
a_nombre' (Rama x z y) = a_map (aux) (Rama x z y)

--f)

a_sum :: Num a => Arbol a -> a
a_sum Hoja = 0
a_sum (Rama a x b) = x + (a_sum a) + (a_sum b)

--g)

a_prod :: Num a => Arbol a -> a
a_prod Hoja = 1
a_prod (Rama a x b) = x * (a_prod a) * (a_prod b)

