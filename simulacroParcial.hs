type Set a = [a]
type Usuario = (Integer, ) -- (id, nombre)
type Relacion = (Usuario, Usuario) -- usuarios que se relacionan
type Publicacion = (Usuario, [Char], Set Usuario) -- (usuario que publica, texto publicacion, likes)
type RedSocial = (Set Usuario, Set Relacion, Set Publicacion)

-- Funciones basicas

usuarios :: RedSocial -> Set Usuario
usuarios (us, _, _) = us

relaciones :: RedSocial -> Set Relacion
relaciones (_, rs, _) = rs

publicaciones :: RedSocial -> Set Publicacion
publicaciones (_, _, ps) = ps

idDeUsuario :: Usuario -> Integer
idDeUsuario (id, _) = id 




























aparicionesDe :: Integer -> [Integer] -> Integer
aparicionesDe n [] = 0	
aparicionesDe n (x:xs) | n==x = 1 + aparicionesDe n xs 
	|otherwise= aparicionesDe n xs

listaLimpia :: [Integer] -> [Integer]
listaLimpia [] = []
listaLimpia (x:xs) | elem x xs = listaLimpia xs
		| otherwise = x:(listaLimpia xs)

comprimir :: [Integer] -> [(Integer,Integer)]
comprimir [] = []
comprimir (x:[]) = [(x,1)]
comprimir (x:xs) = comprimirLimpio (listaLimpia(x:xs)) (x:xs)

comprimirLimpio :: [Integer] -> [Integer] -> [(Integer,Integer)] -- recibe lista limpia y sucia
comprimirLimpio [] _ = []
--comprimirLimpio (x:[]) = [(x,1)]
comprimirLimpio (l:ls) (x:xs) = ( l , aparicionesDe l (x:xs)) : (comprimirLimpio (ls) (x:xs) )




--1-- OK
menorLax :: (Float,Float,Float)->(Float,Float,Float) -> Bool
menorLax (x,y,z) (a,b,c) | x<a = True
			 | x==a && y<b =True
			 | x==a && y==b && z<c =True
			 | otherwise = False

--2-- OK
fibonacci :: Integer -> Integer
fibonacci 0 = 1
fibonacci 1 = 1
fibonacci n = fibonacci (n -1 ) + fibonacci (n -2 )

sumafibonacci :: Integer -> Integer
sumafibonacci 1 = 2 
sumafibonacci n = fibonacci n + sumafibonacci (n - 1) 

--3-- NO
esDivisorDe :: Integer -> Integer -> Bool
esDivisorDe 1 _ = True
esDivisorDe n d | mod d n == 0 = True
		| otherwise = False

divisoresPrimosDe :: Integer -> Integer -> [Integer]
divisoresPrimosDe 1 _ = []
divisoresPrimosDe n m | esDivisorDe (n-1) m = (n - 1) : divisoresPrimosDe (n-1) m
	 	      | otherwise = divisoresPrimosDe (n-1) m

--4-- OK
maximo :: [Integer] -> Integer
maximo [] = 0
maximo (x:xs) | x>= maximo xs = x
	      | otherwise = maximo xs

obtenerListasDistancias :: [Integer] -> [Integer]
obtenerListasDistancias [] = []
obtenerListasDistancias (x:[]) = []
obtenerListasDistancias (x:xs) = abs(abs(x) - abs(head xs)) : obtenerListasDistancias(xs)

maximaDistancia :: [Integer] -> Integer
maximaDistancia xs = maximo( obtenerListasDistancias (xs) )

-----------------------------PARCIAL 2do CUATRIMESTRE 2018 -----------------------------

--1--
suma2 :: (Integer,Integer,Integer) -> Integer -> Bool
suma2 (x,y,z) n | x+y == n || x+z==n || y+z==n = True
	|otherwise = False

--3--
todosIguales :: [Integer] -> Bool
todosIguales [] = True
todosIguales (x:[]) = True
todosIguales (x:xs) | x == head xs = todosIguales xs
		    | otherwise = False

--4--
sacarTodos :: [Integer] -> [Integer] -> [Integer]
sacarTodos xs [] = xs
sacarTodos [] _ = []
sacarTodos (x:xs) ys | elem x ys = sacarTodos xs ys
		     | otherwise = x : sacarTodos xs ys

--5--
notasLimpias :: [(Integer,Float)] -> Integer -> [Float]
notasLimpias [] _ = []
notasLimpias ((x,n):xs) y | x==y = n : (notasLimpias xs y)
			| otherwise = notasLimpias xs y 

promedio :: [Float] -> Float
promedio [] = 0.0
promedio xs = ( sumaNotas xs ) /  (fromIntegral (length xs))

sumaNotas :: [Float] -> Float
sumaNotas [] = 0.0
sumaNotas (x:xs) = x + sumaNotas xs

promedioDe :: [(Integer,Float)] -> Integer -> Float
promedioDe xs n = promedio (notasLimpias xs n )

----------------------PARCIAL ----------------------
--1--
sucesion :: Integer -> Integer
sucesion 0 = 0
sucesion  1 = 1
sucesion n = 1 + (n-1) * sucesion (n-1) + sucesion (n-2)

--2--
reglaDescartes ::  [Integer] -> Integer
reglaDescartes [] = 0
reglaDescartes (x:[]) = 0
reglaDescartes (x:y:[]) | x>0 && y<0 || x<0 && y>0 = 1 + reglaDescartes (y:[])
			| otherwise = reglaDescartes (y:[])
reglaDescartes (x:y:xs) | x>0 && y<0 || x<0 && y>0 = 1 + reglaDescartes (y:xs)
			| otherwise = reglaDescartes (y:xs)



















