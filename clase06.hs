yLogico :: Bool -> Bool -> Bool
yLogico True True = True
yLogico _ _ = False

oLogico :: Bool -> Bool -> Bool
oLogico False False = True
oLogico _ _ = True

implica :: Bool -> Bool -> Bool
implica True False = False
implica _ _ = True

sumaGaussiana :: Integer -> Integer
sumaGaussiana 0 = 0
sumaGaussiana n = n + sumaGaussiana (n-1)

--algunaEsCero ::(Integer, Integer, Integer) -> Bool
--algunaEsCero 0 y z = True
--algunaEsCero x 0 z = True
--algunaEsCero x y 0 = True
--algunaEsCero _ _ _  = False 


productoInterno :: (Float, Float) -> (Float, Float) -> Float
productoInterno (x,y) (z,d) = x*z + y*d

sumaDigitos :: Integer -> Integer
sumaDigitos 0 = 0
sumaDigitos n = mod n 10 + sumaDigitos (div n 10)

digitosIguales :: Integer -> Bool   --hay que reformarlo
digitosIguales n | n<10 = True
	 |  (div (mod n 10 + mod (div n 10) 10) 2 == mod n 10) && digitosIguales(div n 10) = True 
	|otherwise = False  
	
	

--cantidadDigitos :: Integer -> Integer


------------------ CLASE 07 (Listas) ----------------------------

sumatoria :: [Integer] -> Integer
sumatoria lista | length lista == 0 = 0
	|otherwise = (head lista) + sumatoria (tail lista)

pertenece :: Integer -> [Integer] -> Bool
pertenece n lista |length lista==0 = False
	| (head lista)==n = True
	| otherwise = pertenece n (tail lista)

listar :: a -> a -> a -> [a]
listar a b c =  a : b : c : []

perteneceG :: (Eq a) => a -> [a] -> Bool
perteneceG b ls | length ls==0 = False
	| head ls == b = True
	| otherwise = perteneceG b (tail ls)

-- [(-100)..1]

primerMultiploDe45345 :: [Integer] -> Integer
primerMultiploDe45345 ls | (mod (head ls) 45345) == 0 = head ls
	| otherwise = primerMultiploDe45345 (tail ls)

--sumatoriaConPatterMatching :: [Integer] -> Integer
--sumatoriaConPatterMatching [] = 0
--sumatoriaConPatterMatching (x:ls)= x + sumatoriaConPatterMatching ls

perteneceGpm :: (Eq a) => a -> [a] -> Bool
perteneceGpm _ [] = False -- (Pertenece Generico (para cualquier tipo) con pattern matching)
perteneceGpm x (l:ls) | x==l = True
	| otherwise = perteneceGpm x ls

productoria :: [Integer] -> Integer
productoria [] = 1
productoria (l:ls) = l*(productoria ls)

sumarN :: Integer -> [Integer] -> [Integer]
sumarN n [] = []
sumarN n (x:xs) = (x+n) : (sumarN n xs) 

sumarElPrimero :: [Integer] -> [Integer]
sumarElPrimero [x] = [x+x]
sumarElPrimero (x:xs) = (x+x) : sumarN x xs

--obtenerElultimo :: [Integer] -> Integer
--obtenerElultimo [x] = x
--obtenerElultimo (x:xs) = xs:x:[] -- falta pensarlo y hacerlo
--sumarElUltimo :: [Integer] -> [Integer]

--falta termianr todos los ej de la clase 7 


























