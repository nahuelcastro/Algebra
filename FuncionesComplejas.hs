module FuncionesComplejas
where
import FuncionesSimples

type Vector2 =(Float,Float)

cuadruple :: Num a => a -> a
cuadruple x = doble(doble x)

sumaTupla :: Num a => (a,a) -> a
sumaTupla t = suma (fst t) (snd t)

unidades :: Integer -> Integer
unidades x = mod (abs x) 10  

esPar :: Integer -> Bool
esPar x = mod x 2 ==0 

esImpar :: Integer -> Bool
esImpar x = mod x 2 /=0 

esMultiploDe :: Integer -> Integer -> Bool
esMultiploDe x y = mod x y == 0

sumaUnidades3 :: Integer -> Integer -> Integer -> Integer
sumaUnidades3 x y z= (unidades x) + (unidades y) + (unidades z)

todosImpares :: Integer -> Integer -> Integer -> Bool
todosImpares x y z= (esImpar x) && (esImpar y) && (esImpar z)

alMenosUnImpar:: Integer -> Integer -> Integer -> Bool
alMenosUnImpar x y z= (esImpar x) || (esImpar y) || (esImpar z) 

alMenosDosImpares:: Integer -> Integer -> Integer -> Bool
alMenosDosImpares x y z | (esImpar x) && (esImpar y) = True
			| (esImpar x) && (esImpar z) = True
			| (esImpar y) && (esImpar z) = True
			| otherwise = False

-- falta hacer el de alMenosDosPares

r1 :: Integer -> Integer -> Bool
r1 a b | (esPar a) && (esPar b) = True	
	| (esImpar a) && (esImpar b) = True
	| otherwise= False

r2 :: Integer -> Integer -> Bool
r2 a b = esMultiploDe (2*a + 3*b) 5

r3 :: Integer -> Integer -> Bool
r3 a b = (unidades a) /= (unidades b) && (unidades a) /= (unidades (a*b)) && (unidades b) /= (unidades (a*b))
--
r8 :: Float -> Float -> Bool
r8 x y | x<3 && y<3 = True
       | x>=3 && y>= 3 = True
	| otherwise = False

r9 :: Float -> Float -> Bool
r9 x y | x<3 && y<3 = True
       | x>=3 && x<7 && y>=3 && y<7 = True
       | x>=7 && y>= 7 = True
       | otherwise = False

r102 :: Vector2 -> Vector2 -> Bool
r102 (a,b) (p,q) = a*q==b*p

--floor(a)= parte entera de a

r101 :: Vector2 -> Vector2 -> Bool
r101 (a,b) (p,q) = a*q==b*p && floor(a*q)== a*q 


