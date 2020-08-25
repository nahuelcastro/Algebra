type Vector2 =(Float,Float)

doble :: Float -> Float
doble x = x + x 

cuadruple :: Float-> Float
cuadruple x = doble (doble x)

esPar :: Integer -> Bool
esPar x = mod x 2 ==0 

esMultiploDe :: Integer -> Integer -> Bool
esMultiploDe x y = mod x y == 0
 

normaVectorial :: Vector2 -> Float
normaVectorial p = sqrt( (fst p)**2 + (snd p)**2)

crearPar :: a -> b -> (a, b)
crearPar x y= (x,y)

invertir :: (a, b) -> (b, a)
invertir (x,y)= (y,x)


distanciaPuntos :: Vector2 -> Vector2 -> Float
distanciaPuntos (x,y) (z,w)= (normaVectorial (x-z, y-w))    

f1 x = (2*x,x^2,x-7)
f2 :: Integer -> Integer
f2 n | esPar n= div n 2
     | otherwise= n + 1

f3 :: Integer -> Integer
f3 n |esMultiploDe n 6= div (n^2) 2
     |otherwise= 3*n + 1

g :: (Integer, Integer) -> Integer
g (n, m) = n*(m+1)

h :: (Integer, Integer) -> Integer
h (x, y) = f3 (g (x, y))
