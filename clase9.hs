mcd :: Integer -> Integer -> Integer
mcd a 0 = a
mcd a b | div a b == 0 = mcd b a
		| otherwise = mcd b (mod a b)


divisores :: Integer -> Integer -> [Integer]
divisores 1 _ = [1]
divisores n m | mod m n == 0 = n : (divisores (n-1) m)
      | otherwise = divisores (n-1) m

--supongo que tengo la funcion menorDivisor -- la comento porque falta programar menorDivisor 
--mayorDivisor :: Integer -> Integer -> Integer
--mayorDivisor 0 b = 0
--mayorDivisor a 0 = 0
--mayorDivisor  a 1 = a
--mayorDivisor 1 b = b
--mayorDivisor a b | menorDivisor a == menorDivisor b = (menorDivisor a) * mayorDivisor (div a menorDivisor (a)) (div b menorDivisor (b))
--				 | menorDivisor a > menorDivisor b = mayorDivisor a (div b menorDivisor (b))
--				 | menorDivisor a < menorDivisor b = mayorDivisor (div a menorDivisor (a)) b

emcd :: Integer -> Integer -> (Integer, Integer, Integer)
emcd a 0 = (a,1,0)
emcd a b = (g,s,t)
 where 
 (g,s',t') = emcd b (mod a b)
 s = t'
 t = s' - (t' * q)
 q = div a b

fst3 :: (Integer,Integer,Integer) -> Integer
fst3 (a,b,c) = a

snd3 :: (Integer,Integer,Integer) -> Integer
snd3 (a,b,c) = b

trd3 :: (Integer,Integer,Integer) -> Integer
trd3 (a,b,c) = c

ecDiofanticas :: Integer -> Integer -> Integer -> (Integer,Integer)
ecDiofanticas s t g | fst3 (emcd s t) == g = (snd3 (emcd s t) , trd3 (emcd s t))
					| otherwise =  ( (div g (mcd s t)) * snd3 (emcd s t) , (div g (mcd s t)) * trd3 (emcd s t) )


tieneSolucion :: Integer -> Integer -> Integer -> Bool
tieneSolucion a b m | mod (mcd a m ) b == 0 = True
					| otherwise = False

solucionParticular :: Integer -> Integer -> Integer -> Integer
solucionParticular a b m =  fst (ecDiofanticas a m b)
