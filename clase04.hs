--hay que hacer faltorial n 
factorial :: Integer -> Integer
factorial n | n == 0 = 1
	| n > 0 = n*(factorial (n-1))

fib :: Integer -> Integer
fib n | n == 0 = 0
	| n == 1 = 1
	| otherwise = fib(n-1)+fib(n-2)

e3a :: Integer -> Integer
e3a n | n==1 = 2
	| otherwise = 2*n*(e3a (n-1))+2^(n+1)*factorial(n)
	
-- falta hacer e3b y e3c 

f1 :: Integer -> Integer
f1 n | n==0=1
	| n>0 = 2^n + f1(n-1)

f2 :: Integer -> Float -> Float
f2 n q | n==1 = q
	| n>1 = q^n + f2(n-1)(q)

f3 :: Integer -> Float -> Float
f3 n q | n==0 = 0.0
	|n>=1 = q^(2*n) + f3(n-1)(q)

f4 :: Integer -> Float -> Float
f4 n q | n==0 = 0.0
	|n>=1 = (f3 n q) - (f2 n q)

e20 :: Integer -> Bool
e20 n |n==0=True
	|n==1 || n==2 ||n<0= False 
	|n>0= e20(n-3)

esPar :: Integer -> Bool
esPar x = mod x 2 ==0 

esImpar :: Integer -> Bool
esImpar x = mod x 2 /=0 

sumaImpares :: Integer -> Integer
sumaImpares n | n==0=0
	| n==1=1
	| esPar(n)= sumaImpares(n-1)
	| esImpar(n)= n + sumaImpares(n-2)

---------------------------- CLASE 05----------------------------------


eAprox :: Integer -> Float
eAprox n | n==0=1
	|n>0= (fromInteger 1)/(fromInteger (factorial n) ) + eAprox (n-1) 

parteEntera :: Float -> Integer
parteEntera n | n<1 && n>=0 = 0
	|n>=1 = 1+parteEntera(n-1)
	|n<0 = parteEntera(n+1) - 1 

division :: Integer -> Integer -> (Integer,Integer)
division a d| a==0= (0,0)
	|a<d=(0,a)
	|a>=d = (1 + fst qr +1 , snd qr)
	where qr = division(a-d) d

--divisionNeg :: Integer -> Integer -> (Integer,Integer)
--divisionNeg a d| a==0= (0,0)
--	| a>=0 && a<d=(0,a)
--	|a>=d = (1 + fst qr +1 , snd qr)
--	| a<0 = (1 + fst divisionNeg(a+d) d -1 , snd divisionNeg(a+d) d)
--	where qr = divisionNeg(a-d) d
	



--menorDivisor :: Integer -> Integer  
--menorDivisor n= menorDivisorDesde n 2

--menorDivisorDesde :: Integer -> Integer -> Integer
--menorDivisorDesde n k | mod n k ==0=k
--	|  not(mod n k) = menorDivisorDesde n (k+1)

--esPrimo :: Integer -> Bool
--esPrimo n| n==1=False
--	|menorDivisor n==n=False


sumatoriaDoble :: Integer -> Integer -> Integer
sumatoriaDoble n m| n==1= sumatoriaInterna m 1
	|n>1= sumatoriaInterna m n + sumatoriaInterna m (n-1)

sumatoriaInterna ::  Integer -> Integer -> Integer
sumatoriaInterna m i | m==1= i^1
	| m>1 = i^m + sumatoriaInterna(m-1) i




