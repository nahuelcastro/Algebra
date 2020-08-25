 sumatoriaPares :: [Integer] -> Integer
 sumatoriaPares [] = 0
 sumatoriaPares (x:xs) | (mod x 2)== 0 = x+sumatoriaPares xs
  			| otherwise = sumatoriaPares xs


 filtro :: [Integer] -> Integer -> [Integer]
 filtro [] _ = []
 filtro (x:xs) n | (x==n) = (filtro xs n)
 				 | otherwise = x:(filtro xs n)

 inserta :: Integer -> [Integer] -> [Integer]
 inserta n [] = [n]
 inserta n (x:[]) | n <= x = [n,x]
 				  |otherwise = [x,n]
 inserta n (x:xs) | n <= x = n : (x:xs)
 				  |otherwise = x : inserta n xs

 ordenaPorInsercion :: [Integer] -> [Integer]
 ordenaPorInsercion [] = []
 ordenaPorInsercion [x]= [x]
 ordenaPorInsercion [x,y] | x<=y = [x,y]
 						  | otherwise = [y,x]
 ordenaPorInsercion (x:y:xs) | x<=y = x: ordenaPorInsercion (y:xs)
 							 | otherwise = ordenaPorInsercion (y:x:xs)

 comprueboOrden :: [Integer] -> Bool
 comprueboOrden []  = True
 comprueboOrden [_] = True
 comprueboOrden (x:y:xs) | (x<=y) && comprueboOrden (y:xs) = True
 						 |otherwise = False
 

 ordenoPosta :: [Integer] -> [Integer]
 ordenoPosta xs |  comprueboOrden(ordenaPorInsercion xs) = ordenaPorInsercion xs
 				| otherwise = ordenoPosta ( ordenaPorInsercion (ordenaPorInsercion xs) )

 invertir :: [Integer] -> [Integer]
 invertir [] = []
 invertir [x]=[x]
 invertir (x:xs) = invertir xs ++ [x]

 kesimo :: [Integer] -> Integer -> Integer
 kesimo [] _ = 0
 kesimo (x:xs) n | n==1 = x 
 				 | otherwise = kesimo xs (n-1)

 esTipoFibonacci :: [Integer] -> Bool
 esTipoFibonacci [] = True
 esTipoFibonacci [x] = True
 esTipoFibonacci [x,y] = True
 esTipoFibonacci [x,y,z] | x+y==z = True
 						 | otherwise = False
 esTipoFibonacci (x:y:z:xs)| x+y ==z && esTipoFibonacci (y:z:xs) = True
 						   | otherwise = False

--stirling :: Integer -> Integer -> Integer
--stirling _ 1 = 1 
--stirling n k | n==k = 1
--			| (k* stirling (n-1) k) + stirling (n-1) k








































