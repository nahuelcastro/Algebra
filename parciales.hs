divisores :: Integer -> Integer -> [Integer]
divisores 1 _ = [1]
divisores n m | mod m n == 0 = n : (divisores (n-1) m)
      | otherwise = divisores (n-1) m


esPrimo :: Integer -> Bool
esPrimo n | (divisores n n) == [n,1] = True
	  | otherwise = False

divisoresPrimos :: Integer -> Integer -> [Integer]
divisoresPrimos 1 _ = [1]
divisoresPrimos n m | esPrimo n && mod m n == 0 = n : (divisoresPrimos (n-1) m)
      | otherwise = divisoresPrimos (n-1) m


------
mod7 :: Integer -> Integer
mod7 0 = 0
mod7 n = mod n 7

sumaMod7Aux :: Integer -> [Integer] -> [Integer]
sumaMod7Aux _ [] = []
sumaMod7Aux n (x:[]) = []
sumaMod7Aux n (x:xs) =  ( mod7 (n + head xs) ): sumaMod7Aux n xs


--esSumaMod7DeDos :: Integer -> [Integer] -> Bool
--esSumaMod7DeDos _ [] = False
--esSumaMod7DeDos _ (x:[]) = False
--esSumaMod7DeDos n (x:xs) | (elem (mod7 n ) (sumaMod7Aux n (x:xs)) = True
--						 | otherwise = esSumaMod7DeDos n xs	
 
 -----------------------------PARCIAL-------------------------------------

--sumatoriaPares :: [Integer] -> Integer
--sumatoriaPares [] = 0
--sumatoriaPares (x:xs) | (mod x 2)== 0 =x:sumatoriaPares xs
--   			| otherwise = sumatoriaPares xs









