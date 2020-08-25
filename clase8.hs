type Set a = [a]
type Natural = Integer

suma :: Integer -> Bool
suma n | n==8 || n==7 || n==6 || n==1 || n==4 = True
	| otherwise = False

todosDistintos :: [Integer] -> Bool
todosDistintos [] = True
todosDistintos (xs:[]) = True
todosDistintos (xh:x:xs) | xh /= x && todosDistintos (x:xs) = True
	|otherwise = False

vacio:: Set Integer
vacio = []

agregar :: Integer -> Set Integer -> Set Integer
agregar n xs | elem n xs = xs  -- "elem" es la funcion que te da true
	| otherwise= n:xs      -- si un elemento pertenece o no, a la lista
				-- tambien podira ser xs++[n]

incluido :: Set Integer -> Set Integer -> Bool
incluido [] _ = True
incluido (x:xs) ys = (elem x ys) && incluido xs ys

--incluido :: Set Integer -> Set Integer -> Bool
--incluido (x:xs) (y:ys)| (x:xs) == [] = True
--	| elem x (y:ys) && incluido xs (y:ys) = True
-- 	|otherwise= False


iguales :: Set Integer -> Set Integer -> Bool
iguales xs ys= incluido xs ys && incluido ys xs

-- lo de abajo va a tirar error hasta que cree la funcion eliminar repetidos

agregarATodos :: Integer -> Set ( Set Integer ) -> Set ( Set Integer)
agregarATodos n xss = eliminarRepetidos (agregarATodosAux n xss)


agregarATodosAux :: Integer -> Set (Set Integer) -> Set ( Set Integer)
agregarATodosAux _ [] = []
agregarATodosAux n (xs:xss) = ((agregar n xs):(agregarATodos n xss)) --tengo que crear una ffuncion que elimine los conjuntos repetidos 

eliminarRepetidos :: Set (Set Integer) -> Set (Set Integer)
eliminarRepetidos [[]] = [[]]
eliminarRepetidos (xs:xss) | iguales xs (head xss) = eliminarRepetidos xss 
	|otherwise= (xs:xss)


--partes :: Integer -> Set (Set Integer)
--partes 0 = [[]]
--partes n = agregarATodos n partes (n-1) ++ partes (n-1)

---

productoCartesiano :: Set Integer -> Set Integer -> Set (Integer, Integer)
productoCartesiano [] _ = []
productoCartesiano (x:xs) ls = (productoCartesianoAux x ls) ++ (productoCartesiano xs ls)

productoCartesianoAux :: Integer -> Set Integer -> Set (Integer, Integer) -- hace el producto cartesiano entre un integer y un set integer
productoCartesianoAux _ [] = []
productoCartesianoAux n (x:xs) = (n,x) : productoCartesianoAux n xs



--variaciones :: Set Integer -> Integer -> Set [Integer] -- agarro todas las variaciones de la lista para longitud n-1, o sea una anterior, y le agrego adelante las nuevas posibles opciones que le falta. que es agregarle el primer entero a cada uno de los posibles nateriores
--variaciones _ 0 = [[]]
--variaciones ls n = agregarUnConjuntoATodasLasListas ls (variaciones ls (n-1))

--agregarUnEnteroATodasLasListas :: Integer -> Set [Integer] -> Set [Integer]
--agregarUnEnteroATodasLasListas _ [] = []
--agregarUnEnteroATodasLasListas n (xs:xss) = (n:xs) : (agregarUnEnteroATodasLasListas n xss)

--agregarUnConjuntoATodasLasListas :: Set Integer -> Set [Integer] -> Integer
--agregarUnConjuntoATodasLasListas [] lss = []
--agregarUnConjuntoATodasLasListas (x:xs) lss = (agregarUnEnteroATodasLasListas x lss) ++  (agregarUnConjuntoATodasLasListas xs lss)





















