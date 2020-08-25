type Set a = [a]
type Usuario = (Integer, String) -- (id, nombre)
type Relacion = (Usuario, Usuario) -- usuarios que se relacionan
type Publicacion = (Usuario, String, Set Usuario) -- (usuario que publica, texto publicacion, likes)
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

nombreDeUsuario :: Usuario -> String
nombreDeUsuario (_, nombre) = nombre 

usuarioDePublicacion :: Publicacion -> Usuario
usuarioDePublicacion (u, _, _) = u

likesDePublicacion :: Publicacion -> Set Usuario
likesDePublicacion (_, _, us) = us

-- Ejercicios

-- Ejercicios

-- Dada una red social retorna un conjunto con los nombres de todos los usuarios.
nombresDeUsuarios :: RedSocial -> Set String
nombresDeUsuarios ((usuario:[]),_,_) = nombreDeUsuario usuario : []
nombresDeUsuarios ((usuario:usuarioss),rs,ps) = nombreDeUsuario usuario : nombresDeUsuarios (usuarioss,rs,ps)

-- Dada una red social y un usuario retorna el conjunto de amigos del mismo
amigosDe :: RedSocial -> Usuario -> Set Usuario
amigosDe rs usuario = amigosDeIter (relaciones rs) usuario

amigosDeIter :: Set Relacion -> Usuario -> Set Usuario
amigosDeIter [] _ = []
amigosDeIter (relacion:relaciones) usuario | fst relacion == usuario = snd relacion : amigosDeIter relaciones usuario
                                           | snd relacion == usuario = fst relacion : amigosDeIter relaciones usuario 
                                           | otherwise = amigosDeIter relaciones usuario

-- Dada una red social y un usuario retorna la cantidad de amigos de dicho usuario
cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos rs usuario = length(amigosDeIter (relaciones rs) usuario)

-- Dada una red social retorna el usuario con mas amigos. De existir más de uno devuelve cualquiera de ellos.
{-usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos rs = usuarioConMasAmigosIter usuarios rs 0

usuarioConMasAmigosIter :: Set Usuario -> Usuario -> Usuario
usuarioConMasAmigosIter (u:us) 0 = u
usuarioConMasAmigosIter (u:us) max | cantidadDeAmigos u > cantidadDeAmigos max = usuarioConMasAmigosIter us max
                                   | -}

--- Dada una red social retorna True si algún usuario tiene más de un millón de amigos
{-estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos rs | (cantidadDeAmigos rs) >= 1000000 = True
				     | otherwise = False-}

-- Dada una red social y un usuario retorna el conjunto de publicaciones del mismo.
publicacionesDe :: RedSocial -> Usuario -> Set Publicacion
publicacionesDe [] _ = []
publicacionesDe (us,rc,(pb:[])) usuario | usuarioDePublicacion pb == usuario = [pb]
										| otherwise = []
publicacionesDe (us,rc,(pb:pbs)) usuario |  usuarioDePublicacion pb == usuario = pb : publicacionesDe (us,rc,pbs) usuario
										 | otherwise = publicacionesDe (us,rc,pbs) usuario

-- Dada una red social y un usuario retorna el conjunto de publicaciones a las que el usuario les dió like.
publicacionesQueLeGustanA :: RedSocial -> Usuario -> Set Publicacion
publicacionesQueLeGustanA [] _ = []
publicacionesQueLeGustanA (uss, rss,(pb:[])) usuario | elem usuario (likesDePublicacion(pb)) = pb:[]   -- Caso Base: de la RedSocial, que tiene una sola publicacion, evaluo si el usuario pertenece al conjunto de likes, Si pertenece lo agrego a la lista que quiero que me devuelva
											         | otherwise = []                                  -- si no el usuario no pertenece al conjunto de likes dado en la publicacion, pues no me sirve esa publicacion, devuelvo lista vacia 
publicacionesQueLeGustanA (uss,rss,(pb:pbs)) usuario | elem usuario (likesDePublicacion(pb)) = pb : publicacionesQueLeGustanA (uss,rss,pbs) usuario  -- si el usuario pertenece al conjunto de likes, entonces lo agrego al conjunto que voy a devolver, y hago el paso recursivo con la cola del conjunto
													 | otherwise = publicacionesQueLeGustanA (uss,rss,pbs) usuario


-- Dada una red social y dos usuarios indica si les gustan las mismas publicaciones
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones [] _ _ = False
lesGustanLasMismasPublicaciones ( _ , _ , []) _ _ = False 
lesGustanLasMismasPublicaciones (uss,rss,(pb:[])) usuario1 usuario2 | leDioLike usuario1 pb && leDioLike usuario2 pb = True
																	| otherwise = False
lesGustanLasMismasPublicaciones (uss,rss,(pb:pbs)) usuario1 usuario2 | leDioLike usuario1 pb && leDioLike usuario2 pb && lesGustanLasMismasPublicaciones (uss,rss,pbs) usuario1 usuario2 = True
																	 | otherwise = False

-- Funcion auxiliar, leDioLike, dado un usuario y una publicacion, retorna True en caso de que el usuario le haya dado like a dicha publicacion, y False si el usuario no le dio like
leDioLike :: Usuario -> Publicacion -> Bool
leDioLike _ (user,txt,[]) = False
leDioLike us pb | elem us (likesDePublicacion pb) = True
				| otherwise = False


-- Dada una red social y un usuario u, indica si existe un usuario que le puso like a todas las publicaciones de u.
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel = undefined

-- Dada una red social y dos usuarios, indica si existe una secuencia de usuarios relacionados para llegar del primero al segundo.
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos = undefined




-- Aca esta mi mini simulacro de red social que me tira erro, si alguno ve porque o hace otra que ande dejela
--(  [(1,"jose") , (2,"pepe")]    ,  [((1,"jose") , (2,"pepe"))]   ,   [( (1,"jose"), "holaaa" , (2,"pepe") )]  )