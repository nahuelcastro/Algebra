
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

amigosDeIter :: Set Relaciones -> Usuario -> Set Usuario
amigosDeIter [] _ = []
amigosDeIter (relacion:relaciones) usuario | fst relacion == usuario = snd relacion : amigosDeIter relaciones
                                           | snd relacion == usuario = fst relacion : amigosDeIter relaciones 
                                           | otherwise = amigosDeIter relaciones

-- Dada una red social y un usuario retorna la cantidad de amigos de dicho usuario
cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos rs usuario = length(amigosDeIter (relaciones rs) usuario)

-- Dada una red social retorna el usuario con mas amigos. De existir más de uno devuelve cualquiera de ellos.
usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos rs = usuarioConMasAmigosIter usuarios rs 0

usuarioConMasAmigosIter :: Set Usuario -> Usuario -> Usuario
usuarioConMasAmigosIter (u:us) 0 = u
usuarioConMasAmigosIter (u:us) max | cantidadDeAmigos u > cantidadDeAmigos max = usuarioConMasAmigosIter us max
                                   | 


-- Dada una red social retorna True si algún usuario tiene más de un millón de amigos
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos rs | (cantidadDeAmigos rs) >= 1000000 = True
				     | otherwise = False

-- Dada una red social y un usuario retorna el conjunto de publicaciones del mismo.
publicacionesDe :: RedSocial -> Usuario -> Set Publicacion
publicacionesDe (us,rc,(pb:[])) usuario | usuarioDePublicacion pb == usuario = [pb]
										| otherwise = []
publicacionesDe (us,rc,(pb:pbs)) usuario |  usuarioDePublicacion pb == usuario = pb : publicacionesDe (us,rc,pbs)	
										 | otherwise = publicacionesDe (us,rc,pbs)									



--(  [(1,"jose") , (2,"pepe")]    ,  [((1,"jose") , (2,"pepe"))]   ,   [( (1,"jose"), "holaaa" , (2,"pepe") )]  )







-- Ejercicios

-- Dada una red social retorna un conjunto con los nombres de todos los usuarios.
--nombresDeUsuarios :: RedSocial -> Set String
--nombresDeUsuarios (usuario:[]) _ _ = nombreDeUsuario usuario : []
--nombresDeUsuarios (usuario:usuarios) rs ps = nombreDeUsuario usuario : nombresDeUsuarios usuarios rs ps

   --( [ (1,"nahuel"),(2,"pepe"),(3,"jose") ] , [ ( (1,"nahuel") , (2,"pepe") ) , ( ( 3,"jose"),(1,"nahuel") ) ] , [ (1,"nahuel") , "hola pepee", [(2,"pepe"), (3,"jose") ] ] )

 -- usuario: 			   (1,"nahuel") , (2,"pepe")
 -- relacion 				((1,"nahuel") , (2,"pepe"))
 --  publicacion 			( (1,"nahuel"), "hola hawai" , (2,"pepe") )

 --Red Soocial  :       (  [(1,"nahuel") , (2,"pepe")]    ,  [((1,"nahuel") , (2,"pepe"))]   ,   [( (1,"nahuel"), "hola hawai" , (2,"pepe") )]  )