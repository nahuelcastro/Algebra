{-            INTEGRANTES
   Lu: 190/19.     Zion Ezequiel Joaquin
   Lu: 58/19 .     Saied Martin Yoel
   Lu: 203/19.     Castro Russo Matias Nahuel --}

type Set a = [a]
type Usuario = (Integer, String)
type Relacion = (Usuario, Usuario)
type Publicacion = (Usuario, String, Set Usuario)
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
usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos rs = usuarioConMasAmigosIter rs 0

usuarioConMasAmigosIter :: RedSocial -> Int -> Usuario
usuarioConMasAmigosIter (us, rn, pn) max | length (us) == 1 = head us
                                         | cantidadDeAmigos rs (head us) < max = usuarioConMasAmigosIter rs' max
                                         | otherwise = usuarioConMasAmigosIter rs' nuevoMax --mando el máximo al final de la lista para que cuando llegue al caso base me asegure de que el último que queda es el más grande
                                         where rs = (us, rn, pn)
                                               rs' = (tail us, rn, pn)
                                               rs'' = (((tail us) ++ [head us]), rn, pn) --es igual que rs, sólo que mando al usuario con más amigos al final de la lista
                                               nuevoMax = cantidadDeAmigos (us, rn, pn) (head us)

--Dada una red social retorna True si algún usuario tiene más de un millón de amigos
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos rs | cantidadDeAmigos rs (usuarioConMasAmigos rs) >= 1000000 = True
                     | otherwise = False

-- Dada una red social y un usuario retorna el conjunto de publicaciones del mismo.
publicacionesDe :: RedSocial -> Usuario -> Set Publicacion
publicacionesDe (us,rc,[]) usuario = []
publicacionesDe (us,rc,pb) usuario | usuarioDePublicacion (head pb) == usuario = (head pb) : publicacionesDe (us,rc, tail pb) usuario
				                           | otherwise = publicacionesDe (us,rc, tail pb) usuario

-- Dada una red social y un usuario retorna el conjunto de publicaciones a las que el usuario les dió like.
publicacionesQueLeGustanA :: RedSocial -> Usuario -> Set Publicacion
publicacionesQueLeGustanA (us,rc,[]) usuario = []
publicacionesQueLeGustanA (us,rc,pb) usuario | elem usuario (likesDePublicacion (head pb)) = (head pb) : publicacionesQueLeGustanA (us,rc,(tail pb)) usuario
                                             | otherwise = publicacionesQueLeGustanA (us,rc,(tail pb)) usuario

-- Dada una red social y dos usuarios indica si les gustan las mismas publicaciones
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones rs user1 user2 = publicacionesQueLeGustanA rs user1 == publicacionesQueLeGustanA rs user2

-- Dada una red social y un usuario u, indica si existe un usuario que le puso like a todas las publicaciones de u.
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel rs user = tieneUnSeguidorFielIter rs (publicacionesDe rs user)

tieneUnSeguidorFielIter :: RedSocial -> Set Publicacion -> Bool
tieneUnSeguidorFielIter ([],rc,pb) publicacionesDelUsuario = False
tieneUnSeguidorFielIter (us,rc,pb) publicacionesDelUsuario | leDioLikeATodo (head us) publicacionesDelUsuario = True
                                                           | otherwise = tieneUnSeguidorFielIter (tail us,rc,pb) publicacionesDelUsuario

leDioLikeATodo :: Usuario -> Set Publicacion -> Bool
leDioLikeATodo user [] = True
leDioLikeATodo user (pb:pbs) | elem user (likesDePublicacion pb) = leDioLikeATodo user pbs
                             | otherwise = False

-- Dada una red social y dos usuarios, indica si existe una secuencia de usuarios relacionados para llegar del primero al segundo.
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos = undefined



-- Ejemplos

usuario1 = (1, "Juan")
usuario2 = (2, "Natalia")
usuario3 = (3, "Pedro")
usuario4 = (4, "Mariela")
usuario5 = (5, "Natalia")

relacion1_2 = (usuario1, usuario2)
relacion1_3 = (usuario1, usuario3)
relacion1_4 = (usuario4, usuario1) -- Notar que el orden en el que aparecen los usuarios es indistinto
relacion2_3 = (usuario3, usuario2)
relacion2_4 = (usuario2, usuario4)
relacion3_4 = (usuario4, usuario3)

publicacion1_1 = (usuario1, "Este es mi primer post", [usuario2, usuario4])
publicacion1_2 = (usuario1, "Este es mi segundo post", [usuario4])
publicacion1_3 = (usuario1, "Este es mi tercer post", [usuario2, usuario4])
publicacion1_4 = (usuario1, "Este es mi cuarto post", [])

publicacion2_1 = (usuario2, "Hello World", [usuario4])
publicacion2_2 = (usuario2, "Good Bye World", [usuario1, usuario4])

publicacion3_1 = (usuario3, "Lorem Ipsum", [])
publicacion3_2 = (usuario3, "dolor sit amet", [usuario2])
publicacion3_3 = (usuario3, "consectetur adipiscing elit", [usuario2, usuario4])

publicacion4_1 = (usuario4, "I am Alice. Not", [usuario1, usuario2])
publicacion4_2 = (usuario4, "I am Bob", [])
publicacion4_3 = (usuario4, "Just kidding, i am Mariela", [usuario1, usuario3])


usuariosA = [usuario1, usuario2, usuario3, usuario4]
relacionesA = [relacion1_2, relacion1_4, relacion2_3, relacion2_4, relacion3_4]
publicacionesA = [publicacion1_1, publicacion1_2, publicacion2_1, publicacion2_2, publicacion3_1, publicacion3_2, publicacion4_1, publicacion4_2]
redA = (usuariosA, relacionesA, publicacionesA)

usuariosB = [usuario1, usuario2, usuario3, usuario5]
relacionesB = [relacion1_2, relacion2_3]
publicacionesB = [publicacion1_1, publicacion1_2, publicacion1_3, publicacion1_4, publicacion3_1, publicacion3_2, publicacion3_3]
redB = (usuariosB, relacionesB, publicacionesB)