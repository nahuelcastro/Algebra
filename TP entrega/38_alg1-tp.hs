{-    INTEGRANTES GRUPO 38:
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


-- EJERCICIOS OBLIGATORIOS 

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
usuarioConMasAmigos rs = usuarioConMasAmigosIter rs (usuarios rs) (head(usuarios rs))

usuarioConMasAmigosIter :: RedSocial -> Set Usuario -> Usuario -> Usuario
usuarioConMasAmigosIter _ [] max = max
usuarioConMasAmigosIter rs (x:xs) max | cantidadDeAmigos rs x < cantidadDeAmigos rs max = usuarioConMasAmigosIter rs xs max
                                      | otherwise = usuarioConMasAmigosIter rs xs x

--Dada una red social retorna True si algún usuario tiene más de un millón de amigos
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos (us,[],pb) = False
estaRobertoCarlos rs | cantidadDeAmigos rs (usuarioConMasAmigos rs) > 1000000 = True
                     | otherwise = False

-- Dada una red social y un usuario retorna el conjunto de publicaciones del mismo.
publicacionesDe :: RedSocial -> Usuario -> Set Publicacion
publicacionesDe (us,rc,[]) usuario = []
publicacionesDe (us,rc,pb) usuario | usuarioDePublicacion (head pb) == usuario = (head pb) : publicacionesDe (us,rc, tail pb) usuario
                                   | otherwise = publicacionesDe (us,rc, tail pb) usuario

-- Dada una red social y un usuario retorna el conjunto de publicaciones a las que el usuario les dió like.
publicacionesQueLeGustanA :: RedSocial -> Usuario -> Set Publicacion
publicacionesQueLeGustanA (us,rc,[]) usuario = []
publicacionesQueLeGustanA (us,rc,(p:ps)) usuario | elem usuario (likesDePublicacion p) = p : publicacionesQueLeGustanA (us,rc,ps) usuario
                                                 | otherwise = publicacionesQueLeGustanA (us,rc,ps) usuario

-- Dada una red social y dos usuarios indica si les gustan las mismas publicaciones
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones rs u1 u2 = iguales (publicacionesQueLeGustanA rs u1) (publicacionesQueLeGustanA rs u2) -- la función iguales está definida más abajo

-- Dada una red social y un usuario u, indica si existe un usuario que le puso like a todas las publicaciones de u.
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel rs user = tieneUnSeguidorFielIter rs (publicacionesDe rs user)


tieneUnSeguidorFielIter :: RedSocial -> Set Publicacion -> Bool
tieneUnSeguidorFielIter (_,_,_) [] = False
tieneUnSeguidorFielIter ([],_,_) publicacionesDelUsuario = False
tieneUnSeguidorFielIter (us,rc,pb) publicacionesDelUsuario | (head us) == (usuarioDePublicacion (head publicacionesDelUsuario)) = tieneUnSeguidorFielIter (tail us,rc,pb) publicacionesDelUsuario
                                                           | leDioLikeATodo (head us) publicacionesDelUsuario = True
                                                           | otherwise = tieneUnSeguidorFielIter (tail us,rc,pb) publicacionesDelUsuario

-- Dado un usuario y una lista de publicaciones, determina si ese usuario le dio like a todas las publicaciones de la misma
leDioLikeATodo :: Usuario -> Set Publicacion -> Bool
leDioLikeATodo user [] = True
leDioLikeATodo user (pb:pbs) | elem user (likesDePublicacion pb) = leDioLikeATodo user pbs
                             | otherwise = False

-- FIN EJERCICIOS OBLIGATORIOS

-- EJERCICIO OPTATIVO 

-- Dada una red social y dos usuarios, indica si existe una secuencia de usuarios relacionados para llegar del primero al segundo.
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos rs u1 u2 = elem u2 (obtenerRedAmigosTotal rs [u1] []) --ObtenerRedAmigosTotal me devuelve un conjunto con todos los amigos relacionados al primero, me fijo si el segundo usuario se encuentra en dicho conjunto

-- Dada una red social y un conjunto de usuarios, devuelve todos los usuarios que sean directamente e indirectamente amigos de alguno de estos
obtenerRedAmigosTotal :: RedSocial -> Set Usuario -> Set Usuario -> Set Usuario
obtenerRedAmigosTotal rs [] [] = []
obtenerRedAmigosTotal rs (u:[]) [] = obtenerRedAmigosTotal rs (obtenerRedAmigosParcial rs [u]) (obtenerRedAmigosParcial rs [u]) --Primero caso, como la primera vez que se llame a esta funcion sera con un usuario unico y aux vacio, empiezo obteniendo los amigos de este y poniendolos a su vez en el aux
obtenerRedAmigosTotal rs us aux | iguales aux (removeDuplicates(aux ++ (obtenerRedAmigosParcial rs us))) = aux
                                | otherwise = obtenerRedAmigosTotal rs (obtenerRedAmigosParcial rs us) (removeDuplicates(aux ++ (obtenerRedAmigosParcial rs us)))

-- Dada una red social y un conjunto de usuarios, devuelve todos los usuarios que sean directamente amigos de alguno de estos
obtenerRedAmigosParcial :: RedSocial -> Set Usuario -> Set Usuario
obtenerRedAmigosParcial rs [] = []
obtenerRedAmigosParcial rs (u:us) = (amigosDe rs u) ++ obtenerRedAmigosParcial rs us

-- Dados dos conjuntos, compara si son iguales
iguales :: Eq a => Set a -> Set a -> Bool
iguales conj1 conj2 = incluido conj2 conj1 && incluido conj1 conj2

-- Dados dos conjuntos, verifica que cada elemento del primer conjunto esté en el segundo
incluido :: Eq a => Set a -> Set a -> Bool
incluido (y:ys) [] = False
incluido [] [] = True
incluido [] (z:zs) = True
incluido (x:xs) conj2 = elem x conj2 && incluido xs conj2

-- Quita usuarios duplicados de una lista de usuarios 
removeDuplicates :: Set Usuario -> Set Usuario
removeDuplicates [] = []
removeDuplicates (u:us) | elem u us = removeDuplicates us
                        | otherwise = u : removeDuplicates us


-- FIN EJERCICIO OPTATIVO



--Redes de ejemplo para testear
usuario1 = (1, "Juan")
usuario2 = (2, "Natalia")
usuario3 = (3, "Pedro")
usuario4 = (4, "Mariela")
usuario5 = (5, "Natalia")

relacion1_2 = (usuario1, usuario2)
relacion1_3 = (usuario1, usuario3)
relacion1_4 = (usuario4, usuario1)
relacion2_3 = (usuario3, usuario2)
relacion2_4 = (usuario2, usuario4)
relacion3_4 = (usuario4, usuario3)
relacion5_3 = (usuario5, usuario3)

publicacion1_1 = (usuario1, "Este es mi primer post", [usuario2, usuario4])
publicacion1_2 = (usuario1, "Este es mi segundo post", [usuario4])
publicacion1_3 = (usuario1, "Este es mi tercer post", [usuario2, usuario5])
publicacion1_4 = (usuario1, "Este es mi cuarto post", [])
publicacion1_5 = (usuario1, "Este es como mi quinto post", [usuario5])
publicacion1_6 = (usuario1, "Este debe ser como mi sexto post", [usuario3,usuario5])

publicacion2_1 = (usuario2, "Hello World", [usuario4])
publicacion2_2 = (usuario2, "Good Bye World", [usuario1, usuario4])
publicacion2_3 = (usuario2, "Hello World, I Revived Like A Zombie", [usuario1,usuario3, usuario5])

publicacion3_1 = (usuario3, "Lorem Ipsum", [usuario3])
publicacion3_2 = (usuario3, "dolor sit amet", [usuario2,usuario3])
publicacion3_3 = (usuario3, "consectetur adipiscing elit", [usuario2, usuario5,usuario3])

publicacion4_1 = (usuario4, "I am Alice. Not", [usuario1, usuario2])
publicacion4_2 = (usuario4, "I am Bob", [])
publicacion4_3 = (usuario4, "Just kidding, i am Mariela", [usuario1, usuario3])


usuariosA = [usuario1, usuario2, usuario3, usuario4]
relacionesA = [relacion1_2, relacion1_4, relacion2_3, relacion2_4, relacion3_4]
publicacionesA = [publicacion1_1, publicacion1_2, publicacion2_1, publicacion2_2, publicacion3_1, publicacion3_2, publicacion4_1, publicacion4_2]
redA = (usuariosA, relacionesA, publicacionesA)

usuariosB = [usuario1, usuario2, usuario3, usuario5]
relacionesB = [relacion1_2, relacion2_3]
publicacionesB = [publicacion1_3, publicacion1_4, publicacion1_5, publicacion3_1, publicacion3_2, publicacion3_3]
redB = (usuariosB, relacionesB, publicacionesB)

usuariosC = [usuario1,usuario2,usuario3, usuario4, usuario5]
relacionesC = []
publicacionesC = [publicacion1_6, publicacion1_4]
redC = (usuariosC, relacionesC, publicacionesC)




{- TEST: Red A                   -- (excepto en LesGustanLasMismasPublicaciones, usuarioConMasAmigos y LesGustanLasMismasPublicaciones. ya que testeamos con tambien redC )
¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
nombresDeUsuarios redA ~~~> ["Juan","Natalia","Pedro","Mariela"]

amigosDe redA usuario1 ~~~> [(2,"Natalia"),(4,"Mariela")]
amigosDe redA usuario2 ~~~> [(1,"Juan"),(3,"Pedro"),(4,"Mariela")]
amigosDe redA usuario4 ~~~> [(1,"Juan"),(2,"Natalia"),(3,"Pedro")]
amigosDe redA usuario5 ~~~> []     

cantidadDeAmigos redA usuario1 ~~~> 2
cantidadDeAmigos redA usuario2 ~~~> 3
cantidadDeAmigos redA usuario3 ~~~> 2
cantidadDeAmigos redA usuario4 ~~~> 3
cantidadDeAmigos redA usuario5 ~~~> 0  

usuarioConMasAmigos redA ~~~> (4,"Mariela")
usuarioConMasAmigos redC ~~~> (5,"Natalia")  --En una red social que no tiene definida ninguna relacion de amistad, el usuario con mas amigos es cualquiera de los que pertenecen a la red 

estaRobertoCarlos redA ~~~> False  (este caso de test es con la funcion original, que solo devuelve True si algun usuario tiene mas de un millon de amigos)
estaRobertoCarlos redA ~~~> True  (para no crear una red con un millon de amigos, testeamos diciendo que devuelva True si algun usuario tiene mas de 2 amigos). Se cambio la linea de codigo 69 por esta:    estaRobertoCarlos rs | cantidadDeAmigos rs (usuarioConMasAmigos rs) > 2 = True

publicacionesDe redA usuario2 ~~~>
[((2,"Natalia"),"Hello World",[(4,"Mariela")]),((2,"Natalia"),"Good Bye World",[(1,"Juan"),(4,"Mariela")])]


publicacionesQueLeGustanA redA usuario1 ~~~>
[((2,"Natalia"),"Good Bye World",[(1,"Juan"),(4,"Mariela")]),((4,"Mariela"),"I am Alice. Not",[(1,"Juan"),(2,"Natalia")])]

lesGustanLasMismasPublicaciones redA usuario1 usuario2 ~~~> False
lesGustanLasMismasPublicaciones redC usuario3 usuario5 ~~~> True  -- creamos una nueva redC para crear porque ni en redA ni redB habia un caso donde se cumpla y devuelva True, excluyendo los casos triviales.
lesGustanLasMismasPublicaciones redC usuario1 usuario4 ~~~> True  -- caso trivial: Si hay dos usuarios en la red que no dieron like a ninguna publicaci´ony se pregunta si dieron like a las mismas pubicaciones, la respuesta esVerdadera
lesGustanLasMismasPublicaciones redA usuario1 usuario1 ~~~> True  -- es verdadero que a un usuario le gustan las mismas publicaciones que a ese mismo usuario.

tieneUnSeguidorFiel redA usuario1 ~~~> True
tieneUnSeguidorFiel redA usuario2 ~~~> True
tieneUnSeguidorFiel redA usuario3 ~~~> False  -- (notar que todas las publicaciones de usuario3 en redA, fueron likeadas por usuario3, pero nuestro algoritmo no "toma en cuenta" los likes propios)

existeSecuenciaDeAmigos redA usuario1 usuario3 ~~~> True
existeSecuenciaDeAmigos redA usuario1 usuario4 ~~~> True
-}

{- TEST: Red B
¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
nombresDeUsuarios redB ~~~> ["Juan","Natalia","Pedro","Natalia"]

amigosDe redB usuario1 ~~~> [(2,"Natalia")]
amigosDe redB usuario2 ~~~> [(1,"Juan"),(3,"Pedro")]
amigosDe redB usuario3 ~~~> [(2,"Natalia")]
amigosDe redB usuario5 ~~~> []

cantidadDeAmigos redB usuario1 ~~~> 1
cantidadDeAmigos redB usuario2 ~~~> 2
cantidadDeAmigos redB usuario3 ~~~> 1
cantidadDeAmigos redB usuario5 ~~~> 0

usuarioConMasAmigos redB ~~~> (2,"Natalia")

estaRobertoCarlos redB ~~~> False (este caso es pidiendo que algun usuario de la red tenga mas de un millon de amigos)
estaRobertoCarlos redB ~~~> False (este caso es pidiendo que algun usuario de la red tenga mas de 2 amigos) se cambio la linea de codigo 69 por esta:  estaRobertoCarlos rs | cantidadDeAmigos rs (usuarioConMasAmigos rs) > 2 = True


publicacionesDe redB usuario1 ~~~> [((1,"Juan"),"Este es mi tercer post",[(2,"Natalia"),(5,"Natalia")]),((1,"Juan"),"Este es mi cuarto post",[]),((1,"Juan"),"Este es como mi quinto post",[(5,"Natalia")])]
publicacionesDe redB usuario2 ~~~> []
publicacionesDe redB usuario3 ~~~> [((3,"Pedro"),"Lorem Ipsum",[(3, "Pedro")]),((3,"Pedro"),"dolor sit amet",[(2,"Natalia"),(3, "Pedro")]),((3,"Pedro"),"consectetur adipiscing elit",[(2,"Natalia"),(5,"Natalia"),(3, "Pedro")])]
publicacionesDe redB usuario5 ~~~> []

publicacionesQueLeGustanA redB usuario1 ~~~> []
publicacionesQueLeGustanA redB usuario2 ~~~> [((1,"Juan"),"Este es mi tercer post",[(2,"Natalia"),(5,"Natalia")]),((3,"Pedro"),"dolor sit amet",[(2,"Natalia")]),((3,"Pedro"),"consectetur adipiscing elit",[(2,"Natalia"),(5,"Natalia")])]
publicacionesQueLeGustanA redB usuario3 ~~~> [((3,"Pedro"),"Lorem Ipsum",[(3,"Pedro")]),((3,"Pedro"),"dolor sit amet",[(2,"Natalia"),(3,"Pedro")]),((3,"Pedro"),"consectetur adipiscing elit",[(2,"Natalia"),(5,"Natalia"),(3,"Pedro")])]
publicacionesQueLeGustanA redB usuario5 ~~~> [((1,"Juan"),"Este es mi tercer post",[(2,"Natalia"),(5,"Natalia")]),((1,"Juan"),"Este es como mi quinto post",[(5,"Natalia")]),((3,"Pedro"),"consectetur adipiscing elit",[(2,"Natalia"),(5,"Natalia")])]

lesGustanLasMismasPublicaciones redB usuario1 usuario3 ~~~> False
lesGustanLasMismasPublicaciones redB usuario1 usuario2 ~~~> False
lesGustanLasMismasPublicaciones redB usuario1 usuario4 ~~~> True

tieneUnSeguidorFiel redB usuario1 ~~~> False
tieneUnSeguidorFiel redB usuario2 ~~~> False
tieneUnSeguidorFiel redB usuario3 ~~~> False  -- (notar que todas las publicaciones de usuario3 en redA, fueron likeadas por usuario3, pero nuestro algoritmo no "toma en cuenta" los likes propios)
tieneUnSeguidorFiel redB usuario5 ~~~> False  -- Un usuario que no tiene publicaciones no tiene un Seguidor Fiel.


existeSecuenciaDeAmigos redB usuario1 usuario3 ~~~> True
existeSecuenciaDeAmigos redB usuario1 usuario5 ~~~> False
-}
