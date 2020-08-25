f x y = x*x+y*y
g x y z = x+y+z*z
-- comentario

doble x = 2 * x
suma x y = x+y
normavectorial x1 x2 = sqrt (x1**2+x2**2)
funcionConstante8 x = 8 

signo n | n==0=0 
        | n>0=1
        | n<0= (-1) 

valorAbsoluto n= n*(signo n)

maximo x y z |x>y && x>z=x 
             |y>z && y>z y=x



