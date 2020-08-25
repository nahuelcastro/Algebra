doble x = 2 * x

signo n | n == 0 = 1
	| n > 0 = 1
	| n < 0 = (-1)

mayor x y |x >= y = x
           |x <= y = y

mayor3 x y z | (mayor x y) >= z = (mayor x y)
             | (mayor x y) <= z = z



