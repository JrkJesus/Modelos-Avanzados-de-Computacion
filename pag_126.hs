-- Implementar la función divisores
-- que recibe un argumento entero 
-- y que devuelva sus divisores.

divisores n = [ x | x<-[1..n], mod n x == 0]


-- Utilizando la función anterior, programar 
-- la función primo que devuelva verdadero en
-- caso de que su único argumento entero sea 
-- un número primo. No consideraremos al
-- número 1 como primo.

primo x = divisores x == [1, x]


-- Crear una expresión con la que se obtengan
-- los primos entre 1 y 100. Utilizar la notación
-- extendida de listas para este ejercicio

pr_1_100 = [x | x <- [1..100], primo x]


-- Averiguar cómo funcionan las funciones map 
-- y flter e implementarlas utilizando la
-- notación extendida de listas. 
-- Llamar a las nuevas funciones mapea y filtra.

mapea f a = [f x | x <- a]
filtra f a = [ x | x <- a, f x]


-- Programar una función evaluaciones con la siguiente cabecera
evaluaciones :: [a] -> [(a->b)] -> [[b]]
-- La lista de listas resultante contiene listas con los resultados 
-- de aplicar a cada uno de los valores de la primera lista las 
-- funciones de la segunda lista. 
evaluaciones xs fs = [[f x | f <- fs] | x <- xs]
doble x = 2*x
triple x = 3*x


-- Comprobar la funcion anterior con el caso: 
-- xs = [0,(3.14/2),((-3.14)/2), 3.14,(-3.14)] 
-- fs = [sin > 0, cos > 0, tg > 0]
-- Componer la lista utilizando el operador .
-- Resultado = [[False,False,True],
--				[True,False,False],
--				[False,False,False],
--				[True,False,False],
--				[False,False,False]]

xs = [0,(3.14/2),((-3.14)/2), 3.14,(-3.14)]
fs =  [((>0) . sin), ((==0) . cos), ((==0) . tan)]
compr = evaluaciones xs fs 


-- Implementar una función que devuelva 
-- la descomposición en factores primos 
-- de un número entero. 
-- La función devolverá una lista de tuplas
-- tal que la primera componente será el 
-- factor primo y la segunda será el número 
-- de veces que dicho factor primo divide
-- al argumento original.

-- descomposicion :: a -> [(a,b)]
descomposicion n
	| primo n = [(n, 1)]
	| n < 1 = error "Valor menor que 1"
	| otherwise =  union (mcd, 1) (descomposicion ( div n mcd))
		where mcd = head [ x | x <- [1..n], primo x && mod n x == 0]


union (n, n_count) ( (ns, ns_count) : resto )
	| n == ns = (n, ns_count+n_count) : resto
	| resto == [] = [(ns, ns_count), (n, n_count)]
	| otherwise =  (ns, ns_count) : (union (n, n_count) resto )

