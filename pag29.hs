iguales :: Eq a => a -> a -> a -> Bool
iguales x y z = x==y && y==z

{- 
	Sin cabecera: el tipo es eq porque es el mas
	generico que permite hacer una igualdad
-}

divide :: RealFloat a => a -> a -> a
divide x y = x / y

{- 
	{Cabecera} divide :: Num a => a -> a -> a
	{Error} divide usa la division y no se puede
			dividir si el numero es un real. 
			Como minimo tiene que ser un fractional
-}

--factorial :: Int -> Int
{- 
	{Cabecera} factorial :: Int -> Int
	{Error} el tipo int esta limitado entonces si
			tenemos factorial 20 sale negativo por
			sobrecarga
-}
factorial 1 = 1
factorial n = n * factorial (n-1)
{- 
	Ecuaciones: muy parecido a Prolog
-}


factorial2 n
	| n == 0 = 1
	| n > 0 = n * factorial (n-1)
	| otherwise = error "valor negativo"
{- 
	Guardas: cada linea con | es una opcion distinta
	TIENE QUE IR TODO EN LA MISMA LINEA
-}

factorial3 n = if (n==0) then 1 else n*factorial(n-1)
{- 
	IF-THEN estructure: 
-}

