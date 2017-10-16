divisible :: Int -> Int -> Bool
divisible x y = resto == 0
	where resto = mod x y

{- 
	Definicion local: Me creo una "funcion" y 
		defino dicha funcion en el resto.
-}