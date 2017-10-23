primero :: Num a => a -> a -> a
primero x _ = x


suma x y z = x + y + z

mi_map list = map mi_func list
	where mi_func = (suma 1 1)