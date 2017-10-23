maximo :: Fractional a => a -> a -> a
maximo x y = ( (x + y) + abs ( x - y ) ) / 2.0