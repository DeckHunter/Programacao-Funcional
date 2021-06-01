
-- Site Usado Pra Pesquisa http://haskell.tailorfontela.com.br/starting-out

--------------- | Soma 2 | -------------- 
soma x y = x + y 

--------------- | Minimo 2 | -------------- 
minimo2 x y = if x < y then x else y

--------------- | Minimo 3 | -------------- 
minimo3 x y z 
    | x < y && x < z    = x
    | y < x && y < z    = y
    | z < x && z < y    = z

--------------- | Gangorra | -------------- 
gangorra a b c d 
    | (a * b) == (c * d)    = 0
    | (a * b) > (c * d)    = -1 
    | otherwise             = 1

--------------- | CountNeg | -------------- 
countNeg :: [Integer] -> Int
countNeg xs = length( filter ( <= -1 ) xs )

--------------- | Miolo | -------------- 
interior :: [Integer] -> [Integer]
--interior xs = init  ++  tail xs
interior xs = init $ tail xs

--------------- | Final | -------------- 
final :: Int -> [Integer] -> [Integer]
final y xs = reverse $ (take y $ reverse xs)

--------------- | Iguais | -------------- 
iguais :: Int -> Int -> Int -> Int
iguais x y z 
    |  x == y && x == z                         = 3
    |  x == y && y /= z || x /= y && y == z     = 2
    |  otherwise                                = 0