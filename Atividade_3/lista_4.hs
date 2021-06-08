--------------- | Soma Impares | -------------- 

somaImpares :: [Integer] -> Integer
somaImpares [] = 0
somaImpares (x:xs) = if (mod x 2) /= 0 then (x + somaImpares xs) else somaImpares xs

--------------- | Max 3 | -------------- 

max3 :: Integer -> Integer -> Integer -> Integer
max3 x y z
    | x >= y && x >= z      = x
    | y >= x && y >= z      = y
    | z >= x && z >= y      = z

--------------- | Fatorial | --------------

fatorial :: Integer -> Integer

fatorial 0 = 1
fatorial x = x * fatorial(x-1)

--------------- | Elemento | --------------

elemento :: [Integer] -> Integer -> Integer
elemento n xs = 0

--------------- | Pertence | --------------
pertence :: Integer -> [Integer] -> Bool
pertence n [] = False

pertence n (x:xs)
    | x == n        = True
    | x /= n        = pertence n xs

pertence n xs = pertence n xs

--------------- | Total | --------------
--total :: [Integer] -> Integer

--total = foldr(const (1+)) 0

--------------- | Maior | --------------
maior :: [Integer] -> Integer
maior [x] = x
maior (x : xs) = if (x > (maior xs)) then x else (maior xs)

--------------- | Corpo | --------------

corpo :: [Integer] -> [Integer]
corpo xs = init xs

--------------- | Paridade | --------------

paridade:: [Bool] -> Bool
paridade xs = odd impar_par 
    where
        impar_par = length (filter(== True) xs)

--------------- | Uniao | --------------

uniao :: [Integer] -> [Integer] -> [Integer]
uniao [] [] =[]
uniao [] (x:xs) =(x:xs)
uniao (x:xs) [] =(x:xs)
uniao (x:xs)(y:ys)
    | x < y             = x:uniao xs (y:ys)
    | x == y            = x: uniao xs ys
    | otherwise         = y: uniao (x:xs) ys

--------------- | Intersec | --------------
intersec :: [Integer] -> [Integer] -> [Integer]
intersec xs ys = reverse([ y | y <- ys, y `elem` xs ])