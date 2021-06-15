---------------------- |Fibonacci| ---------------------- 

fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

---------------------- |Frequencia| ---------------------- 

frequencia :: Integer -> [Integer] -> Integer
frequencia y [] = 0
frequencia y (x:xs) 
        | y == x         = 1 + frequencia y xs
        | otherwise      = frequencia y xs

---------------------- |Concatena| ---------------------- 

concatena :: [Integer] -> [Integer] -> [Integer]
concatena [] [] = []
concatena xs [] = xs
concatena [] ys = ys
concatena (x:xs)(y:ys) = x : y : concatena xs ys

---------------------- |Unico| ---------------------- 

unico :: Integer -> [Integer] -> Bool
unico y [] = False
unico y [x] = if y == x then True else False
unico y (x:xs)
    | y == x && unico y xs == False      = True
    | y == x && unico y xs == True       = False
    | otherwise                          = unico y xs

---------------------- |Reverso| ---------------------- 

reverso :: [Integer] -> [Integer]  
reverso [] = []  
reverso (x:xs) = reverso xs ++ [x]  

---------------------- |Menores| ----------------------

menores :: (Ord a) => Int -> [a] -> [a]  
menores n [] = []  
menores n (x:xs) = take n (min)
    where min =
            let smallerSorted = menores n [a | a <- xs, a <= x]  
                biggerSorted = menores n [a | a <- xs, a > x] 
            in  smallerSorted ++ [x] ++ biggerSorted

---------------------- |Line| ----------------------

line :: Integer -> [Integer]
soma n = sum [1..n]
line n = [soma (n-1)+1..soma n]

---------------------- |Triangle| ----------------------

triangle :: Integer -> [[Integer]]
triangle n = [line m | m <- [1..n]]

---------------------- |Intercal| ----------------------
 
intercal :: [Integer] -> [Integer] -> [Integer]
intercal [] [] = []
intercal xs [] = xs
intercal [] ys = ys
intercal (x:xs)(y:ys) = x : y : intercal xs ys

---------------------- |Deletee| ----------------------

--deletee :: Integer -> [Integer] -> [Integer]
deletee n [] = []
deletee n (x:xs)    
    | n == x        = xs
    | n /= x        = x : deletee n xs

---------------------- |Listacc| ----------------------

listacc :: Num a => [a] -> [a]
listacc x = [sum $ take k x | k <- [1..length x]]

---------------------- |Quadperf| ----------------------



---------------------- |Alter| ----------------------

alter :: Integer -> [Integer]
alter 0 = []
alter n = alter (n-1) ++ n : [negate(n)]

---------------------- |Sequencia| ----------------------

sequencia :: Integer -> Integer -> [Integer]
sequencia 0 m = []
sequencia 1 m = [m]
sequencia n m = m : sequencia (n-1) (m+1)