import Data.List
import Data.Maybe

------------------------ | Gerador1 | ------------------------

geradorDeInteiros :: Integer -> [[Integer]]
geradorDeInteiros n = [[x, negate x] | x <- [1 .. n]]

gerador1 :: Integer -> [Integer]
gerador1 n = 0 : concat (geradorDeInteiros(n))

------------------------ | Gerador2 | ------------------------

par :: Int -> Bool
par n = (n `mod` 2 == 0)

--gerador2 :: Integer -> [Integer]
gerador2 n = take n(map(\x -> if par x then negate x else x) $ iterate (+ 1) 1)

------------------------ | Gerador3 | ------------------------

gerador3 :: Integer -> [Integer]
gerador3 n = [2*x | x <- [1 .. n]]
------------------------ | Gerador4 | ------------------------

gerador4 :: Integer -> [Integer]
gerador4 n = takeWhile (>0) (iterate (`div` 2) n)

------------------------ | Gerador5 | ------------------------

--gerador5 :: String -> [Integer]

gerador5 n = map (\x -> read [x]::Int) n

--gerador5 = unfoldr foldr_
 --   where
 --     foldr_ 0 = Nothing 
 --     foldr_ x = Just (x, x `div` 2)

------------------------ | Cifra de Vigenere | ------------------------
