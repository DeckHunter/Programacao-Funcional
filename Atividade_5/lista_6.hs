import Data.List

---------------------- |Tails - Data.List.tails| ----------------------

tails_1 :: [Integer] -> [[Integer]]
tails_1 (x:xs) = tails xs

---------------------- |Unique - Data.List.nub| ----------------------

removeDup :: [Integer] -> [Integer]
removeDup []     = []
removeDup [a]    = [a]
removeDup (x:xs) = x:(removeDup $ filter (/=x) xs)

unique :: [Integer] -> [Integer]
unique (x:xs) = removeDup (semRepeticoes)
    where semRepeticoes = sort (x:xs)
