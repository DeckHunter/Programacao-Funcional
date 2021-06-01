--------------- | Soma Impares | -------------- 

somaImpares :: [Integer] -> Integer
somaImpares [] = 0
somaImpares (x:xs) = if (mod x 2) /= 0 then (x + somaImpares xs) else somaImpares xs