
---------------------- |ProdutoEscalar - utilizando a função zip| ----------------------
produtoEscalar :: [Integer] -> [Integer] -> Integer
produtoEscalar  xs ys = sum [ x*y | (x,y) <- zip xs ys]

---------------------- |Indices - busca posições do elemento| ----------------------


