import Data.Char
import Data.String
import qualified Data.Map as Map  

---------------------- |Upper| ----------------------

upper :: String -> String
upper s = map toUpper(s)

---------------------- |Titulo| ----------------------

--titulo :: String -> String
titulo s = head(map toUpper(head palavras)) : tail(map toLower(head palavras))
    where palavras = words(s)

---------------------- |Selec| ----------------------
selec :: String -> [Integer] -> String


---------------------- |isPalind| ----------------------

isPalind :: String -> Bool
isPalind s = if s == reverse s then True else False
