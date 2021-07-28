import qualified Data.Map as Map
import Data.Either
import Data.Map
data Estado = Alugado | Livre deriving (Show, Eq)
type Codigo = String
type Armario = (Estado, Codigo)
type Armarios = Map.Map Int (Estado, Codigo)

-- consulta : dado o número de um armário, retorna se ele está alugado ou não
-- retorna Nothing se o armário não existir
consulta :: Int -> Armarios -> Maybe Estado
consulta indice armarios = 
    case Map.lockers indice armarios of
        Nothing -> Left $ "Numero : " ++ show indice ++ " Não Existe"  
        Just (Estado, Codigo) -> if Estado /= Alugado   
                                then Right Codigo  
                                else Left $ "Bloqueado " ++ show indice ++ " Já Esta Alugado"  

-- dado o número do armário, retorna Right armário ou Left "Armario nao existe"
pegarArmario :: Int -> Armarios -> Either String Armario
pegarArmario indice armarios = 
    case Map.lockers indice armarios of
        Nothing -> Left $ "Armario : " ++ show armários ++ " Não Existe" 
        Just (Estado, Codigo) -> if Estado /= Alugado   
                                then Right Armario
                                else Left $ "Armario : " ++ show armários ++ " Já Esta Ocupado"

-- consultar: dado o número de um armário retorna "Right código"
-- Se não existir, retorna "Left Armario não existe", se já estiver alugado retorna "Left ja esta ocupado"
-- Utilize a função pegarArmario para obter o armário e verificar se ele existe
-- Não reescreva a mesma mensagem de erro, utilize a invocação do pegarArmário
pegarCodigo :: Int -> Armarios -> Either String Codigo
pegarCodigo indice armarios = if pegarArmario(indice,armarios) == Right Armario then consulta(indice, armarios)
        
     
-- aluguel - se o armário existir e estiver desocupado, o estado do armário no mapa é alterado para alugado
-- reaproveite a função pegarCodigo para obter a chave atual e os erros de "não existe" e "está ocupado"
aluguel :: Int -> Armarios -> Either String Armarios
aluguel indice armarios = 
    if pegarCodigo(indice, armarios) == Right Codigo then armario_
        where armario_ == pegarArmario(indice, armarios)
        
-- para devolver um armário é necessário que ele exista, que esteja alugado e que o código esteja correto
-- utilize a função pegar armário para verificar a existência.
-- Se o armário não estiver alugado ou se o código estiver incorreto avise utilizando o Left.
devolucao :: Int -> Codigo -> Armarios -> Either String Armarios
devolucao indice codigo armarios = 
    case Map.lockers indice armarios of
        Nothing -> Left $ "Armario : " ++ show armários ++ " Não Existe" 
        Just (Estado, Codigo) -> if Estado == Alugado && Codigo == codigo
                                then Right Armario
                                else Left $ "Codigo Do Armario : " ++ show armários ++ "Incorrento"


lockers :: Armarios
lockers = Map.fromList
    [(100,(Alugado,"ZD39I"))
    ,(101,(Livre,"JAH3I"))
    ,(103,(Livre,"IQSA9"))
    ,(105,(Livre,"QOTSA"))
    ,(109,(Alugado,"893JJ"))
    ,(110,(Alugado,"99292"))
    ]

main :: IO ()
main = do
    let armarios = lockers
    print $ armarios
    print $ consulta 100 armarios
    print $ consulta 101 armarios
    print $ consulta 200 armarios
    print $ pegarCodigo 100 armarios
    print $ pegarCodigo 101 armarios
    print $ pegarCodigo 200 armarios
    print $ aluguel 100 armarios
    print $ consulta 101 armarios
    let armarios1 = fromRight armarios $ aluguel 101 armarios
    print $ consulta 101 armarios1
    print $ fromLeft "" $ devolucao 103 "IQSA9" armarios1
    print $ fromLeft "" $ devolucao 101 "ZD39I" armarios1
    let armarios2 = fromRight armarios1 $ devolucao 110 "99292" armarios1
    print $ consulta 110 armarios2
