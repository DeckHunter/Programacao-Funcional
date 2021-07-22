import Debug.Trace ( trace )
import Data.Function ( (&) )

data Car = Car  { pass :: Int
                , maxPass :: Int
                , gas :: Int
                , maxGas :: Int
                , km :: Int
                } deriving (Eq, Show, Read)


data Op = Op { name :: String
             , result :: Bool
             } deriving (Eq, Show, Read)

data Info = Info { car :: Car
                 , op  :: Op
                 } deriving (Eq, Show, Read)


toString (Info (Car pass maxPass gas maxGas km) (Op name result)) =
                "Car pass: " ++ show pass ++ "/" ++ show maxPass
                 ++ " gas: " ++ show gas  ++ "/" ++ show maxGas
                 ++  " km: " ++ show km
                 ++ " Operation: " ++ name ++ " Result: " ++ show result

resume :: Info -> Info
resume info = trace (toString info) info

-- cria um carro passando maxPass e maxGas - retorna sempre true.
createCar :: Int -> Int -> Info
createCar maxPass maxGas = True

-- enche o tanque passando a qtd de gas. Retorna falso apenas se o tanque já estiver completamente cheio.
fuel :: Int -> Info -> Info
fuel qtdGas car 
    | (Info(car).gas) == (Info(car).maxGas)         = False    
    | otherwise                                     = (Info(car).gas + qtdGas)

-- Faz entrar uma pessoa no carro. Retorna false se já estiver lotado.
embark :: Info -> Info 
embark car
    |(Info(car).pass) == (Info(car).maxGas)     = Info(car)
    |otherwise                                  = (Info(car).pass + 1)

-- Retira uma pessoa do carro, retorna false se não tiver ninguém no carro
disembark :: Info -> Info
disembark car 
    |(Info(car).pass) == 0       = False
    |otherwise                   = (Info(car).pass - 1)

-- dirige diminuindo a gasolina e aumentando km. 
-- Só é possível dirigir se houver alguém no carro e houver alguma gasolina.
-- Aumenta a km da gasolina gasta.
-- retorna false se não há ninguém no carro ou se não tinha gasolina para completar a viagem.
drive :: Int -> Info -> Info 
drive x car
    |((Info(car).pass) >= 1) && ((Info(car).gas) >= 1)      = Info((car).gas - 1 (car).km + 1)
    |otherwise                                              = False