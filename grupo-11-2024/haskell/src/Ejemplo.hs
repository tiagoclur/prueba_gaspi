module Ejemplo where
import Text.Show.Functions()

doble :: Int -> Int
doble = (*2)

data Ciudad = Ciudad  {
    nombre :: String,
    anioFundacion :: Int,
    atracciones :: [String],
    costoDeVida :: Int 
} deriving (Show,Eq)

-- CASOS DE PRUEBA
baradero :: Ciudad
baradero = Ciudad{
    nombre = "Baradero",
    anioFundacion = 1615,
    atracciones = ["Parque del Este", "Museo Alejandro Barbich"],
    costoDeVida = 150
}

nullish :: Ciudad
nullish = Ciudad{
    nombre = "Nullish",
    anioFundacion = 1800,
    atracciones = [],
    costoDeVida = 140
}

caletaOlivia :: Ciudad
caletaOlivia = Ciudad{
    nombre = "Caleta Olivia",
    anioFundacion = 1901,
    atracciones = ["El Gorosito", "Faro Costanera"],
    costoDeVida = 120
}

maipu :: Ciudad
maipu = Ciudad  {
    nombre = "Maipu",
    anioFundacion = 1878,
    atracciones = ["Fortin Kakel"],
    costoDeVida = 115
}
azul :: Ciudad
azul = Ciudad  {
    nombre = "Azul",
    anioFundacion = 1832,
    atracciones = ["Teatro Español", "Parque Municipal Sarmiento", "Costanera Cacique Catriel"],
    costoDeVida = 190
}
--PUNTO 1 - VALOR DE UNA CIUDAD 
valorDe :: Ciudad -> Int
valorDe unaCiudad
  | anioFundacion unaCiudad < 1800 = unaOperacion 5 1800 (anioFundacion unaCiudad)
  | null (atracciones unaCiudad) = unaOperacion 2 0 (costoDeVida unaCiudad)
  | otherwise = unaOperacion 3 0 (costoDeVida unaCiudad)

unaOperacion :: Int -> Int -> Int -> Int
unaOperacion nro1 nro2 nro3 = (*nro1).abs.(subtract nro2) $ nro3

--PUNTO 2 - CARACTERISTICAS DE LAS CIUDADES 
--PUNTO 2.1 - Alguna atracción copada 
isVowel :: Char -> Bool
isVowel character = character `elem` "aeiouAEIOU"

existeAtraccionCopada :: Ciudad -> Bool
existeAtraccionCopada  = any isVowel . map head . atracciones 

--PUNTO 2.2 - Ciudad sobria 
type CantidadDeLetras = Int 
esSobria :: Ciudad -> CantidadDeLetras -> Bool
esSobria laCiudad xLetras = all (>xLetras). map length $ atracciones laCiudad

--PUNTO 2.3 - Ciudad con nombre raro 
ciudadConNombreRaro :: Ciudad -> Bool
ciudadConNombreRaro = (<5).length.nombre

--PUNTO 3 - EVENTOS
--PUNTO 3.1 - Sumar una nueva atracción 
agregarNuevaAtraccion :: Ciudad -> String -> Ciudad
agregarNuevaAtraccion ciudad nuevaAtraccion = ciudad {atracciones= (atracciones ciudad ++ [nuevaAtraccion] ), costoDeVida=(costoDeVida ciudad + (div (costoDeVida ciudad*2) 10)) }

{- Otra forma:

agregarA :: Ciudad -> String -> Ciudad
agregarA unaCiudad unaAtraccion = unaCiudad {
                                             atracciones = (atracciones unaCiudad ++ [unaAtraccion]), 
                                             costoDeVida = (`div` 5).(*6) $ costoDeVida unaCiudad 
                                            }
-}

--PUNTO 3.2 - Crisis (DUDA)
type ListaDeAtracciones = [String]
atraviesaUnaCrisis :: Ciudad -> Ciudad
atraviesaUnaCrisis ciudad = ciudad {
                                    atracciones = otraFuncionMas (atracciones ciudad) ,
                                    costoDeVida = (`div` 10).(*9) $ costoDeVida ciudad    
                                   }

otraFuncionMas :: ListaDeAtracciones -> ListaDeAtracciones
otraFuncionMas [] = []
otraFuncionMas unaLista = init unaLista

{-
para el caso nullish como no tiene atracciones nose si reducirle el costo de vida, 
pero por las dudas aca hice otra formula:

atraviesaUnaCrisis :: Ciudad -> Ciudad
atraviesaUnaCrisis ciudad = otraFuncionMas ciudad (atracciones ciudad)

otraFuncionMas :: Ciudad -> ListaDeAtracciones -> Ciudad
otraFuncionMas unaCiudad [] = unaCiudad
otraFuncionMas unaCiudad unaLista = unaCiudad {
                                    atracciones = init unaLista,
                                    costoDeVida = (`div` 10).(*9) $ costoDeVida unaCiudad    
                                   }
-}

{- DUDA GENERAL
  PARA EL CASO DE MAIPU ME TENDRIA QUE DAR 103.5 PERO ME DA 103
  POR ESO TENGO 2 DUDAS: UNA SOBRE SI USAR O NO fromIntegral y fromFractional CON LOS PORCENTAJES
  Y SOBRE COMO IMPLEMENTARLOS

  porque esto podria hacer que tengamos resultamos mas precisos con cada funcion del codigo
-}

--PUNTO 3.3 - Remodelación
type Porcentaje = Int
type CostoDeVida = Int

remodelar :: Porcentaje -> Ciudad -> Ciudad
remodelar unPorcentaje unaCiudad = unaCiudad {
                                              nombre = "New" ++ " " ++ (nombre unaCiudad),
                                              costoDeVida = otraOperacion 100 (100 + unPorcentaje) (costoDeVida unaCiudad)
                                             }

otraOperacion :: Int -> Int -> CostoDeVida -> CostoDeVida
otraOperacion unNumero otroNumero costo = (`div` unNumero).(*otroNumero) $ costo

--PUNTO 3.4 - Reevaluación
type Letras = Int

reevaluacion :: Letras -> Ciudad -> Ciudad
reevaluacion cantidadLetras unaCiudad =
  unaCiudad {costoDeVida = unaFuncion cantidadLetras unaCiudad}

unaFuncion :: Letras -> Ciudad -> CostoDeVida
unaFuncion unNumeroDeletras unaCiudad 
  | esSobria unaCiudad unNumeroDeletras = otraOperacion 100 (costoDeVida unaCiudad) 110
  | otherwise = unaOperacion 1 3 (costoDeVida unaCiudad) 

--HAY QUE CAMBIAR EL NOMBRE A ALGUNOS PARAMETROS PARA QUE SEA MAS INTUITIVO

--PUNTO 4 - LA TRANSFORMACION NO PARA
{-
  Reflejar de qué manera podemos hacer que una ciudad tenga:
  * El agregado de una nueva atracciona
  * Una remodelacion
  * Una Crisis
  * y una reevaluacion
  en la consola de GHCi.

  Copiar la consulta en forma de comentario en el archivo .hs.
-}
-- GHCI> 