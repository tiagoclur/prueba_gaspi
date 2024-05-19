import Test.Hspec
import Ejemplo

main :: IO ()
main = hspec $ do
  describe "doble multiplica a un número por 2" $ do
    it "el doble de 2 es 4" $ do
      doble 2 `shouldBe` 4

----------       TEST DEL TP DE FUNCIONAL:

  describe "Ciudad con nombre raro" $ do
    it "la ciudad Maipu tiene 5 letras. No es una ciudad de nombre raro" $ do
      ciudadConNombreRaro maipu `shouldBe` False

    it "la ciudad Azul tiene 4 letras. Es una ciudad de nombre raro" $ do
      ciudadConNombreRaro maipu `shouldBe` True

--  describe "Eventos" $ do
--    it "Agrego una nueva atraccion a la ciudad" $ do
--      agregarNuevaAtraccion azul "Balneario Municipal Alte. Guillermo Brown" `shouldBe` Ciudad {nombre = "Azul", anioFundacion = 1832, atracciones = ["Teatro Espaniol","Parque Municipal Sarmiento","Costanera Cacique Catriel","Balneario Municipal Alte. Guillermo Brown"], costoDeVida = 228}
