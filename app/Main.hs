import Control.Exception (ErrorCall (..), evaluate)
import T2
import Test.Hspec (Spec, describe, hspec, it, shouldBe, shouldThrow)
import Test.QuickCheck (quickCheck)

{-------------------------------------------}
{-----------------  TEST  ------------------}
{-------------------------------------------}

-- Pruebas para EJERCICIO 1 =============================
testsEjecicio1 :: Spec
testsEjecicio1 = describe "EJERCICIO 1" $ do
--   it "pruebas para primosHastaN" $ do
--     primosHastaN 50 `shouldBe` [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47]
--     primosHastaN 10 `shouldBe` [2, 3, 5, 7]
--     primosHastaN 1 `shouldBe` []
--     primosHastaN 2 `shouldBe` [2]


-- Propiedades para EJERCICIO 1
propiedadesEjecicio1 :: Spec
propiedadesEjecicio1 = describe "EJERCICIO 1 Propiedades" $ do
--   it "propiedades de factorizar" $ do
--     quickCheck prop_cantidadFactores

-- Pruebas para EJERCICIO 2 =============================
testsEjecicio1 :: Spec
testsEjecicio1 = describe "EJERCICIO 1" $ do
--   it "pruebas para primosHastaN" $ do
--     primosHastaN 50 `shouldBe` [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47]
--     primosHastaN 10 `shouldBe` [2, 3, 5, 7]
--     primosHastaN 1 `shouldBe` []
--     primosHastaN 2 `shouldBe` [2]


-- Propiedades para EJERCICIO 2
propiedadesEjecicio1 :: Spec
propiedadesEjecicio1 = describe "EJERCICIO 1 Propiedades" $ do
--   it "propiedades de factorizar" $ do
--     quickCheck prop_cantidadFactores

-- Pruebas para EJERCICIO 3 =============================
testsEjecicio1 :: Spec
testsEjecicio1 = describe "EJERCICIO 1" $ do
--   it "pruebas para primosHastaN" $ do
--     primosHastaN 50 `shouldBe` [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47]
--     primosHastaN 10 `shouldBe` [2, 3, 5, 7]
--     primosHastaN 1 `shouldBe` []
--     primosHastaN 2 `shouldBe` [2]


-- Propiedades para EJERCICIO 3
propiedadesEjecicio1 :: Spec
propiedadesEjecicio1 = describe "EJERCICIO 1 Propiedades" $ do
--   it "propiedades de factorizar" $ do
--     quickCheck prop_cantidadFactores

-- Pruebas para EJERCICIO 4 =============================
testsEjecicio1 :: Spec
testsEjecicio1 = describe "EJERCICIO 1" $ do
--   it "pruebas para primosHastaN" $ do
--     primosHastaN 50 `shouldBe` [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47]
--     primosHastaN 10 `shouldBe` [2, 3, 5, 7]
--     primosHastaN 1 `shouldBe` []
--     primosHastaN 2 `shouldBe` [2]


-- Propiedades para EJERCICIO 4
propiedadesEjecicio1 :: Spec
propiedadesEjecicio1 = describe "EJERCICIO 1 Propiedades" $ do
--   it "propiedades de factorizar" $ do
--     quickCheck prop_cantidadFactores

-- Pruebas para EJERCICIO 5 =============================
testsEjecicio1 :: Spec
testsEjecicio1 = describe "EJERCICIO 1" $ do
--   it "pruebas para primosHastaN" $ do
--     primosHastaN 50 `shouldBe` [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47]
--     primosHastaN 10 `shouldBe` [2, 3, 5, 7]
--     primosHastaN 1 `shouldBe` []
--     primosHastaN 2 `shouldBe` [2]


-- Propiedades para EJERCICIO 5
propiedadesEjecicio1 :: Spec
propiedadesEjecicio1 = describe "EJERCICIO 1 Propiedades" $ do
--   it "propiedades de factorizar" $ do
--     quickCheck prop_cantidadFactores


{-------------------------------------------}
{------------------ MAIN -------------------}
{-------------------------------------------}

main :: IO ()
main = hspec $ do
  testsEjecicio1
  propiedadesEjecicio1

  testsEjecicio2
  propiedadesEjecicio2

  testsEjecicio3
  propiedadesEjecicio3

  testsEjecicio4
  propiedadesEjecicio4

  testsEjecicio5
  propiedadesEjecicio5