import T2
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck (quickCheck)

{-------------------------------------------}
{-----------------  TEST  ------------------}
{-------------------------------------------}

-- Pruebas para EJERCICIO 1 =============================

-- Parte (b): evalCF
testsEjecicio1ParteB :: Spec
testsEjecicio1ParteB = describe "EJERCICIO 1" $ do
  it "pruebas para 1.b) evalCF" $ do
    evalCF (Simple 5)  `shouldBe`  (5, 1)
    evalCF (Compound 1 2 (Simple 3))  `shouldBe`  (5, 3)
    evalCF t `shouldBe` (649, 200)

-- Parte (c): degree
testsEjecicio1ParteC :: Spec
testsEjecicio1ParteC = describe "EJERCICIO 1.c) degree" $ do
  it "pruebas para degree" $ do
    degree t `shouldBe` 3  -- Ajustar según el grado esperado de `t`

-- Parte (d): foldCF
testsEjecicio1ParteD :: Spec
testsEjecicio1ParteD = describe "EJERCICIO 1.d) foldCF" $ do
  it "pruebas para foldCF" $ do
    foldCF t id (\a b acc -> a + b + acc) `shouldBe` (3 + 1 + 4 + 1 + 12 + 1 + 4)

-- Parte (e): foldEvalCF y foldDegree
testsEjecicio1ParteE :: Spec
testsEjecicio1ParteE = describe "EJERCICIO 1.e) foldEvalCF y foldDegree" $ do
  it "pruebas para foldEvalCF" $ do
    foldEvalCF t `shouldBe` evalCF t

  it "pruebas para foldDegree" $ do
    foldDegree t `shouldBe` degree t

-- Parte (f): evalCF
testsEjecicio1ParteF :: Spec
testsEjecicio1ParteF = describe "EJERCICIO 1.f) frac2ConFrac " $ do
  it "pruebas para frac2ConFrac" $ do
    frac2ConFrac (649, 200) `shouldBe` t

-- Parte (g): Propiedad
propiedadEjecicio1 :: Spec
propiedadEjecicio1 = describe "EJERCICIO 1.g) Propiedad" $ do
  it "propiedades de identidad" $ do
    quickCheck testFrac2ConFrac

-- Pruebas para EJERCICIO 2 =============================

-- Ejemplo de prueba para una función del ejercicio 2
testsEjecicio2 :: Spec
testsEjecicio2 = describe "EJERCICIO 2" $ do
  -- Añadir pruebas específicas para el ejercicio 2 aquí
  return ()

-- Pruebas para EJERCICIO 3 =============================

-- Ejemplo de prueba para una función del ejercicio 3
testsEjecicio3 :: Spec
testsEjecicio3 = describe "EJERCICIO 3" $ do
  -- Añadir pruebas específicas para el ejercicio 3 aquí
  return ()

-- Pruebas para EJERCICIO 4 =============================

-- Ejemplo de prueba para una función del ejercicio 4
testsEjecicio4 :: Spec
testsEjecicio4 = describe "EJERCICIO 4" $ do
  -- Añadir pruebas específicas para el ejercicio 4 aquí
  return ()

-- Pruebas para EJERCICIO 5 =============================

-- Ejemplo de prueba para una función del ejercicio 5
testsEjecicio5 :: Spec
testsEjecicio5 = describe "EJERCICIO 5" $ do
  -- Añadir pruebas específicas para el ejercicio 5 aquí
  return ()

{-------------------------------------------}
{------------------ MAIN -------------------}
{-------------------------------------------}

main :: IO ()
main = hspec $ do
  testsEjecicio1ParteB
  testsEjecicio1ParteC
  testsEjecicio1ParteD
  testsEjecicio1ParteE
  testsEjecicio1ParteF
  propiedadEjecicio1
  testsEjecicio2
  testsEjecicio3
  testsEjecicio4
  testsEjecicio5
