{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
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

-- Parte (a): foldF
testsEjecicio2ParteA :: Spec
testsEjecicio2ParteA = describe "EJERCICIO 2.a) foldF" $ do
  it "pruebas para foldF" $ do
    let formula = And (Const True) (Not (Var 'x'))
    let val1 =[('x',True)]
    let val2 =[('x',False)]
    let ffold = ( \valu x -> case find x valu of
          Right val -> val
          Left val -> error val )
    foldF id (ffold val1) not (&&) (<=) formula `shouldBe` False
    foldF id (ffold val2) not (&&) (<=) formula `shouldBe` True


-- Parte (b): foldEvalF
testsEjecicio2ParteB :: Spec
testsEjecicio2ParteB = describe "EJERCICIO 2.b) foldEvalF" $ do
  it "pruebas para foldEvalF" $ do
    let formula = And (Const True) (Not (Var 'x'))
    foldEvalF formula [('x', False)] `shouldBe` True
    foldEvalF formula [('x', True)] `shouldBe` False

-- Parte (c): isTaut
testsEjecicio2ParteC :: Spec
testsEjecicio2ParteC = describe "EJERCICIO 2.c) isTaut" $ do
  it "pruebas para isTaut" $ do
    let formula1 = And (Const True) (Var 'x')
    let formula2 = And (Var 'x') (Not (Var 'x'))
    let formula3 = Imply (And (Var 'a') (Imply (Var 'a') (Var 'b'))) (Var 'b')
    isTaut formula1 `shouldBe` Just [('x', False)]
    isTaut formula2 `shouldBe` Just [('x', False)]
    isTaut formula3 `shouldBe` Nothing


-- Pruebas para EJERCICIO 3 =============================

-- Parte (a): find
testsEjecicio3ParteA :: Spec
testsEjecicio3ParteA = describe "EJERCICIO 3.a) find" $ do
  it "pruebas para find" $ do
    find 'x' [('x', True)] `shouldBe` Right True
    find 'y' [('x', True)] `shouldBe` Left "Key 'y' not found"
    find 'x' [('x', True), ('x', False)] `shouldBe` Left "Multiple values for key 'x'"


-- Pruebas para EJERCICIO 4 =============================

-- Parte (a): nFirstEven
testsEjecicio4ParteA :: Spec
testsEjecicio4ParteA = describe "EJERCICIO 4.a) nFirstEven" $ do
  it "pruebas para nFirstEven" $ do
    let result = [Zero, Succ Zero, Succ (Succ Zero), Succ (Succ (Succ Zero))]
    nFirstEven (Succ (Succ (Succ Zero))) `shouldBe` result

-- Parte (b): fun
testsEjecicio4ParteB :: Spec
testsEjecicio4ParteB = describe "EJERCICIO 4.b) fun" $ do
  it "pruebas para fun" $ do
    let expected = add (mult dos dos) (add uno uno)
    fun (Succ (Succ Zero)) `shouldBe` expected


{-------------------------------------------}
{------------------ MAIN -------------------}
{-------------------------------------------}

main :: IO ()
main = hspec $ do
  -- Ejercicio 1
  testsEjecicio1ParteB
  testsEjecicio1ParteC
  testsEjecicio1ParteD
  testsEjecicio1ParteE
  testsEjecicio1ParteF
  propiedadEjecicio1
  -- Ejercicio 2
  testsEjecicio2ParteA
  testsEjecicio2ParteB
  testsEjecicio2ParteC
  -- Ejercicio 3
  testsEjecicio3ParteA
  -- Ejercicio 4
  testsEjecicio4ParteA
  testsEjecicio4ParteB