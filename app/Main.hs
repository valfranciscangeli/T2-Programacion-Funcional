import Control.Exception (SomeException, evaluate, try)
import T2
    ( Nat(Succ, Zero),
      Formula(Var, Const, Not, And, Imply),
      ContFraction(Simple, Compound),
      t,
      evalCF,
      degree,
      foldCF,
      foldEvalCF,
      foldDegree,
      frac2ConFrac,
      testFrac2ConFrac,
      foldF,
      foldEvalF,
      isTaut,
      find,
      uno,
      dos,
      f,
      esPar )
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck (Arbitrary (arbitrary), Gen, forAll, ioProperty, quickCheck, suchThat)

{-------------------------------------------}
{-----------------  TEST  ------------------}
{-------------------------------------------}

-- Pruebas para EJERCICIO 1 =============================

-- Parte (b): evalCF
testsEjecicio1ParteB :: Spec
testsEjecicio1ParteB = describe "EJERCICIO 1.b) evalCF" $ do
  it "pruebas para evalCF" $ do
    evalCF (Simple 5) `shouldBe` (5, 1)
    evalCF (Compound 1 2 (Simple 3)) `shouldBe` (5, 3)
    evalCF t `shouldBe` (649, 200)

-- Parte (c): degree
testsEjecicio1ParteC :: Spec
testsEjecicio1ParteC = describe "EJERCICIO 1.c) degree" $ do
  it "pruebas para degree" $ do
    degree t `shouldBe` 3 

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

-- se agrega esta funcion para no interrumpir el test en el caso de division por 0
testFrac2ConFracSafe :: Integer -> Integer -> IO Bool
testFrac2ConFracSafe m n = do
  result <- try (evaluate (testFrac2ConFrac m n)) :: IO (Either SomeException Bool)
  case result of
    Left _ -> return False
    Right res -> return res

-- generar n solo positivos y agregue que n!=m
genPositiveFrac :: Gen (Integer, Integer)
genPositiveFrac = do
  m <- arbitrary `suchThat` (> 0)
  n <- arbitrary `suchThat` (\x -> x > 0 && x /= m)
  return (m, n)

propiedadEjecicio1 :: Spec
propiedadEjecicio1 = describe "EJERCICIO 1.g) Propiedad" $ do
  it "propiedades de identidad con fracciones positivas" $ do
    quickCheck $ forAll genPositiveFrac (\(m, n) -> ioProperty $ testFrac2ConFracSafe m n)

-- Pruebas para EJERCICIO 2 =============================

-- Parte (a): foldF
testsEjecicio2ParteA :: Spec
testsEjecicio2ParteA = describe "EJERCICIO 2.a) foldF" $ do
  it "pruebas para foldF" $ do
    let formula = And (Const True) (Not (Var 'x'))
    let val1 = [('x', True)]
    let val2 = [('x', False)]
    let ffold =( \valu x -> case find x valu of
                          Right val -> val
                          Left val -> error val)
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
testsEjecicio4 :: Spec
testsEjecicio4 = describe "EJERCICIO 4)" $ do
  let cero = Zero
  let tres = Succ dos
  let cuatro = Succ tres
  let cinco = Succ cuatro
  let seis = Succ cinco
  let siete = Succ seis
  let ocho = Succ siete
  let nueve = Succ ocho
  let diez = Succ nueve
  let trece = Succ (Succ (Succ diez))
  let catorce = Succ trece
  it "pruebas para esPar" $ do
    esPar cero `shouldBe` True
    esPar uno `shouldBe` False
    esPar dos `shouldBe` True
    esPar tres `shouldBe` False
    esPar cuatro `shouldBe` True
    esPar cinco `shouldBe` False
    esPar seis `shouldBe` True
    esPar siete `shouldBe` False
    esPar ocho `shouldBe` True
    esPar nueve `shouldBe` False
    esPar diez `shouldBe` True
  it "pruebas para f" $ do
    f cero `shouldBe` dos
    f uno `shouldBe` tres
    f dos `shouldBe` cuatro
    f tres `shouldBe` trece
    f cuatro `shouldBe`ocho
    f seis `shouldBe` catorce

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
  testsEjecicio4