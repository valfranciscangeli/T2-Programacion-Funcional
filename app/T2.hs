module Main where

  import Test.Hspec
  import Test.QuickCheck


  {-------------------------------------------}
  {--------------  EJERCICIO 1  --------------}
  {-------------------------------------------}

  -- Parte a)


  -- Parte b)
  -- evalCF :: ContFraction -> (Integer,Integer)


  -- Parte c)
  -- degree ::  ContFraction -> Integer


  -- Parte d)


  -- Parte e)


  -- Parte f)
  -- frac2ConFrac :: (Integer, Integer) -> ContFraction


  -- parte g)
  -- testFrac2ConFrac :: Spec




  {-------------------------------------------}
  {--------------  EJERCICIO 2  --------------}
  {-------------------------------------------}

  type Variable  = Char
  data Formula   = Const Bool
                 | Var Variable
                 | Not Formula
                 | And Formula Formula
                 | Imply Formula Formula



  -- Parte (a)


  -- Parte (b)


  -- Parte (c)
  rmdups :: Eq a => [a] -> [a]
  rmdups [] = []
  rmdups (x:xs) = x : rmdups (filter (/= x) xs)

  bools :: Int -> [[Bool]]
  bools 0 = [[]]
  bools n = map (False :) r ++ map (True :) r where
    r = bools (n-1)

  allVals :: Formula -> [Valuation]
  allVals f = map (zip vars) vals where
    vars = rmdups (fvar f)
    vals = bools (length vars)

  -- isTaut :: Formula -> Maybe Valuation




  {-------------------------------------------}
  {--------------  EJERCICIO 3  --------------}
  {-------------------------------------------}

  type Assoc k v = [(k,v)]
  type Error     = String

  -- Parte (a)


  -- Parte (b)



  {-------------------------------------------}
  {--------------  EJERCICIO 4  --------------}
  {-------------------------------------------}

  data Nat = Zero | Succ Nat deriving Show

  add :: Nat -> Nat -> Nat
  add Zero     m = m
  add (Succ n) m = Succ (add n m)

  mult :: Nat -> Nat -> Nat
  mult Zero     m = Zero
  mult (Succ n) m = add m (mult n m)

  foldNat :: (b -> b) -> b -> Nat -> b
  foldNat f v Zero     = v
  foldNat f v (Succ n) = f (foldNat f v n)





  {-------------------------------------------}
  {--------------  EJERCICIO 5  --------------}
  {-------------------------------------------}

