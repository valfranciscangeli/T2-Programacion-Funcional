{-# LANGUAGE TupleSections #-}

module T2 where

{-------------------------------------------}
{--------------  EJERCICIO 1  --------------}
{-------------------------------------------}

-- Parte a)

data ContFraction
  = Simple Integer
  | Compound Integer Integer ContFraction
  deriving (Eq, Show, Ord)

t :: ContFraction
t = Compound 3 1 (Compound 4 1 (Compound 12 1 (Simple 4)))

-- Parte b)
evalCF :: ContFraction -> (Integer, Integer)
evalCF (Simple a) = (a, 1)
evalCF (Compound a b frac) = (a * num + b * den, num) -- desarrollo de la formula para que haya una unica fraccion
  where
    (num, den) = evalCF frac

-- Parte c)
degree :: ContFraction -> Integer
degree (Simple _) = 0
degree (Compound _ _ frac) = 1 + degree frac

-- Parte d)
-- por las funciones desarrolladar previamente se puede notar un comportamiendo
-- diferente cuando se encuentra con un ContFraction Simple o Compound
-- ( se aplican diferentes funciones)
foldCF :: ContFraction -> (Integer -> a) -> (Integer -> Integer -> a -> a) -> a
foldCF (Simple val) fSim _ = fSim val
foldCF (Compound aval bval frac) fSim fCom =
  fCom
    aval
    bval
    (foldCF frac fSim fCom)

-- Parte e)
foldEvalCF :: ContFraction -> (Integer, Integer)
foldEvalCF frac =
  foldCF
    frac
    (,1)
    (\a b (num, den) -> (a * num + b * den, num))

foldDegree :: ContFraction -> Integer
foldDegree frac =
  foldCF
    frac
    (const 0)
    (\_ _ gradInt -> 1 + gradInt)

-- Parte f)
frac2ConFrac :: (Integer, Integer) -> ContFraction
frac2ConFrac (val, 1) = Simple val
frac2ConFrac (num, den) = Compound (num `div` den) 1 (frac2ConFrac (den, num `mod` den))

-- parte g)
testFrac2ConFrac :: Integer -> Integer -> Bool
-- se modifico el tipo para correr el test en el archivo main
testFrac2ConFrac m n
  | m < 0 = True -- caso no contemplado en la propiedad
  | n <= 0 = True -- caso no contemplado en la propiedad
  | otherwise = evalCF (frac2ConFrac (m, n)) == (m, n)

{-------------------------------------------}
{--------------  EJERCICIO 2  --------------}
{-------------------------------------------}

type Variable = Char

data Formula
  = Const Bool
  | Var Variable
  | Not Formula
  | And Formula Formula
  | Imply Formula Formula

-- Parte (a)
foldF ::
  (Bool -> a) -> -- funcion para Const
  (Variable -> a) -> -- Funcion para Var
  (a -> a) -> -- Funcion para Not
  (a -> a -> a) -> -- Funcion para And
  (a -> a -> a) -> -- Funcion para Imply
  Formula ->
  a -- Finalmente la formula y retorna algo tipo a
foldF fConst fVar fNot fAnd fImply formula = case formula of
  Const p -> fConst p
  Var p -> fVar p
  Not p -> fNot (foldF fConst fVar fNot fAnd fImply p)
  And p q -> fAnd (foldF fConst fVar fNot fAnd fImply p) (foldF fConst fVar fNot fAnd fImply q)
  Imply p q -> fImply (foldF fConst fVar fNot fAnd fImply p) (foldF fConst fVar fNot fAnd fImply q)

-- Parte (b)
foldEvalF :: -- se cambio el nombre de fold a foldEvalF para evitar colision de nombre
  Formula ->
  Valuation ->
  Bool -- retorna Bool
foldEvalF formula s =
  foldF
    id
    ( \x -> case find x s of
        Right val -> val
        Left val -> error val
    )
    not
    (&&)
    (<=)
    formula

fvar ::
  Formula -> [Char]
fvar =
  foldF
    (const [])
    (: [])
    id
    (++)
    (++)

-- Parte (c)
rmdups :: (Eq a) => [a] -> [a]
rmdups [] = []
rmdups (x : xs) = x : rmdups (filter (/= x) xs)

bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False :) r ++ map (True :) r
  where
    r = bools (n - 1)

allVals :: Formula -> [Valuation]
allVals f = map (zip vars) vals
  where
    vars = rmdups (fvar f)
    vals = bools (length vars)

isTaut :: Formula -> Maybe Valuation
isTaut formula =
  if and trueValues
    then Nothing
    else eval1 formula valuations
  where
    valuations = allVals formula
    trueValues = map (foldEvalF formula) valuations

    -- eval1: entrega la primera valuacion que haga la formula False
    eval1 _ [] = Nothing
    eval1 form (val : vals)
      | not (foldEvalF form val) = Just val
      | otherwise = eval1 form vals

{-------------------------------------------}
{--------------  EJERCICIO 3  --------------}
{-------------------------------------------}

type Assoc k v = [(k, v)]

type Error = String

type Valuation = Assoc Char Bool

-- Parte (a)
find ::
  (Eq k, Show k, Eq v) =>
  k ->
  Assoc k v ->
  Either Error v
find key assoc = case [v | (k, v) <- assoc, k == key] of
  [] -> Left ("Key " ++ show key ++ " not found")
  [v] -> Right v
  _ -> Left ("Multiple values for key " ++ show key)

-- Parte (b)
-- Eq k: Se debe declarar que k es de tipo Eq para comparar igualdad con la llave buscada.
-- Show k: Se necesita que k cumpla este tipo para poder ser mostrado en pantalla con el mensaje de error.
-- Eq v: Se necesita comparar igualdad también en v para revisar si existen valores duplicados (iguales) asociados a la misma clave k.

{-------------------------------------------}
{--------------  EJERCICIO 4  --------------}
{-------------------------------------------}

data Nat = Zero | Succ Nat deriving (Show, Eq)

add :: Nat -> Nat -> Nat
add Zero m = m
add (Succ n) m = Succ (add n m)

mult :: Nat -> Nat -> Nat
mult Zero _ = Zero
mult (Succ n) m = add m (mult n m)

foldNat :: (b -> b) -> b -> Nat -> b
foldNat _ v Zero = v
foldNat f v (Succ n) = f (foldNat f v n)

-- se agrega funcion para restar
subs :: Nat -> Nat -> Nat
subs Zero _ = Zero
subs n Zero = n
subs (Succ n) (Succ m) = subs n m

-- numeros que se repite uso
uno :: Nat
uno = Succ Zero

dos :: Nat
dos = Succ uno

-- funcion aux para obtener una lista de los primeros n numeros pares
nFirstEven :: Nat -> [Nat]
nFirstEven = genList Zero
  where
    genList :: Nat -> Nat -> [Nat]
    genList _ Zero = []
    genList count (Succ k) = mult dos count : genList (Succ count) k

fun :: Nat -> Nat
fun n = foldNat funN dos n
  where
    funN :: Nat -> Nat
    funN k
      | k `elem` nFirstEven n = add k (fun (subs n dos))
      | otherwise = add (mult k k) (fun (subs n uno))

{-------------------------------------------}
{--------------  EJERCICIO 5  --------------}
{-------------------------------------------}
data BinTree a = Leaf a | InNode (BinTree a) a (BinTree a)

foldBT :: (b -> a -> b -> b) -> (a -> b) -> (BinTree a -> b)
foldBT _ g (Leaf v) = g v
foldBT f g (InNode t1 v t2) = f (foldBT f g t1) v (foldBT f g t2)

-- Parte (a)
{- 
  f:: b -> a -> b -> b
  g:: a -> b
  f’:: b’ -> a -> b’ -> b’
  g':: a -> b’
  h:: b -> b’ 

  composicion de funciones -> (f . g) x = f (g x)

  las funciones cumplen que:
  1. h (g v) = g’ v
  2. h (f x1 v x2) = f’ (h x1) v (h x2)

  PROBAR QUE: 
  h . foldBT f g = foldBT f’ g’

  -> Se tiene unarbol binario t

  CASO BASE:
    -> t = Leaf v

    probar: (sin la notacion .)
    h(foldBT f g (Leaf v)) = foldBT f' g' (Leaf v)

    entonces, izquierda:
      h(foldBT f g (Leaf v)) 
      = h(g(v))               / -> foldBT.1
      = g' v                  / condicion 1) h (g v) = g’ v


    luego, derecha:
      foldBT f' g' (Leaf v)
      = g' v !!     / -> foldBT.1

    por lo que se demuestra para caso base.

  CASO INDUCTIVO:
    HI: la propiedad se cumple para los subarboles t1 y t2
    -> t = InNode t1 v t2

    probar: (sin la notacion .)
    h(foldBT f g (InNode t1 v t2)) = foldBT f' g' (InNode t1 v t2)

    entonces, izquierda:
      h(foldBT f g (InNode t1 v t2))
      = h(f (foldBT f g t1) v (foldBT f g t2))        / -> foldBT.2
      = f' (h(foldBT f g t1) v h(foldBT f g t2))      / condicion 2) h (f x1 v x2) = f’ (h x1) v (h x2)
      = f'((foldBT f' g' t1) v (foldBT f' g' t2))     / Por HI, h(foldBT f g t1)= foldBT f' g' t1, lo mismo para t2
      = foldBT f' g' (InNode t1 v t2)                 / <- foldBT.2

      por lo que queda igual que la equivalencia derecha.

  Finalmente, se demuestra para caso base y caso inductivo. Por lo tanto, la propiedad se cumple para todo BT.
  -}

-- Parte (b)

-- Parte (c)
