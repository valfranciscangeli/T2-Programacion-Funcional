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
-- por las funciones desarrolladas previamente se puede notar un comportamiento
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
frac2ConFrac (0, _) = Simple 0
frac2ConFrac (_,0) = error "denominador  igual a 0"
frac2ConFrac (val, 1) = Simple val
frac2ConFrac (num, den) = Compound (num `div` den) 1 (frac2ConFrac (den, num `mod` den))

-- parte g)

testFrac2ConFrac :: Integer -> Integer -> Bool
testFrac2ConFrac m n
  | m < 0 || n <= 0 = True -- casos no definidos
  | otherwise = (num1 * den2) == (den1 * num2)
    where
      (num1, den1)= evalCF (frac2ConFrac (m, n))
      (num2, den2) = (m, n)

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
  (Bool -> a) ->            -- funcion para Const
  (Variable -> a) ->        -- funcion para Var
  (a -> a) ->               -- funcion para Not
  (a -> a -> a) ->          -- funcion para And
  (a -> a -> a) ->          -- funcion para Imply
  Formula -> a
foldF fConst fVar fNot fAnd fImply formula = case formula of
  Const p -> fConst p
  Var p -> fVar p
  Not p -> fNot (foldF fConst fVar fNot fAnd fImply p)
  And p q -> fAnd (foldF fConst fVar fNot fAnd fImply p) (foldF fConst fVar fNot fAnd fImply q)
  Imply p q -> fImply (foldF fConst fVar fNot fAnd fImply p) (foldF fConst fVar fNot fAnd fImply q)

-- Parte (b)
foldEvalF :: -- se cambio el nombre de fold a foldEvalF para evitar colision de nombre
  Formula -> Valuation -> Bool
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
allVals form = map (zip vars) vals
  where
    vars = rmdups (fvar form)
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
    eval1 :: Formula -> [Valuation] -> Maybe Valuation
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
  -- Eq v: Se necesita comparar igualdad tambien en v para revisar si existen valores duplicados (iguales) asociados a la misma clave k.

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

-- foldNat de las diapos de clases
foldNat :: (b -> b) -> b -> Nat -> b
foldNat _ v Zero= v
foldNat fn v (Succ n) = fn (foldNat fn v n)

-- numeros que se repite uso
uno :: Nat
uno = Succ Zero

dos :: Nat
dos = Succ uno

-- se agrega funcion para restar
subs :: Nat -> Nat -> Nat
subs Zero _ = Zero    -- restar cualquier cosa a 0 trunca en 0 por ser conjunto N
subs n Zero = n
subs (Succ n) (Succ m) = subs n m

-- ==================

-- ver si un Nat es par
esPar :: Nat -> Bool
esPar Zero = True
esPar (Succ Zero) = False
esPar (Succ (Succ n)) = esPar n

-- funcion f usando foldNat (no resulta)
{- f :: Nat -> Nat
f Zero = dos -- f(0) = 2
f n = foldNat aux dos n 
  where
    aux:: Nat-> Nat 
    aux v 
      | esPar v = add v aux(subs v dos)  -- n + f(n - 2)
      | otherwise  = add (mult v v) aux(subs v uno)  -- n^2 + f(n - 1) -}

-- f sin foldNat
-- finalmente no pude utilizar fold para resolver la funcion
-- esto porque necesitaba aplicar la funcion aux a dos valores, pero foldNat
-- requiere una funcion unaria (es la definicion que se vio en clases)
f :: Nat -> Nat
f Zero = dos -- f(0) = 2
f n
  | esPar n   = add n (f (subs n dos))  -- n + f(n - 2) si n es par
  | otherwise = add (mult n n) (f (subs n uno))  -- n^2 + f(n - 1) si n es impar


{-------------------------------------------}
{--------------  EJERCICIO 5  --------------}
{-------------------------------------------}
data BinTree a = Leaf a | InNode (BinTree a) a (BinTree a)

foldBT :: (b -> a -> b -> b) -> (a -> b) -> (BinTree a -> b)
foldBT _ g (Leaf v) = g v
foldBT fn g (InNode t1 v t2) = fn (foldBT fn g t1) v (foldBT fn g t2)

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

  -> Se tiene un arbol binario t

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

--mirrorBT :: BinTree a -> BinTree a
--mirrorBT = foldBT (\ r1 v r2 -> InNode r2 v r1 ) Leaf

flattenBT :: BinTree a -> [ a ]
flattenBT = foldBT (\ r1 v r2 -> r1 ++ [ v ] ++ r2 ) ( : [ ] )

sizeBT :: BinTree a -> Int
sizeBT = foldBT (\ r1 _ r2 -> r1 + 1 + r2 ) ( const 1)

{- 
Por parte a) se tiene que:
  h(foldBT f g t) = foldBT f' g' t
  para un BT t.

Probar que
  length.flattenBT = sizeBT

HINT: Asumir que 
  length (xs ++ ys) = length xs + lenght ys


CASO BASE:
  -> t = Leaf v

  probar: (sin la notacion .)
  length (flattenBT (Leaf v)) = sizeBT (Leaf v)

  entonces, izquierda:
    length (flattenBT (Leaf v))
    =length ([v])                               / -> flattenBT (foldBT.1)
    = 1                                         / -> length.2

  luego, derecha:
    sizeBT (Leaf v)
    = const 1                                   / -> sizeBT (foldBT.1)
    = 1

  ambos lados son iguales, por lo que se demuestra para caso base.

CASO INDUCTIVO:

  HI: la propiedad se cumple para los subarboles t1 y t2
    -> t = InNode t1 v t2

  probar: (sin la notacion .)
  length (flattenBT (InNode t1 v t2)) = sizeBT (InNode t1 v t2)

  entonces, izquierda:
    length (flattenBT (InNode t1 v t2))
    = length ((flattenBT t1) ++ [v] ++ (flattenBT t2))              / -> flattenBT (foldBT.2)
    = length(flattenBT t1) + length [v] + length(flattenBT t2)      /-> Hint
    = sizeBT t1 + length [v] + sizeBT t2                            /-> (a) y HI para cada subarbol
    = sizeBT t1 + 1 + sizeBT t2                                     / -> length.2
  
  luego, derecha:
    sizeBT (InNode t1 v t2)
    = sizeBT t1 + 1 + sizeBT t2                                     / -> sizeBT (fold.2)

  ambos lados son iguales, por lo que se demuestra para caso inductivo.


Finalmente, se demuestra para caso base y caso inductivo. Por lo tanto, la propiedad se cumple para todo BT.

-}

-- Parte (c)

mirrorBT :: BinTree a -> BinTree a
mirrorBT = foldBT (\ r1 v r2 -> InNode r2 v r1 ) Leaf

idBT :: BinTree a -> BinTree a
idBT = foldBT InNode Leaf

{-

Por parte a) se tiene que:
  h(foldBT f g t) = foldBT f' g' t
  para un BT t.

Probar que
  mirrorBt.mirrorBT = idBT

CASO BASE:
  -> t = Leaf v

  probar: (sin la notacion .)
  mirrorBT (mirrorBt (Leaf v)) = idBT (Leaf v)

  entonces, izquierda:
    mirrorBT (mirrorBt (Leaf v))
    = mirrorBt (Leaf v)                   / -> mirrorBT (fold.1)
    = Leaf v                              / -> mirrorBT (fold.1)


  luego, derecha:
    idBT (Leaf v)
    = Leaf v                              / -> idBT (fold.1)

  ambos lados son iguales, por lo que se demuestra para caso base.


CASO INDUCTIVO:
  HI: la propiedad se cumple para los subarboles t1 y t2
    -> t = InNode t1 v t2

  probar: (sin la notacion .)
  mirrorBT (mirrorBt (InNode t1 v t2)) = idBT (InNode t1 v t2)

  entonces, izquierda:
    mirrorBT (mirrorBt (InNode t1 v t2))
    = mirrorBT (InNode (mirrorBT t2) v (mirrorBT t1))   !!        / -> mirrorBt (fold.2)
    = InNode (mirrorBt(mirrorBT t1) v mirrorBt(mirrorBT t2) )     / -> mirrorBt (fold.2)
    = InNode idBT(t1) v idBT(t2)                                  / (a) y HI para cada sub arbol
    = InNode t1 v t2                                              / -> idBT (foldBT.2)

  luego, derecha:
    idBT (InNode t1 v t2)
    = InNode t1 v t2                                              / -> idBT (fold.2)

  ambos lados son iguales, por lo que se demuestra para caso inductivo.
  
Finalmente, se demuestra para caso base y caso inductivo. Por lo tanto, la propiedad se cumple para todo BT.

-}



