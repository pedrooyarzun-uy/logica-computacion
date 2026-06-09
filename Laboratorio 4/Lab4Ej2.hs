
module Lab4Ej2 where
------------------- Estudiante/s -------------------
-- Nombres y apellidos: 
-- Números: 
----------------------------------------------------

import Prelude
import Data.List
import Data.Maybe

import SMTlib
import GraphUtils
import PropLogic

----------------------------------------------------------------------------------
-- 2.1. Cardinalidad naive
----------------------------------------------------------------------------------

-- a) "Existen al menos k índices que cumplen la fórmula f"
existAtLeast :: Nat -> [Nat] -> (Nat -> L) -> L
existAtLeast 0 is f = top
existAtLeast 1 is f = bigOr is f 
existAtLeast k is f = bigOr is (\i -> f i /\ existAtLeast (k-1) (is\\[i]) f) 

-- b) "Existen a lo sumo k índices que cumplen la fórmula f"
existAtMost :: Nat -> [Nat] -> (Nat -> L) -> L
existAtMost k is f = neg (existAtLeast (k+1) is f)

-- c) "Existen exactamente k índices que cumplen la fórmula f"
existExact :: Nat -> [Nat] -> (Nat -> L) -> L
existExact k is f = existAtLeast k is f /\ existAtMost k is f

----------------------------------------------------------------------------------
-- 2.2. Cardinalidad a la Sinz
----------------------------------------------------------------------------------

-- a) "Existen a lo sumo k índices que cumplen la fórmula f"
existAtMost' :: String -> Nat -> [Nat] -> (Nat -> L) -> L -- O(n^2)
existAtMost' name k is p 
  | k >= (length is) = top
  | k == 0 = bigAnd is (\i -> neg (p i)) -- O(n)
  | n == 0 = top
  | otherwise = foldr (/\) top rules 
  where 
    n = length is
    s i j  = v2 name i j
    rules =  r2 ++ r3 ++ r4 ++ r5

    r2 = [(p i) ==> (s i 1) | i <- [1..n]] -- O(n)
    r3 = [(s (i-1) j ==> s i j) | i <- [2..n], j <- [1..k]] --O(n*k)
    r4 = [(p i) /\ s (i-1) (j-1) ==> s i j | i <- [2..n], j <- [2..k]] -- O(n*k)
    r5 = [s (i-1) k ==> neg (p i) | i <- [2..n]] -- O(n)


-- b) "Existen a al menos k índices que cumplen la fórmula f"
existAtLeast' :: String -> Nat -> [Nat] -> (Nat -> L) -> L
existAtLeast' name k is p
  | k <= 0 = top
  | k > n  = bot
  | otherwise = existAtMost' name (n - k) is (\i -> neg (p i))
  where
    n = length is

-- c) "Existen exactamente k índices que cumplen la fórmula f"
existExact' :: String -> String -> Nat -> [Nat] -> (Nat -> L) -> L
existExact' name1 name2 k is f =
  existAtLeast' name1 k is f /\ existAtMost' name2 k is f

----------------------------------------------------------------------------------
-- 2.3. Estimación de tamaño y eficiencia
----------------------------------------------------------------------------------

--a) RESPUESTA AQUÍ

--b) RESPUESTA AQUÍ

--c) RESPUESTA AQUÍ

--d) RESPUESTA AQUÍ

----------------------------------------------------------------------------------
-- Testing de equivalencia de la codificación
----------------------------------------------------------------------------------

-- Ejecuta una batería de tests de equivalencia para laS codificación Naive y Sinz.
-- Recibe un n máximo y prueba todos los tamaños de dominio 1..n.
-- Para cada n, verifica todos los k en el rango 0..n+1.
-- Si todos los tests pasan, imprime un mensaje de éxito global.
testEquiv :: Nat -> IO ()
testEquiv maxN  = do
  putStrLn "Ejecutando test de equivalencia Naive <=> Sinz"
  results <- mapM testEquivN [1..maxN]
  if and results
    then putStrLn "Todos los test pasan :)"
    else putStrLn "Algunos test no pasan :("

-- Test de corrección para un tamaño de dominio fijo n.
-- Para ese n, prueba todos los valores de k entre 0 y n+1.
-- Devuelve True si todos los tests para ese n pasan.
testEquivN :: Nat -> IO Bool
testEquivN n = do
  putStrLn $ "\nProbando con n = " ++ show n ++ ":"
  ks <- mapM (testEquivK n) [0..n+1]
  return (and ks)

-- Test de un caso puntual (n,k).
-- Devuelve True si el solver NO encuentra contraejemplo (es decir, insat), 
-- y False si encuentra uno (sat).
testEquivK :: Nat -> Nat -> IO Bool
testEquivK n k = do
  res <- decidirEquiv k n (\i -> v1 "p" i)
  putStrLn $ "k = " ++ show k ++ " -> " ++ show res
  return res

-- Construye el problema de cardinalidad sobre las variables p1,...,pn,
-- y verifica que la codificación naive y la de Sinz coinciden.
-- Para esto lo que hacemos es construir una fórmula de LPO que expresa lo siguiente:
-- 
--  "Para toda asignación de las variables originales p1,...,pn, la fórmula naive es verdadera 
--   si y solo si existe alguna asignación de las variables auxiliares de Sinz s_ij que 
--   hace verdadera la fórmula de Sinz."
--
-- Formalmente en LPO:  
--   (∀ p₁,...,pₙ)(existAtMost(k,[1..n],f) ⟷ (∃ s₁₁,...,sₙₖ)existAtMost'("s",k,[1..n],f)))
--
-- Luego, esta fórmula se niega y se envía al SMT solver.  
-- Resultado esperado:
--  Si el solver devuelve UNSAT, no existe contraejemplo y la función retorna True.
--  Si el solver devuelve SAT, existe una valuación de las p_i donde ambas
--  codificaciones difieren, y la función retorna False.
decidirEquiv :: Nat -> Nat -> (Nat -> L) -> IO Bool
decidirEquiv k n f = do
  satCounterExample <- decide problem
  return (not satCounterExample)
  where
    naive = existAtMost k [1..n] f
    sinz  = existAtMost' "s" k [1..n] f
    equivalence =  "(forall (" ++ unwords pDecls ++ ") " ++
                      "(= " ++ lp2SMT naive ++ " " ++ existsSMT sDecls (lp2SMT sinz) ++ "))"
    negEquivalence = "(not " ++ equivalence ++ ")"
    problem = ("ALL", [pVarDec], [negEquivalence])
    pDecls = [ "(" ++ show (f i) ++ " Bool)" | i <- [1..n] ]
    sDecls = [ "(" ++ show (v2 "s" i j) ++ " Bool)" | i <- [1 ..n] , j <- [1..k] ]

existsSMT :: [String] -> String -> String
existsSMT [] body = body
existsSMT vars body = "(exists (" ++ unwords vars ++ ") " ++ body ++ ")"  