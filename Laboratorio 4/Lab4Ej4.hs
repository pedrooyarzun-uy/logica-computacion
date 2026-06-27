
module Lab4Ej4 where
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
import Lab4Ej2

----------------------------------------------------------------------------------
-- 4.1 Camino Hamiltoniano
----------------------------------------------------------------------------------

-- Pre: recibe un grafo no dirigido.
-- Pos: retorna una fórmula de LP formalizando el problema del Camino Hamiltoniano.
hamilPath :: Graph -> [L]
hamilPath (n, es) =
  [ bigAnd [1..n] (\i -> existExact 1 [1..n] (\j -> h i j))
  , bigAnd [1..n] (\j -> existExact 1 [1..n] (\i -> h i j))
  , bigAnd [1..n-1] (\i ->
      bigAnd [1..n] (\a ->
        bigAnd [1..n] (\b ->
          if adjacent a b then top else neg (h i a) \/ neg (h (i+1) b))))
  ]
  where
    h i j = v2 "h" i j
    adjacent a b = (a,b) `elem` es || (b,a) `elem` es

-- Pre: recibe un grafo no dirigido.
-- Pos: en caso positivo retorna el modelo que representa el camino, 
--      de lo contrario retorna Nothing.
solveHamilPath :: Graph -> IO (Maybe Model)
solveHamilPath g@(n, _) =
  solve ("QF_UF",
         varDecl "Bool" "p" : genVars2 "Bool" "h" [1..n] [1..n],
         map lp2SMT (hamilPath g))

----------------------------------------------------------------------------------
-- 4.2. Primer grafo de ejercicio
----------------------------------------------------------------------------------

g8 :: Graph  
g8 = (8, [(1,2),(2,3),(2,5),(3,4),(4,5),(3,6),(5,6),(5,7),(6,7),(7,8)])

-- Camino 1: 8 => 7 => 6 => 3 => 4 => 5 => 2 => 1
-- Camino 2: 8 => 7 => 6 => 5 => 4 => 3 => 2 => 1
----------------------------------------------------------------------------------
-- 4.3 Ciclo Hamiltoniano
----------------------------------------------------------------------------------

-- Pre: recibe un grafo no dirigido.
-- Pos: retorna una fórmula de LP formalizando el problema del Ciclo Hamiltoniano.
hamilCycle :: Graph -> [L]
hamilCycle g@(n, es) = hamilPath g ++ [closed]
  where
    h i j = v2 "h" i j
    adjacent a b = (a,b) `elem` es || (b,a) `elem` es
    closed =
      bigAnd [1..n] (\a ->
        bigAnd [1..n] (\b ->
          if adjacent a b then top else neg (h n a) \/ neg (h 1 b)))

-- Pre: recibe un grafo no dirigido.
-- Pos: en caso positivo retorna el modelo que representa el ciclo, 
--      de lo contrario retorna Nothing.
solveHamilCycle :: Graph -> IO (Maybe Model)
solveHamilCycle g@(n, _) =
  solve ("QF_UF",
         varDecl "Bool" "p" : genVars2 "Bool" "h" [1..n] [1..n],
         map lp2SMT (hamilCycle g))

----------------------------------------------------------------------------------
-- 4.4. Segundo grafo de ejercicio
----------------------------------------------------------------------------------

g8' :: Graph  
g8' = (8, [(3,2),(3,6),(2,1),(2,5),(1,4),(4,5),(4,7),(5,7),(5,8),(8,7),(8,6),(7,6),(5,6)])

-- a)
-- 6 => 3 => 2 => 1 => 4 => 5 => 7 => 8 => 6

-- b)
-- No presenta ciclo, si hacemos solveHamilCycle g8, nos va a devolver Nothing, que es correcto, porque no hay una implementacion posible que sea ciclo hamiltoniano.

----------------------------------------------------------------------------------
-- 4.5 El recorrido del caballero
----------------------------------------------------------------------------------

-- a)
solveKnightTour :: Nat -> IO (Maybe Model)
solveKnightTour = undefined

-- b)
solveClosedKnightTour :: Nat -> IO (Maybe Model)
solveClosedKnightTour = undefined

-- c) COMPLETAR CON RESULTADOS
-- | (n) | Recorrido abierto | Recorrido cerrado |
-- | --: | :---------------: | :---------------: |
-- |   1 |       
-- |   2 |       
-- |   3 |       
-- |   4 |      
-- |   5 |      
-- |   6 |      
-- |   7 |       
-- |   8 |       

-- d) 
solveClosedKnightTourFrom :: (Nat,Nat) -> Nat -> IO (Maybe Model)
solveClosedKnightTourFrom = undefined
