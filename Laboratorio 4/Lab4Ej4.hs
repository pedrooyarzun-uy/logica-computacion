
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
hamilPath = undefined

-- Pre: recibe un grafo no dirigido.
-- Pos: en caso positivo retorna el modelo que representa el camino, 
--      de lo contrario retorna Nothing.
solveHamilPath :: Graph -> IO (Maybe Model)
solveHamilPath = undefined

----------------------------------------------------------------------------------
-- 4.2. Primer grafo de ejercicio
----------------------------------------------------------------------------------

g8 :: Graph  
g8 = undefined

----------------------------------------------------------------------------------
-- 4.3 Ciclo Hamiltoniano
----------------------------------------------------------------------------------

-- Pre: recibe un grafo no dirigido.
-- Pos: retorna una fórmula de LP formalizando el problema del Ciclo Hamiltoniano.
hamilCycle :: Graph -> [L]
hamilCycle = undefined

-- Pre: recibe un grafo no dirigido.
-- Pos: en caso positivo retorna el modelo que representa el ciclo, 
--      de lo contrario retorna Nothing.
solveHamilCycle :: Graph -> IO (Maybe Model)
solveHamilCycle g = undefined

----------------------------------------------------------------------------------
-- 4.4. Segundo grafo de ejercicio
----------------------------------------------------------------------------------

g8' :: Graph  
g8' = undefined

-- a)
-- ESCRIBIR RESPUESTA AQUÍ

-- b)
-- ESCRIBIR RESPUESTA AQUÍ

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