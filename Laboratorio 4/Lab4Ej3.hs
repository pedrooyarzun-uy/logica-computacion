
module Lab4Ej3 where
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

-- Clique de grafo
type KClique = (Graph, Nat)

----------------------------------------------------------------------------------
-- 3.1. k-clique como problema de búsqueda
----------------------------------------------------------------------------------

-- Pre: recibe un grafo no dirigido g y un tamaño de clique k
-- Pos: retorna una fórmula de LP formalizando el problema del k-clique 
kClique :: KClique -> [L]
kClique = undefined

--      Problema del k-clique como problema de búsqueda.
-- Pre: recibe una instancia de problema de k-clique.
-- Pos: en caso positivo retorna el modelo que representa la selección, 
--      de lo contrario retorna Nothing.
solvekClique :: KClique -> IO (Maybe Model)
solvekClique = undefined

----------------------------------------------------------------------------------
-- 3.2. Grafo de ejercicio
----------------------------------------------------------------------------------

g8 :: Graph 
g8 = undefined

-- a) 
-- ESCRIBIR RESPUESTA AQUÍ

-- b) 
-- ESCRIBIR RESPUESTA AQUÍ

-- c) 
-- ESCRIBIR RESPUESTA AQUÍ

----------------------------------------------------------------------------------
-- 3.3. k-clique como problema de optimización
----------------------------------------------------------------------------------

-- a)   Problema del k-clique como problema de optimización.
-- Pre: recibe una instancia de problema de k-clique.
-- Pos: retorna una pareja con el valor máximo posible y el modelo que representa la selección
maxClique :: KClique -> IO (Maybe (Int,Model))
maxClique = undefined

-- b) 
-- ESCRIBIR RESPUESTA AQUÍ

-- c) 
-- ESCRIBIR RESPUESTA AQUÍ

----------------------------------------------------------------------------------
-- 3.4. Trabajando con archivos
----------------------------------------------------------------------------------

-- a)   Problema del k-clique como problema de búsqueda, pero leyendo grafo desde archivo.
-- Pre: recibe la ubicacion de un archivo en formato TSV y un tamaño de clique k.
-- Pos: en caso positivo retorna el modelo que representa la selección, 
--      de lo contrario retorna Nothing.
solvekCliqueFrom :: String -> Nat -> IO (Maybe Model)
solvekCliqueFrom = undefined

-- a)   Problema del k-clique como problema de optimización, pero leyendo grafo desde archivo.
-- Pre: recibe la ubicacion de un archivo en formato TSV.
-- Pos: retorna una pareja con el valor máximo posible y el modelo que representa la selección.
maxCliqueFrom :: String -> IO (Maybe (Int,Model))
maxCliqueFrom = undefined

----------------------------------------------------------------------------------
-- 3.5. Grafos network_yeast.tsv y network_butterfly.tsv
----------------------------------------------------------------------------------

-- ESCRIBIR RESPUESTA AQUÍ



