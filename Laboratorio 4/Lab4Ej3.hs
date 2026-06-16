
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
kClique ((n, es), k) =
  [ bigAnd [1..length paresNoAdyacentes]
      (\idx -> let (i,j) = paresNoAdyacentes !! (idx - 1)
               in neg (v1 "v" i) \/ neg (v1 "v" j))
  , existExact k [1..n] (\i -> v1 "v" i)
  ]
  where
    paresNoAdyacentes =
      [ (i,j)
      | i <- [1..n]
      , j <- [1..n]
      , i < j
      , (i,j) `notElem` es
      , (j,i) `notElem` es
      ]

--      Problema del k-clique como problema de búsqueda.
-- Pre: recibe una instancia de problema de k-clique.
-- Pos: en caso positivo retorna el modelo que representa la selección, 
--      de lo contrario retorna Nothing.
solveKClique :: KClique -> IO (Maybe Model)
solveKClique ((n,e),k) =
  solve ("QF_UF",
         varDecl "Bool" "p" : genVars1 "Bool" "v" [1..n],
         map lp2SMT (kClique ((n,e),k)))

----------------------------------------------------------------------------------
-- 3.2. Grafo de ejercicio
----------------------------------------------------------------------------------

g8 :: Graph
g8 =
  (8,
   [ (1,2),(2,1)
   , (1,3),(3,1)
   , (1,4),(4,1)
   , (1,5),(5,1)
   , (1,6),(6,1)
   , (2,3),(3,2)
   , (2,4),(4,2)
   , (2,5),(5,2)
   , (2,7),(7,2)
   , (3,4),(4,3)
   , (3,6),(6,3)
   , (3,7),(7,3)
   , (3,8),(8,3)
   , (4,5),(5,4)
   , (4,6),(6,4)
   , (4,7),(7,4)
   , (5,6),(6,5)
   , (5,7),(7,5)
   , (6,7),(7,6)
   , (7,8),(8,7)
   ])

-- a) 
-- Si, el kclique es: [2,4,5]. [3(4,5),(4,2),(2,5)]

-- b) No, no existe.
-- 

-- c) 
-- El clique mas grande es [4,5,6,7]

----------------------------------------------------------------------------------
-- 3.3. k-clique como problema de optimización
----------------------------------------------------------------------------------

-- a)   Problema del k-clique como problema de optimización.
-- Pre: recibe una instancia de problema de k-clique.
-- Pos: retorna una pareja con el valor máximo posible y el modelo que representa la selección
maxClique :: KClique -> IO (Maybe (Int,Model))
maxClique ((n,e),_) = maxCliqueAux ((n,e),n)

maxCliqueAux :: KClique -> IO (Maybe (Int,Model))
maxCliqueAux ((n,e),k) = case k of
  0 -> return Nothing
  _ -> do
    res <- solveKClique ((n,e),k)
    case res of
      Just m -> return (Just (k,m))
      Nothing -> maxCliqueAux ((n,e),k-1)

-- b) 
-- El clique mas grande es [4,5,6,7]. 
-- Salida: Just (4,[("p","false"),("v1","false"),("v2","false"),("v3","false"),("v4","true"),("v5","true"),("v6","true"),("v7","true"),("v8","false")])

-- c) 
-- El teorema tiene sentido porque, si hay un clique de tamaño k, cada vértice de ese clique tiene que estar conectado con los otros k−1 vértices. Entonces, cada uno de esos vértices tiene grado al menos k−1. Como ningún vértice puede tener un grado mayor que el grado máximo del grafo, el tamaño de un clique no puede ser mayor que ese grado máximo más 1.
-- Esto se puede aprovechar en maxClique porque, en lugar de empezar a buscar desde la cantidad total de vértices del grafo, podemos empezar desde el grado máximo más 1. Así evitamos probar tamaños de clique que sabemos que no pueden existir y hacemos menos llamadas a solveKClique.

----------------------------------------------------------------------------------
-- 3.4. Trabajando con archivos
----------------------------------------------------------------------------------

-- a)   Problema del k-clique como problema de búsqueda, pero leyendo grafo desde archivo.
-- Pre: recibe la ubicacion de un archivo en formato TSV y un tamaño de clique k.
-- Pos: en caso positivo retorna el modelo que representa la selección, 
--      de lo contrario retorna Nothing.
solvekCliqueFrom :: String -> Nat -> IO (Maybe Model)
solvekCliqueFrom path k = do
  g <- loadGraphTSV path
  solveKClique (g,k)

-- a)   Problema del k-clique como problema de optimización, pero leyendo grafo desde archivo.
-- Pre: recibe la ubicacion de un archivo en formato TSV.
-- Pos: retorna una pareja con el valor máximo posible y el modelo que representa la selección.
maxCliqueFrom :: String -> IO (Maybe (Int,Model))
maxCliqueFrom path = do
  g <- loadGraphTSV path
  maxClique (g, maxDegree g + 1)

----------------------------------------------------------------------------------
-- 3.5. Grafos network_yeast.tsv y network_butterfly.tsv
----------------------------------------------------------------------------------

-- ESCRIBIR RESPUESTA AQUÍ



