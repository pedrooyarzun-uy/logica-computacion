---------------------------------------------------------------------------------------------------
-- Algunas utilidades para trabajar con grafos
---------------------------------------------------------------------------------------------------

module GraphUtils (Graph, loadGraphTSV, parseEdge, numberOfVertices, sim, degree, maxDegree
                  ) where

import Data.List (foldl')

type Nat = Int

type Graph = (Nat, [(Nat, Nat)])

-- Lee un grafo desde un archivo en formato TSV
loadGraphTSV :: FilePath -> IO Graph
loadGraphTSV path = do
  content <- readFile path
  let edges = map parseEdge . filter (not . null) . lines $ content
      n = numberOfVertices edges
  pure (n, edges)

parseEdge :: String -> (Int, Int)
parseEdge line =
  case words line of
    [a, b] -> (read a, read b)
    _      -> error ("Arista invalida: " ++ line)

numberOfVertices :: [(Int, Int)] -> Int
numberOfVertices [] = 0
numberOfVertices es = 1 + foldl' max 0 [max u v | (u, v) <- es]

-- Devuelve la relación simétrica de una relación
sim :: [(Nat,Nat)] -> [(Nat,Nat)] 
sim rel = [swap pair | pair <- rel] 
  where swap (i,j) = (j,i)

-- Devuelve el grado de un vértice en un grafo no dirigido
degree :: Graph -> Nat -> Nat
degree (_,e) v = length [ u | (u,w) <- es, u == v ]
  where
    es = e ++ sim e

-- Devuelve el máximo grado del grafo
maxDegree :: Graph -> Nat
maxDegree g@(n,_) =
  maximum [ degree g v | v <- [1..n] ]      