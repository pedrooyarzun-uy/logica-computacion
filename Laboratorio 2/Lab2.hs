module Lab2 where
------------------- Estudiante/s -------------------
-- Nombres y apellidos: Pedro Oyarzun y Nicolás Romero
-- Números: 242531 y 
----------------------------------------------------

import Prelude

-- Formalización del lenguaje
type Var = String

data L = V Var | Neg L | Bin L BC L
  deriving (Eq)
data BC = And | Or | Imp | Iff
  deriving (Eq)

-- Fórmulas del Lab1
p = V "p"
q = V "q"
r = V "r"
fa :: L
fa = Bin p And (Neg (Neg q))                   -- (p ∧ ¬¬q)
fb :: L
fb = Bin p And (Bin (Neg q) And (Neg r))       -- (p ∧ ¬q ∧ ¬r)
fc :: L
fc = Bin (Neg (Neg p)) Or (Neg (Bin q And p))  -- (¬¬p ∨ ¬(q ∧ p))
fd :: L
fd = Bin (Neg (Bin r Imp r)) And fc            -- ¬(r ⊃ r) ∧ (¬¬p ∨ ¬(q ∧ p))


-- EJERCICIO 1 --
--1.1)
eval :: (Var -> Bool) -> L -> Bool
eval i (V x) = i x
eval i (Neg l) = not (eval i l)
eval i (Bin l o r) = case o of
  And -> (eval i l) && (eval i r)
  Or -> (eval i l) || (eval i r)
  Imp -> not (eval i l) || (eval i r)
  Iff -> ((eval i l) && (eval i r)) || ((not (eval i l)) && (not (eval i r)))

--1.2)
itodasverdaderas ::  Var -> Bool
itodasverdaderas = undefined

--1.3)
itodasfalsas :: Var -> Bool
itodasfalsas = undefined

--1.4)
irfalsa :: Var -> Bool
irfalsa = undefined 

--1.5)
-- Completar con verdadera/falsa:
-- fa es ... bajo itodasfalsas
-- fb es ... bajo itodasfalsas
-- fc es ... bajo itodasfalsas
-- fd es ... bajo itodasfalsas
-- 
-- fa es ... bajo itodasverdaderas
-- fb es ... bajo itodasverdaderas
-- fc es ... bajo itodasverdaderas
-- fd es ... bajo itodasverdaderas
--
-- fa es ... bajo irfalsa
-- fb es ... bajo irfalsa
-- fc es ... bajo irfalsa
-- fd es ... bajo irfalsa

--1.6)
creari :: [(Var, Bool)] -> (Var -> Bool)
creari = undefined

--1.7)
-- Responder aquí.


-- EJERCICIO 2 --
type Fila = [(Var, Bool)]
type TV = [(Fila, Bool)]

data Clase = Tau | Contra | Cont | Sat | Fal

--2.1)
filas :: [Var] -> [Fila]
filas = undefined

--2.2)
tv :: L -> TV
tv = undefined

--2.3)
es :: L -> Clase -> Bool
es = undefined

--2.4)
-- Completar con tautología/contingencia/contradicción:
-- fa es ...
-- fb es ...
-- fc es ...
-- fd es ...

--2.5) 
fnc :: L -> L
fnc = undefined


----------------------------------------------------------------------------------
-- Pretty Printing (rudimentario)
----------------------------------------------------------------------------------
instance Show L where
  show (V p)         = p
  show (Neg (Neg a)) = "¬" ++ show (Neg a)
  show (Neg (V p))   = "¬ " ++ show (V p)
  show (Neg a)       = "¬ (" ++ show a ++ ")"
  show (Bin a And b) = "(" ++ show a ++ ") /\\ (" ++ show b ++ ")"
  show (Bin a Or b)  = "(" ++ show a ++ ") \\/ (" ++ show b ++ ")"
  show (Bin a Imp b) = "(" ++ show a ++ ") --> (" ++ show b ++ ")"
  show (Bin a Iff b) = "(" ++ show a ++ ") <-> (" ++ show b ++ ")"