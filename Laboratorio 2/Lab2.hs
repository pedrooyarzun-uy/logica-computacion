module Lab2 where
------------------- Estudiante/s -------------------
-- Nombres y apellidos: Pedro Oyarzun y Matias Romero
-- Números: 242531 y 280487
----------------------------------------------------

import Prelude
import Data.List

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
itodasverdaderas _ = True

--1.3)
itodasfalsas :: Var -> Bool
itodasfalsas _ = False

--1.4)
irfalsa :: Var -> Bool
irfalsa x = not (x == "r")

--1.5)
-- Completar con verdadera/falsa:
-- fa es Falsa bajo itodasfalsas
-- fb es Falsa bajo itodasfalsas
-- fc es Verdadera bajo itodasfalsas
-- fd es Falsa bajo itodasfalsas
-- 
-- fa es Verdadera bajo itodasverdaderas
-- fb es Falsa bajo itodasverdaderas
-- fc es Verdadera bajo itodasverdaderas
-- fd es Falsa bajo itodasverdaderas
--
-- fa es Verdadera bajo irfalsa
-- fb es Falsa bajo irfalsa
-- fc es Verdadera bajo irfalsa
-- fd es Falsa bajo irfalsa

--1.6)
creari :: [(Var, Bool)] -> (Var -> Bool)
creari lista v = case lista of
    [] -> False
    (x,b):xs -> case x == v of
        True  -> b
        False -> creari xs v

--1.7)
-- Responder aquí.
{-
Si, es la misma interpretacion, al hacer las asignaciones,
en ambas interpretaciones el resultado es False, al pasarle r como parametro y True al pasarle p y q
-}
-- EJERCICIO 2 --
type Fila = [(Var, Bool)]
type TV = [(Fila, Bool)]

data Clase = Tau | Contra | Cont | Sat | Fal

--2.1)
filas :: [Var] -> [Fila]
filas [] = [[]]
filas (x:xs) = [[(x, b)] ++ f | b <- [True, False], f <- filas xs]

--2.2)
tv :: L -> TV
tv l = [(fila, eval (creari fila) l) | fila <- filas (listarProp l)]

-- Importamos listarProp de Lab1
listarProp :: L -> [Var]
listarProp (V x) = [x]
listarProp (Neg l) = listarProp l
listarProp (Bin l _ r) = nub (listarProp l ++ listarProp r)

--2.3)
-- [(Fila, Bool)]
es :: L -> Clase -> Bool
es l c = case c of 
  Tau -> esTautologia (tv l)
  Contra -> esContradiccion (tv l)
  Cont -> esContingencia (tv l)
  Sat -> esSatisfacible (tv l)
  Fal -> esFalsificable (tv l)

esTautologia :: TV -> Bool
esTautologia [] = True
esTautologia ((_, b):xs) = b && esTautologia xs

esContradiccion :: TV -> Bool
esContradiccion [] = True
esContradiccion ((_, b):xs) = not(b) && esContradiccion xs

esContingencia :: TV -> Bool
esContingencia t = esSatisfacible t && esFalsificable t

esSatisfacible :: TV -> Bool
esSatisfacible [] = False
esSatisfacible ((_, b):xs) = b || esSatisfacible xs

esFalsificable :: TV -> Bool
esFalsificable [] = False
esFalsificable ((_, b):xs) = not b || esFalsificable xs

--2.4)
-- Completar con tautología/contingencia/contradicción:
-- fa es Contingencia
-- fb es Contingencia
-- fc es Tautologia
-- fd es Falseable

--2.5) 
fnc :: L -> L
fnc f = case [clausula fila | (fila, False) <- tv f] of
  []     -> Bin (V "p") Or (Neg (V "p"))
  (c:cs) -> combinarAnd c cs


clausula :: Fila -> L
clausula f = case f of
  [] -> V "True"
  (v,b):xs ->
    case b of
      True  -> combinarOr (Neg (V v)) xs
      False -> combinarOr (V v) xs


combinarOr :: L -> [(Var,Bool)] -> L
combinarOr l lista = case lista of
  [] -> l
  (v,b):xs ->
    case b of
      True  -> combinarOr (Bin l Or (Neg (V v))) xs
      False -> combinarOr (Bin l Or (V v)) xs


combinarAnd :: L -> [L] -> L
combinarAnd l lista = case lista of
  [] -> l
  (x:xs) -> combinarAnd (Bin l And x) xs
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
