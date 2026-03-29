module Lab1 where
------------------- Estudiante/s -------------------
-- Nombres y apellidos: Pedro Oyarzun Fagundez
-- Números: 242531
----------------------------------------------------

import Prelude

-- EJERCICIO 1.1 --
type Var = String

data L = V Var | Neg L | Bin L BC L
  deriving (Show, Eq)
data BC = And | Or | Imp | Iff
  deriving (Show, Eq)
  
p :: L
p = V "p"

q :: L
q = V "q"

r :: L
r = V "r"


-- EJERCICIO 1.2 --
--a)
fa :: L
fa = Bin (V "p") And (Neg (Neg (V "q")))
--b)
fb :: L
fb = Bin (Bin (V "p") And (Neg (V "q"))) And (Neg (V "r"))
--c)
fc :: L
fc = Bin (Neg (Neg (V "p"))) Or (Neg (Bin (V "q") And (V "p")))
--d)
fd :: L
fd = Bin (Neg (Bin (V "r") Imp (V "r"))) And (Bin (Neg (Neg (V "p"))) Or (Neg (Bin (V "q") And (V "p"))))


-- EJERCICIO 1.3 --
--a)
cantBin :: L -> Int
cantBin (V x) = 0
cantBin (Neg l) = cantBin l
cantBin (Bin l o r) = 1 + cantBin l + cantBin r

--b)
valores :: L -> [(Var,Bool)]
valores = undefined

--c)
dobleNeg :: L -> L
dobleNeg (V x) = V x
dobleNeg (Neg (Neg l)) = dobleNeg l
dobleNeg (Neg l ) = Neg (dobleNeg l)
dobleNeg (Bin l o r) = Bin (dobleNeg l) o (dobleNeg r)

--d)
cambiar :: L -> L
cambiar (V x) = V x
cambiar (Neg l) = Neg (cambiar l)
cambiar (Bin l Or r) = Bin (Neg (cambiar l)) Imp (cambiar r)
cambiar (Bin l o r) = Bin (cambiar l) o (cambiar r)

--e)
cantPropX :: L -> Var -> Int
cantPropX = undefined

--f)
listarProp :: L -> [Var]
listarProp = undefined

--g)
sustCon :: L -> BC -> BC -> L
sustCon = undefined

--h)
swapCon :: L -> BC -> BC -> L
swapCon = undefined

--i)
invertir :: L -> L
invertir = undefined

--j)
sustSimp :: Var -> L -> L -> L
sustSimp = undefined

--k)
sustMult :: [(Var, L)] -> L -> L
sustMult = undefined