
module Lab4Ej1 where
------------------- Estudiante/s -------------------
-- Nombres y apellidos: 
-- Números: 
----------------------------------------------------

import Prelude
import Data.List
import Data.Maybe

import SMTlib
import PropLogic

----------------------------------------------------------------------------------
-- 1. Veraces y mentirosos
----------------------------------------------------------------------------------

-- Declaración de letras proposicionales e interpretación intuitiva:
--    ... COMPLETAR AQUÍ ...
vocEj1 :: [SymDecl]
vocEj1 = [ varDecl "Bool" "a"
         , varDecl "Bool" "b"
         , varDecl "Bool" "c"
         ]     

-- Formalización del caso 1.
-- Respuesta: Just [("a","false"),("b","true"),("c","false")]
caso1 :: [L]
caso1 = [fraseACaso1, fraseBCaso1]

fraseACaso1 :: L
fraseACaso1 = (V "a") <==> (neg (V "a") /\ (neg (V "b") /\ neg (V "c")))

fraseBCaso1 :: L
fraseBCaso1 = (V "b") <==> ((V "a" /\ neg (V "b") /\ neg (V "c")) \/ (V "b" /\ neg (V "a" /\ neg (V "c"))) \/ (V "c" /\ neg (V "a" /\ neg (V "b"))))

-- Formalización del caso 2.
-- Respuesta: Just [("a","true"),("b","false"),("c","true")]
caso2 :: [L]
caso2 = [fraseACaso2, fraseBCaso2, fraseCCaso2, fraseDCaso2]

fraseACaso2 :: L
fraseACaso2 = (V "a") <==> (V "b" /\ neg (V "c")) \/ (V "c" /\ neg (V "b"))

fraseBCaso2 :: L
fraseBCaso2 = (V "b") <==> (neg (V "a") /\ neg (V "b"))

fraseCCaso2 :: L
fraseCCaso2 = (V "b") <==> neg (V "c")

fraseDCaso2 :: L
fraseDCaso2 = (V "c") <==> (V "a")