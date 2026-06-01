
module PropLogic where
------------------- Estudiante/s -------------------
-- Nombres y apellidos: 
-- Números: 
----------------------------------------------------

import Prelude
import Data.List
import Data.Maybe

import SMTlib

type Nat = Int

type Var = String

data L = V Var | Neg L | Bin L BC L
  deriving (Eq)

data BC = And | Or | Imp | Iff 
  deriving (Show, Eq)

-- Top y Bottom se definen en términos de las conectivas primitivas
top = Bin (V "p") Or  (Neg $ V "p") 
bot = Bin (V "p") And (Neg $ V "p") 

-- Conjuntoria (universal finito) de fórmulas indexadas
bigAnd :: [Int] -> (Int -> L) -> L
bigAnd is f = foldr (\i b -> Bin (f i) And b) top is

-- Disyuntoria (existencial finito) de fórmulas indexadas
bigOr :: [Int] -> (Int -> L) -> L
bigOr is f = foldr (\i b -> Bin (f i) Or b) bot is

-- Utilidad para construir variables proposicionales indexadas
v1 :: Var -> Nat -> L
v1 p i = V (p ++ (show i))

-- Utilidad para construir variables proposicionales doblemente indexadas
v2 :: Var -> Nat -> Nat -> L
v2 p i j = V (p ++ (show i) ++ "_" ++ (show j))

-- Utilidad para construir variables proposicionales triplemente indexadas
v3 :: Var -> Nat -> Nat -> Nat -> L
v3 p i j k = V (p ++ (show i) ++ "_" ++ (show j) ++ "_" ++ (show k))

-- Para escribir conectivas anidadas más facil 
neg :: L -> L
neg p = Neg p

(/\) :: L -> L -> L
p /\ q = Bin p And q 

(\/) :: L -> L -> L
p \/ q = Bin p Or q 

(==>) :: L -> L -> L
p ==> q = Bin p Imp q 

(<==>) :: L -> L -> L
p <==> q = Bin p Iff q 

infixr 8 /\
infixr 7 \/
infixr 6 ==>
infixr 5 <==>

or3 :: L -> L -> L -> L
or3 a b c = a \/ b \/ c

-- Traducción de LP a SMT-LIB

pVarDec = "(declare-fun p () Bool)\n"

-- Pre: recibe una fórmula de LP.
-- Pos: traduce la fórmula al formato SMT-LIB.
lp2SMT :: L -> String
lp2SMT (V p)         = p
lp2SMT (Neg a)       = "(not " ++ lp2SMT a ++ ")"
lp2SMT (Bin a And b) = "(and " ++ lp2SMT a ++ " " ++ lp2SMT b ++ ")"
lp2SMT (Bin a Or  b) = "(or "  ++ lp2SMT a ++ " " ++ lp2SMT b ++ ")"
lp2SMT (Bin a Imp b) = "(=> "  ++ lp2SMT a ++ " " ++ lp2SMT b ++ ")"
lp2SMT (Bin a Iff b) = "(= "   ++ lp2SMT a ++ " " ++ lp2SMT b ++ ")"     

-- Pretty printing de LP en consola

instance Show L where
  show (V p)         = p
  show (Neg a)       = "¬(" ++ show a ++ ")"
  show (Bin (V p) And (Neg (V p'))) | p == p' =  "_|_"
  show (Bin a And b) = "(" ++ show a ++ " /\\ " ++ show b ++ ")"
  show (Bin (V p) Or (Neg (V p'))) | p == p' = "T"
  show (Bin a Or  b) = "("  ++ show a ++ " \\/ " ++ show b ++ ")"
  show (Bin a Imp b) = "("  ++ show a ++ " ==> " ++ show b ++ ")"
  show (Bin a Iff b) = "("   ++ show a ++ " <==> " ++ show b ++ ")" 

size :: L -> Nat
size (V p)                            = 1
size (Neg a)                          = 1 + size a
size (Bin (V "p") Or  (Neg (V "p")))  = 1
size (Bin (V "p") And (Neg (V "p")))  = 1 
size (Bin a _ b)                      = 1 + size a + size b