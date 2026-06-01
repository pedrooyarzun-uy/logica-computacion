---------------------------------------------------------------------------------------------------
-- Módulo de comunicación Haskell-SMT-Solver
---------------------------------------------------------------------------------------------------

module SMTlib (Type, Logic, SymDecl, Formula, SATproblem, Model,
               decide, solve,
               symbolDecl, symbolDef,
               varDecl, constDef, mapVarDecl, mapConstDef, genVars1, genVars2
               ) where

import Control.Exception (try, SomeException)
import Data.List
import Prelude
import System.Exit (ExitCode(..))
import System.IO
import System.Process

import qualified Data.Text as T
import qualified Data.Text.IO as TI

---------------------------------------------------------------------------------------------------
-- Tipos básicos
---------------------------------------------------------------------------------------------------

type Type    = String
type Logic   = String  
type SymDecl = String 
type Formula = String

-- Un problema de SAT queda determinado por una teoría, una lista de declaraciones de simbolos,
-- y una lista de formulas a analizar.
type SATproblem = (Logic, [SymDecl], [Formula])

-- Un modelo es una lista de parejas (simbolo,valor)
type Model = [(String, String)]

---------------------------------------------------------------------------------------------------
-- Configuración de ambiente de ejecución
---------------------------------------------------------------------------------------------------

-- Ubicación donde se encuentra el SMT-solver a utilizar
-- En caso de haberla agregado al PATH, dejar lo siguiente sin cambiar.
-- De lo contrario, modificar lo siguiente con dicha ubicación.
solverPath = "c:\\tools\\cvc5\\bin\\cvc5.exe"  -- cvc5.exe

-- Ubicación donde se guarda el último script SMT-LIB creado
scriptPath = ".\\script.smt2"

---------------------------------------------------------------------------------------------------
-- Funciones para invocar la decisión o búsqueda de SAT
---------------------------------------------------------------------------------------------------

-- ACLARACIÓN: todas nuestras comunicaciones con el solver construyen fisicamente el script en el disco
-- duro. Esto es solamente útil desde un punto de vista pedagógico o para hacer debugging, pero tiene
-- un overhead significativo en la performance y en realidad no es necesario.

-- Solicitud para realizar decisión de sat
decide :: SATproblem -> IO Bool
decide problem = do
  ret <- try (openFile scriptPath WriteMode) :: IO (Either SomeException Handle)
  case ret of
    Left  err -> error $ show err
    Right hdl -> do TI.hPutStr hdl $ buildSMTScript problem False             
                    hClose hdl                                       
  (exitCode, stdout, stderr) <- readProcessWithExitCode solverPath [scriptPath] ""
  -- putStrLn $ "End invocation: " -- quick debug
  case exitCode of
    ExitFailure c -> error stdout 
    ExitSuccess   -> return $ case lines stdout of 
                                "sat":_   -> True
                                "unsat":_ -> False
                                otherwise -> error stdout -- No puede ocurrir en una teoría decidible 

-- Solicitud para realizar búsqueda de sat
-- IMPORTANTE: la lectura de modelos sirve (al menos) para valores booleanos y enteros 
solve :: SATproblem -> IO (Maybe Model)
solve problem = do
  ret <- try (openFile scriptPath WriteMode) :: IO (Either SomeException Handle)
  case ret of
    Left  err -> error $ show err
    Right hdl -> do TI.hPutStr hdl $ buildSMTScript problem True             
                    hClose hdl
                    -- return Nothing
                    -- putStrLn $ "End Writing script " -- quick debug   
  (exitCode, stdout, stderr) <- readProcessWithExitCode solverPath [scriptPath] "" 
  case exitCode of
    ExitFailure c -> error stdout 
    ExitSuccess   -> return $ case lines stdout of 
                                "unsat":_      -> Nothing
                                ("sat":"(":xs) -> Just $ map parseDecl (init xs) 
                                otherwise      -> error stdout  -- No puede ocurrir en una teoría decidible

-- Construe el script SMT-LIB para un SATproblem
buildSMTScript :: SATproblem -> Bool -> T.Text
buildSMTScript (th,vs,fs) modelOpt =
  T.concat
    ([ T.pack "(set-logic ", T.pack th, T.pack ")\n" ]
     ++ (if modelOpt then [T.pack "(set-option :produce-models true)\n"] else [])
     ++ [T.concat (map T.pack vs),
         T.concat (map (\f -> T.concat [T.pack "(assert ", T.pack f, T.pack ")\n"]) fs),
         T.pack "(check-sat)\n"]
     ++ (if modelOpt then [T.pack "(get-model)\n"] else [])
     ++ [T.pack "(exit)\n"])

-- Parsea definiciones de símbolos exactamente de la forma:
--     (define-fun NAME () TYPE VALUE)
--   o (define-fun NAME () TYPE (- VALUE))
-- Devuelve:  (NAME,VALUE) 
--          o (NAME,(- VALUE))
-- TODO: generalizar parsing para leer modelos con otros tipos de objetos
parseDecl :: SymDecl -> (String, String)
parseDecl d = (ws!!1, if ws!!4 == "(-" then ws!!4 ++ " " ++ init (ws!!5) 
                                       else init (ws!!4)) 
  where
    ws = words d

---------------------------------------------------------------------------------------------------
-- Declaraciones y definiciones de simbolos.
-- Estas son funciones de bajo nivel para declarar/definir cualquier simbolo. 
-- Nota: todos los simbolos en SMT-LIB son funciones, por lo tanto las constantes son 
-- funciones de aridad 0 y los predicados son funciones con tipo de salida booleano.
---------------------------------------------------------------------------------------------------

-- Pre: recibe un nombre, lista de parámetros tipados de entrada y tipo de salida.
--      Nota: los argumentos deben venir con la forma (x TYPE)
-- Pos: devuelve una declaración de símbolo de función en el formato SMT-LIB.
--      Esto es algo de la forma: 
--          (declare-fun NAME ((x_1 TYPE_1) (x_2 TYPE_2) ... (x_n TYPE_n)) TYPE)
symbolDecl :: String -> [String] -> String -> SymDecl
symbolDecl name ins out = "(declare-fun " ++ name ++ " (" ++ args ++ ") " ++ out ++ ")\n"
  where args = foldr (\a b -> a ++ " " ++ b) "" ins

-- Pre: recibe un nombre, lista de parámetros tipados de entrada, tipo de salida 
--      y cuerpo de definición.
--      Nota 1: los argumentos deben venir con la forma (x TYPE)
--      Nota 2: si el cuerpo no es un valor literal debe venir parentizado apropiadamente.
--              Por ejemplo, el valor "-2" debe venir con la forma "(- 2)"
-- Pos: devuelve una definición de símbolo de función en el formato SMT-LIB.
--      Esto es algo de la forma: 
--          (define-fun NAME ((x_1 TYPE_1) (x_2 TYPE_2) ... (x_n TYPE_n)) TYPE BODY)
symbolDef :: String -> [String] -> String -> String -> SymDecl
symbolDef name ins out body = "(define-fun " ++ name ++ " (" ++ args ++ ") " ++ out ++ " " ++ body ++ ")\n"
  where args = foldr (\a b -> a ++ " " ++ b) "" ins

---------------------------------------------------------------------------------------------------
-- Declaraciones y definiciones de constantes.
-- Funciones especificas para declarar/definir constantes. Como las declaraciones de constantes
-- juegan el rol de variables libres, les denominamos variables.
---------------------------------------------------------------------------------------------------

-- Pre: recibe un tipo y un nombre de variable.
--      Nota: formalmente, en el lenguaje de SMTlib, una "variable" es un
--            un símbolo de constante no interpretado. Esto significa que podrá
--            variar entre las interpretaciones como si fuera una "variable libre".
-- Pos: devuelve una declaración de variable en el formato SMT-LIB.
varDecl :: Type -> String -> SymDecl
varDecl t name = symbolDecl name [] t

-- Pre: recibe un tipo, nombre de constante y el valor asignado.
--      Nota: formalmente, en el lenguaje de SMTlib, una "constante" es un
--            un símbolo de constante interpretado. Esto significa que queda 
--            con valor fijo y no puede variar entre las interpretaciones.
-- Pos: devuelve una definición de constante en el formato SMT-LIB.
constDef :: Type -> String -> String -> SymDecl
constDef t name val = symbolDef name [] t val

-- Generalización de varDecl para listas de variables de algún tipo
mapVarDecl :: Type -> [String] -> [SymDecl]
mapVarDecl t l = map (varDecl t) l 

-- Generalización de constDecl para listas de constantes de algún tipo
mapConstDef :: Type -> [(String,String)] -> [SymDecl]
mapConstDef t l = map (\(n,v) -> constDef t n v) l 

-- Pre: recibe un tipo t, nombre de variable p y un entero positivo n.
-- Pos: genera una lista con todas las posibles declaraciones de variables con tipo t
--      y nombre p indexadas en el rango 1..n y en el formato SMT-LIB.
--      Por ejemplo, si n=4 tenemos 4 declaraciones de variables.  
genVars1 :: Type -> String -> [Int] -> [SymDecl]
genVars1 t p rn = mapVarDecl t vars     
  where 
    vars = [p ++ (show i) | i <- rn]

-- Pre: recibe un tipo t, nombre de variable p y dos enteros positivos n y m.
-- Pos: genera una lista con todas todas las posibles declaraciones de variables con tipo t
--      y nombre p doblemente indexadas en el rango 1..n y 1..m 
--      y en el formato SMT-LIB.
--      Por ejemplo, si n=4 y m=2 tenemos 4*2=8 declaraciones de variables.
genVars2 :: Type -> String -> [Int] -> [Int] -> [SymDecl]
genVars2 t p rn rm = mapVarDecl t vars  
  where 
    vars = [p ++ (show i) ++ "_" ++ (show j) | i <- rn, j <- rm]
