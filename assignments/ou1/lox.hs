module Main where

import System.Environment
import System.IO
import qualified Data.Map as Map

import Tokens
import LoxGrammar
import Scanner
import Parser


data Value
    = IntValue Float
    | BoolValue Bool
    | StringValue String
    | NilValue 

instance Show Value where
    show (IntValue num) = show num
    show (BoolValue True) = "true"
    show (BoolValue False) = "false"
    show (StringValue str) = str
    show (NilValue) = "nil"

data Environment = ENVIRONMENT (Map.Map String Value) (Maybe Environment)


main :: IO ()
main = do
    args <- getArgs
    case args of
        [filename] -> do
            contents <- readFile filename
            let tokens = scanTokens contents
            let program = parse tokens
            let output = evalProgram program newEnvironment []
            putStrLn output
        _ -> putStrLn "Usage: lox <filename.lox>"


evalProgram :: Program -> Environment -> [Char] -> [Char]
evalProgram (PROGRAM []) env output = output
evalProgram (PROGRAM (decl : decls)) env output =
  let (newEnv, newOutput) = evalDeclaration decl env output
  in evalProgram (PROGRAM decls) newEnv newOutput



evalDeclaration :: Declaration -> Environment -> [Char] -> (Environment, [Char])
-- VARIABEL DECL
evalDeclaration (VariableDecl (ID name) maybeExpr) env output =
  case maybeExpr of
    Nothing -> (insertValue name NilValue env, output)
    Just expr ->
      let (env', val) = evalExpr expr env output
      in (insertValue name val env', output)
-- STATEMENT
evalDeclaration (Statement stmt) env output = evalStatement stmt env output


evalStatement :: Stmt -> Environment -> [Char] -> (Environment, [Char])
-- PRINT
evalStatement (PrintStmt expr) env output = 
    let (env', exprVal) = evalExpr expr env output
    in (env', output ++ (show exprVal) ++ "\n")
-- EXPRESSION
evalStatement (ExprStmt expr) env output =
    let (env', exprVal) = evalExpr expr env output
    in (env', output)



evalExpr :: Expr -> Environment -> [Char] -> (Environment, Value)
-- ASSIGNMENT
evalExpr (Assignment strId expr) env output =
    let (env', val) = evalExpr expr env output
    in (insertValue strId val env', val)

-- BINARY - COMPARISON
evalExpr comparison@(Comparison leftExpr op rightExpr) env output =
  let (env', leftVal) = evalExpr leftExpr env output
      (env'', rightVal) = evalExpr rightExpr env' output
  in case (leftVal, rightVal) of
       (IntValue l, IntValue r) ->
         case op of
           ">" -> (env'', BoolValue (l > r))
           ">=" -> (env'', BoolValue (l >= r))
           "<" -> (env'', BoolValue (l < r))
           "<=" -> (env'', BoolValue (l <= r))
       _ -> error "Cannot apply arithmetic operation to non-numeric values"

-- BINARY - FACTOR
evalExpr factor@(Factor leftExpr op rightExpr) env output =
  let (env', leftVal) = evalExpr leftExpr env output
      (env'', rightVal) = evalExpr rightExpr env' output
  in case (leftVal, rightVal) of
       (IntValue l, IntValue r) ->
         case op of
           "*" -> (env'', IntValue (l * r))
           "/" -> (env'', IntValue (l / r))
       _ -> error "Cannot apply arithmetic operation to non-numeric values"

-- BINARY - TERM
evalExpr term@(Term leftExpr op rightExpr) env output =
  let (env', leftVal) = evalExpr leftExpr env output
      (env'', rightVal) = evalExpr rightExpr env' output
  in case (leftVal, rightVal) of
       (IntValue l, IntValue r) ->
         case op of
           "+" -> (env'', IntValue (l + r))
           "-" -> (env'', IntValue (l - r))
           "*" -> (env'', IntValue (l * r))
           "/" -> (env'', IntValue (l / r))
       (StringValue l, StringValue r) ->
         case op of
           "+" -> (env'', StringValue (l ++ r))
       _ -> error "Cannot apply arithmetic operation to non-numeric values"

-- PRIMARY
evalExpr (Primary (NUM num)) env output = (env, IntValue num)
evalExpr (Primary (STR str)) env output = (env, StringValue str)
evalExpr (Primary (NIL_LIT)) env output = (env, NilValue)
evalExpr (Primary (ID var)) env output = (env, lookupValue var env)






---------------------------------------------------------------------
-------------------------- ENV. FUNCIONS ----------------------------
---------------------------------------------------------------------
newEnvironment :: Environment
newEnvironment = (ENVIRONMENT Map.empty Nothing)

insertValue :: [Char] -> Value -> Environment -> Environment
insertValue name val (ENVIRONMENT values parentEnv) =
    ENVIRONMENT (Map.insert name val values) parentEnv 

lookupValue :: [Char] -> Environment -> Value
lookupValue name (ENVIRONMENT vars outer) =
    case Map.lookup name vars of
        Just val -> val
        Nothing -> case outer of
            Just env -> lookupValue name env
            Nothing -> error ("Undefined variable '" ++ name ++ "'")