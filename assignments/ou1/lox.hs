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
evalDeclaration (VariableDecl (ID name) maybeExpr) env output =
  case maybeExpr of
    Nothing -> (insertValue name NilValue env, output)
    Just expr ->
      let val = evalExpr expr env output
      in (insertValue name val env, output)
evalDeclaration (Statement stmt) env output = evalStatement stmt env output


evalStatement :: Stmt -> Environment -> [Char] -> (Environment, [Char])
evalStatement (PrintStmt expr) env output = 
    let exprVal = evalExpr expr env output
    in (env, output ++ (show exprVal) ++ "\n")
evalStatement (ExprStmt expr) env output =
    case expr of
        (Factor _ _ _) -> 
            let exprVal = evalExpr expr env output
            in (env, output)
        (Term _ _ _) ->
            let exprVal = evalExpr expr env output
            in (env, output)
        (Assignment _ _) ->
            let env' = evalAssignment expr env output
            in (env', output)


evalExpr :: Expr -> Environment -> [Char] -> (Value)
evalExpr (Factor leftExpr op rightExpr) env output =
  let leftVal = evalExpr leftExpr env output
      rightVal = evalExpr rightExpr env output
  in case (leftVal, rightVal) of
       (IntValue l, IntValue r) ->
         case op of
           "*" -> IntValue (l * r)
           "/" -> IntValue (l / r)
       _ -> error "Cannot apply arithmetic operation to non-numeric values"
evalExpr (Term leftExpr op rightExpr) env output =
  let leftVal = evalExpr leftExpr env output
      rightVal = evalExpr rightExpr env output
  in case (leftVal, rightVal) of
       (IntValue l, IntValue r) ->
         case op of
           "+" -> IntValue (l + r)
           "-" -> IntValue (l - r)
           "*" -> IntValue (l * r)
           "/" -> IntValue (l / r)
       (StringValue l, StringValue r) ->
         case op of
           "+" -> StringValue (l ++ r)
       _ -> error "Cannot apply arithmetic operation to non-numeric values"
evalExpr (Primary (NUM num)) _ output = IntValue num
evalExpr (Primary (STR str)) _ output = StringValue str
evalExpr (Primary (NIL_LIT)) _ output = NilValue
evalExpr (Primary (ID var)) env output = lookupValue var env


evalAssignment :: Expr -> Environment -> [Char] -> Environment
evalAssignment (Assignment strId expr) env output =
    let val = evalExpr expr env output
    in insertValue strId val env






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