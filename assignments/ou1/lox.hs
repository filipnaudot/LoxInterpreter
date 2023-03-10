module Main where

import System.Environment
import System.IO
import qualified Data.Map as Map
import Data.Maybe

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
            let (_, output) = evalProgram program newEnvironment []
            putStrLn output
        _ -> putStrLn "Usage: lox <filename.lox>"


evalProgram :: Program -> Environment -> [Char] -> (Environment, [Char])
evalProgram (PROGRAM []) env output = (env, output)
evalProgram (PROGRAM (decl : decls)) env output =
  let (env', output') = evalDeclaration decl env output
  in evalProgram (PROGRAM decls) env' output'



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
-- EXPRESSION
evalStatement (ExprStmt expr) env output =
    let (env', exprVal) = evalExpr expr env output
    in (env', output)

-- IF
evalStatement (IfStmt expr ifStmt elseStmt) env output =
    let (env', exprVal) = evalExpr expr env output
    in if isTruthy exprVal then evalStatement ifStmt env' output else
      (if isNothing elseStmt then (env', output) else evalStatement (fromJust elseStmt) env' output)

-- PRINT
evalStatement (PrintStmt expr) env output = 
    let (env', exprVal) = evalExpr expr env output
    in (env', output ++ (show exprVal) ++ "\n")

-- WHILE
evalStatement whileStmt@(WhileStmt expr stmt ) env output =
  let (env', exprVal) = evalExpr expr env output
  in if isTruthy exprVal
    then let (env'', output') = evalStatement stmt env' output
         in evalStatement (WhileStmt expr stmt) env'' output'
    else (env', output)

-- BLOCK
evalStatement blockStmt@(BlockStmt decls) env output =
  let ((ENVIRONMENT _ (Just env')), output') = evalProgram (PROGRAM decls) (ENVIRONMENT Map.empty (Just env)) output
  in (env', output')
  

-- EXPRESSION
evalExpr :: Expr -> Environment -> [Char] -> (Environment, Value)
-- ASSIGNMENT
evalExpr (Assignment strId expr) env output =
    let (env', val) = evalExpr expr env output
    in (assignValue strId val env', val)

evalExpr (LogicalOr leftExpr rightExpr) env output =
  let (env', val) = evalExpr leftExpr env output
  in if isTruthy val then (env', val) else evalExpr rightExpr env' output

evalExpr (LogicalAnd leftExpr rightExpr) env output =
  let (env', val) = evalExpr leftExpr env output
  in if not (isTruthy val) then (env', val) else evalExpr rightExpr env' output

-- BINARY - EQUALITY
evalExpr equality@(Equality leftExpr op rightExpr) env output =
  let (env', leftVal) = evalExpr leftExpr env output
      (env'', rightVal) = evalExpr rightExpr env' output
  in case (leftVal, rightVal) of
    (IntValue l, IntValue r) ->
      case op of
        "==" -> (env'', BoolValue (l == r))
        "!=" -> (env'', BoolValue (l /= r))
    (BoolValue l, BoolValue r) ->
      case op of
        "==" -> (env'', BoolValue (l == r))
        "!=" -> (env'', BoolValue (l /= r))
    (StringValue l, StringValue r) ->
      case op of
        "==" -> (env'', BoolValue (l == r))
        "!=" -> (env'', BoolValue (l /= r))
    _ ->
      case op of
        "==" -> (env'', BoolValue False)
        "!=" -> (env'', BoolValue True)
  
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
    (StringValue l, StringValue r) ->
      case op of
        ">" -> (env'', BoolValue (l > r))
        ">=" -> (env'', BoolValue (l >= r))
        "<" -> (env'', BoolValue (l < r))
        "<=" -> (env'', BoolValue (l <= r))
    _ -> error "Error: Cannot apply comparison operation to values of different types \n"

-- BINARY - TERM
evalExpr term@(Term leftExpr op rightExpr) env output =
  let (env', leftVal) = evalExpr leftExpr env output
      (env'', rightVal) = evalExpr rightExpr env' output
  in case (leftVal, rightVal) of
       (IntValue l, IntValue r) ->
         case op of
           "+" -> (env'', IntValue (l + r))
           "-" -> (env'', IntValue (l - r))
       (StringValue l, StringValue r) ->
         case op of
           "+" -> (env'', StringValue (l ++ r))
       _ -> error "Error: Cannot apply term operation to values of different types \n"

-- BINARY - FACTOR
evalExpr factor@(Factor leftExpr op rightExpr) env output =
  let (env', leftVal) = evalExpr leftExpr env output
      (env'', rightVal) = evalExpr rightExpr env' output
  in case (leftVal, rightVal) of
       (IntValue l, IntValue r) ->
         case op of
           "*" -> (env'', IntValue (l * r))
           "/" -> (env'', IntValue (l / r))
       _ -> error "\nError: Can only apply factor operation to values numerical values \n"

-- GROUPING
evalExpr (Grouping expr) env output = evalExpr expr env output
-- PRIMARY
evalExpr (Primary (NUM num)) env output = (env, IntValue num)
evalExpr (Primary (STR str)) env output = (env, StringValue str)
evalExpr (Primary (NIL_LIT)) env output = (env, NilValue)
evalExpr (Primary (TRUE_LIT)) env output = (env, BoolValue True)
evalExpr (Primary (FALSE_LIT)) env output = (env, BoolValue False)
evalExpr (Primary (ID var)) env output = (env, lookupValue var env)



isTruthy :: Value -> Bool
isTruthy NilValue = False
isTruthy (BoolValue False) = False
isTruthy _ = True


---------------------------------------------------------------------
-------------------------- ENV. FUNCIONS ----------------------------
---------------------------------------------------------------------
newEnvironment :: Environment
newEnvironment = (ENVIRONMENT Map.empty Nothing)


insertValue :: [Char] -> Value -> Environment -> Environment
insertValue name val environment@(ENVIRONMENT values parentEnv) =
  case parentEnv of
    Nothing -> (ENVIRONMENT (Map.insert name val values) parentEnv)
    Just _ -> case Map.lookup name values of
      Nothing -> (ENVIRONMENT (Map.insert name val values) parentEnv)
      Just _ -> error ("\nError: Redeclaring variable '" ++ name ++ "'\n")


assignValue :: [Char] -> Value -> Environment -> Environment
assignValue name val environment@(ENVIRONMENT values parentEnv) =
  case Map.lookup name values of
    Just _ -> (ENVIRONMENT (Map.insert name val values) parentEnv)
    Nothing -> case parentEnv of
      Just env -> (ENVIRONMENT values (Just (assignValue name val env)))
      Nothing -> error ("\nError: Assignment to undefined variable '" ++ name ++ "'\n")


lookupValue :: [Char] -> Environment -> Value
lookupValue name (ENVIRONMENT values outer) =
  case Map.lookup name values of
    Just val -> val
    Nothing -> case outer of
      Just env -> lookupValue name env
      Nothing -> error ("\nError: Undefined variable '" ++ name ++ "'\n")