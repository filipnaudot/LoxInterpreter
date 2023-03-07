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

instance Show Value where
    show (IntValue num) = show num

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
--evalDeclaration (VariableDecl name maybeExpr) env output =
--  case maybeExpr of
--    Nothing -> (insertValue name "" env, output)
--    Just expr ->
--      let val = evalExpr expr env output
--      in (insertValue name val env, output)
evalDeclaration (Statement stmt) env output = evalStatement stmt env output


evalStatement :: Stmt -> Environment -> [Char] -> (Environment, [Char])
evalStatement (ExprStmt expr) env output = 
    let exprVal = evalExpr expr env output
    in (env, output ++ (show exprVal))


evalExpr :: Expr -> Environment -> [Char] -> Value
evalExpr (Term leftExpr op rightExpr) env output =
  let leftVal = evalExpr leftExpr env output
      rightVal = evalExpr rightExpr env output
  in case (leftVal, rightVal) of
       (IntValue l, IntValue r) ->
         case op of
           "+" -> IntValue (l + r)
           "-" -> IntValue (l - r)
           _   -> error ("Unsupported operator: " ++ op)
       (StringValue l, StringValue r) ->
         case op of
           "+" -> StringValue (l ++ r)
           _   -> error ("Unsupported operator: " ++ op)
       _ -> error "Cannot apply arithmetic operation to non-numeric values"
evalExpr (Primary (NUM num)) env output = IntValue num
evalExpr (Primary (STR str)) env output = StringValue str












newEnvironment :: Environment
newEnvironment = (ENVIRONMENT Map.empty Nothing)