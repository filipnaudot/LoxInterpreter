module LoxGrammar where

import Tokens




---------------------------------------------------------
---------------- Lox grammar/data types -----------------
---------------------------------------------------------

-- Datatype for representing a Lox program
data Program = PROGRAM [Declaration]

instance Show Program where
  show (PROGRAM decls) = show (length decls) ++ "\n" ++ unlines (map show decls)



-- Datatype for representing a Lox declaration
data Declaration = VariableDecl Literal (Maybe Expr) | Statement Stmt

instance Show Declaration where
  show (VariableDecl id (Just expr)) = "V DEC -> " ++ getLiteral id ++ "=" ++ show expr ++ ";"
  show (VariableDecl id Nothing) = "V DEC -> " ++ getLiteral id ++ ";"
  show (Statement stmt) = show stmt



-- Datatype for representing a Lox statement
data Stmt 
  = ExprStmt Expr
  | IfStmt Expr Stmt (Maybe Stmt)
  | PrintStmt Expr
  | ReturnStmt (Maybe Expr)
  | WhileStmt Expr Stmt
  | BlockStmt [Declaration]

instance Show Stmt where
  show (ExprStmt expr) = show expr ++ ";"
  show (IfStmt condition stmt (Just elseStmt)) = "if(" ++ show condition ++ ")" ++ show stmt ++ "else" ++ show elseStmt
  show (IfStmt condition stmt Nothing) = "if(" ++ show condition ++ ")" ++ show stmt
  show (PrintStmt expr) = "print" ++ show expr ++ ";"
  show (ReturnStmt (Just expr)) = "return" ++ show expr ++ ";"
  show (ReturnStmt Nothing) = "return;"
  show (WhileStmt condition stmt) = "while(" ++ show condition ++ ")" ++ show stmt
  show (BlockStmt decls) = "{" ++ unwords (map show decls) ++ "}"



-- Datatype for representing a Lox expression
data Expr
  = Assignment String Expr
  | LogicalOr Expr Expr
  | LogicalAnd Expr Expr
  | Equality Expr String Expr
  | Comparison Expr String Expr
  | Term Expr String Expr
  | Factor Expr String Expr
  | Unary String Expr
  | Primary Literal
  | Grouping Expr

instance Show Expr where
    show (Assignment id expr) = id ++ "=" ++ show expr
    show (LogicalOr expr1 expr2) = "(" ++ show expr1 ++ "||" ++ show expr2 ++ ")"
    show (LogicalAnd expr1 expr2) = "(" ++ show expr1 ++ "&&" ++ show expr2  ++ ")"
    show (Equality expr1 operator expr2) = "(" ++ show expr1 ++ operator ++ show expr2 ++ ")"
    show (Comparison expr1 operator expr2) = show expr1 ++ operator ++ show expr2
    show (Term expr1 operator expr2) = "(" ++ show expr1 ++ operator ++ show expr2 ++ ")"
    show (Factor expr1 operator expr2) = "(" ++ show expr1 ++ operator ++ show expr2 ++ ")"
    show (Unary operator expr) = "(" ++ operator ++ show expr ++ ")"
    show (Primary lit) = getLiteral lit
    show (Grouping expr) = "(" ++ show expr ++ ")"



---------------------------------------------------------
------------------- Helper functions --------------------
---------------------------------------------------------

-- Format/retrieve the actual value of a literal for show function.
getLiteral (STR str) = "\"" ++ str ++ "\""
getLiteral (ID id) = id
getLiteral (NUM num) = show num
getLiteral literal = show literal -- Used for FALSE_LIT, TRUE_LIT and NIL_LIT